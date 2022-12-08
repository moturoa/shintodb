#' Generic Postgres Database layer R6 class
#' @description Generic methods to work with postgres, avoiding any SQL in the next
#' architecture layer.
#' @importFrom R6 R6Class
#' @importFrom pool dbPool poolClose
#' @importFrom dbplyr in_schema
#' @importFrom dplyr tbl left_join collect
#' @export
# TODO
# - test sqlite (or no schema)
# - better logging ... cli?
# - optional printing (turn on/off on initialize)
# - optional timing (turn on/off)
databaseClass <- R6::R6Class(lock_objects = FALSE,

  public = list(

    #' @field con Connection object
    con = NULL,

    #' @field schema DB Schema
    schema = NULL,

    #' @field dbname DB name
    dbname = NULL,

    #' @field dbuser DB user
    dbuser = NULL,

    #' @field pool Logical; whether we are connected with the `pool` package or not.
    pool = NULL,

    #' @field dbtype DB type; either 'postgres' or 'sqlite'
    dbtype = NULL,

    #' @description Make a new R6 object of class `database`
    #' @param config_file Path to the standard DB connection config file
    #' @param what Connect to which database entry in the config file
    #' @param schema Name of schema where the data resides
    #' @param pool Logical; use [dbPool()] or not.
    #' @param connect_on_init Whether to immediately make a DB connection when
    #' making this object
    #' @param log_level Either 'all' (all logging) or 'none'.
    initialize = function(config_file = "conf/config.yml",
                          what,
                          schema = NULL,
                          pool = TRUE,
                          connect_on_init = TRUE,
                          log_level = c("all","none")
                          ){

      if(connect_on_init){

        if(is.null(schema)){
          schema <- "public"
        }

        self$connect_to_database(config_file, schema, what, pool)
      }

      self$log_level <- match.arg(log_level)


    },

    #' @description Make database connection
    #' @param config_file Path to the standard DB connection config file
    #' @param what Connect to which database entry in the config file
    #' @param schema Name of schema where the data resides
    #' @param pool Logical; use [dbPool()] or not.
    #' @param sqlite Name of `sqlite` DB file; if used.
    connect_to_database = function(config_file = NULL,
                                   schema = NULL,
                                   what = NULL,
                                   pool = TRUE,
                                   sqlite = NULL){


      if(!is.null(sqlite)){

        if(!file.exists(sqlite)){
          stop("SQlite not found, check path")
        }

        self$schema <- NULL
        self$dbname <- sqlite
        self$pool <- pool

        if(pool){
          self$con <- dbPool(RSQLite::SQLite(), dbname = sqlite)
        } else {
          self$con <- dbConnect(RSQLite::SQLite(), dbname = sqlite)
        }

        self$dbtype <- "sqlite"

      } else {

        self$schema <- schema
        self$dbname <- what
        self$pool <- pool

        cf <- config::get(what, file = config_file)
        self$dbuser <- cf$dbuser

        response <- try(
          shintodb::connect(what, config_file, pool)
        )

        if(!inherits(response, "try-error")){
          self$con <- response
        }

        self$dbtype <- "postgres"

      }


    },

    #' @description Close DB connection.
    close = function(){

      if(!is.null(self$con) && dbIsValid(self$con)){

        if(self$pool){
          private$log("poolClose")
          pool::poolClose(self$con)
        } else {
          private$log("dbDisconnect")
          DBI::dbDisconnect(self$con)
        }

      } else {
         private$log("Not closing an invalid or null connection")
      }
    },

    #' @description List tables
    list_tables = function(){

      if(self$dbtype == "postgres"){
        query <- glue::glue("SELECT table_name FROM information_schema.tables WHERE table_schema='{self$schema}'")

        self$query(query)

      } else {
        DBI::dbListTables(self$con)  # no schema in sqlite
      }

    },

    #' @description Does `table` have `column`?
    #' @param table Table name
    #' @param column Column name
    #' @return Logical
    have_column = function(table, column){

      query <- glue::glue(
        "SELECT EXISTS (SELECT 1
                     FROM information_schema.columns
                     WHERE table_schema='{self$schema}' AND table_name='{table}' AND column_name='{column}');"
      )

      out <- self$query(query)
      out$exists
    },


    #' @description Add a column to a table
    #' @param table Table name
    #' @param column Column name
    #' @param type Column type - watch out, not checked against valid types!
    make_column = function(table, column, type = "varchar"){

      qu <- glue::glue("alter table {self$schema}.{table} add column {column} {type}")
      self$execute_query(qu)

    },


    #' @description Read a table from the DB, maybe "lazy"
    #' @details If `lazy=TRUE`, returns a lazy connection that can be filtered, selected etc.
    #' using `dbplyr`'s `filter`, `select`, etc. Use if you want the work to be done by the DB,
    #' before downloading the entire table.
    #' @param table Name of table (in the global schema)
    #' @param lazy Logical, if FALSE (default), simply downloads the whole table.
    read_table = function(table, lazy = FALSE){

      if(!is.null(self$schema)){
        out <- dplyr::tbl(self$con,  dbplyr::in_schema(self$schema, table))
      } else {
        out <- dplyr::tbl(self$con, table)
      }

      if(!lazy){
        out <- dplyr::collect(out)

        private$write_output_log(what = glue::glue("Read table: {table}"),
                                 data = out)
      }

      out

    },

    #' @description Append rows to an existing table
    #' @param table Table name
    #' @param data Dataframe with columns that exist in `table`
    #' @details Wrapped in a `try`, so will not stop the application if this operation fails.
    #' It will fail when `data` has columns that do not exist in `table`, or are of
    #' incompatible type.
    append_data = function(table, data){

      if(!is.null(self$schema)){
        tm <- try(
          dbWriteTable(self$con,
                       name = Id(schema = self$schema, table = table),
                       value = data,
                       append = TRUE)
        )
      } else {
        tm <- try(
          dbWriteTable(self$con,
                       name = table,
                       value = data,
                       append = TRUE)
        )

      }

      private$log(glue::glue("Append {nrow(data)} rows to '{table}'"))

      return(invisible(!inherits(tm, "try-error")))

    },

    #' @description Run an SQL statement with [dbGetQuery()]
    #' @param txt SQL query
    #' @param glue If TRUE, can `glue` the SQL query
    #' @details Query wrapped in `try`, for safety
    query = function(txt, glue = FALSE){

      if(glue)txt <- glue::glue(txt)

      out <- try(
        DBI::dbGetQuery(self$con, txt)
      )

      private$write_output_log(query = txt, data = out)

    },

    #' @description Run an SQL statement with [dbGetQuery()]
    #' @param ... Further arguments to `self$query`
    #' @details Query wrapped in `try`, for safety
    get_query = function(...){
      self$query(...)
    },


    #' @description Run an SQL statement with [dbExecute()]
    #' @param txt SQL query ("alter table" type commands)
    #' @param glue If TRUE, `glue`s the query
    execute_query = function(txt, glue = FALSE){

      if(glue)txt <- glue::glue(txt)

      private$write_output_log(query = txt)

      try(
        DBI::dbExecute(self$con, txt)
      )

    },

    #' @description Does `column` in `table` have this `value`?
    #' @param table Table name
    #' @param column Column name
    #' @param value Value
    has_value = function(table, column, value){

      query <- glue::glue("select {column} from {self$schema}.{table} where {column} = '{value}' limit 1")

      out <- self$query(query)

      nrow(out) > 0
    },


    #' @description Update a value in a table given a single logical condition
    #' @param table Table name
    #' @param col_replace Column name where to update values
    #' @param val_replace Value to enter in updated cells
    #' @param col_compare Column used to select rows for updating
    #' @param val_compare Value in `col_compare` used to select rows for updating
    #' @param query_only Logical; if TRUE, returns the query. If FALSE, also runs the query.
    #' @param quiet If TRUE, no logging
    #' @examples
    #' \dontrun{
    #' # set verwijderd = 1 where naam = 'gekozennaam'
    #' replace_value_where("table", 'verwijderd', 'true', 'naam', 'gekozennaam')
    #' }
    replace_value_where = function(table,
                                   col_replace, val_replace, col_compare, val_compare,
                                   query_only = FALSE, quiet = FALSE){

      # postgres accepts 'true' but not 1 (which sqlInterpolate makes it into)
      if(is.logical(val_replace)){
        val_replace <- tolower(as.character(val_replace))
      }

      query <- glue::glue("update {self$schema}.{table} set {col_replace} = ?val_replace where",
                          " {col_compare} = ?val_compare") %>% as.character()

      query <- sqlInterpolate(DBI::ANSI(),
                              query,
                              val_replace = val_replace, val_compare = val_compare)

      if(query_only)return(query)

      self$execute_query(query)


    },


    #' @description Synonym for `$replace_value_where`. See its help page.
    #' @param ... Further arguments passed to `replace_value_where`
    update_where = function(...){
      self$replace_value_where(...)
    },


    #' @description Update a value in a table given a two logical AND conditions
    #' @param table Table name
    #' @param col_replace Column name where to update value(s)
    #' @param val_replace Value to enter in updated cells
    #' @param col_compare1 Column used to select rows for updating
    #' @param val_compare1 Value in `col_compare` used to select rows for updating
    #' @param col_compare2 Second column used to select rows for updating
    #' @param val_compare2 Second value in `col_compare` used to select rows for updating
    replace_value_where2 = function(table, col_replace, val_replace,
                                    col_compare1, val_compare1,
                                    col_compare2, val_compare2){

      if(!is.null(self$schema)){
        query <- glue::glue("update {self$schema}.{table} set {col_replace} = ?val_replace where ",
                      "{col_compare1} = ?val_compare1 AND ",
                      "{col_compare2} = ?val_compare2") %>% as.character()
      } else {
        query <- glue::glue("update {table} set {col_replace} = ?val_replace where ",
                      "{col_compare1} = ?val_compare1 AND ",
                      "{col_compare2} = ?val_compare2") %>% as.character()
      }


      query <- DBI::sqlInterpolate(DBI::ANSI(),
                              query,
                              val_replace = val_replace,
                              val_compare1 = val_compare1,
                              val_compare2 = val_compare2)

      self$execute_query(query)


    },


    #' @description Get detailed table info (postgres)
    #' @param table Table name
    #' @details Queries `information_schema.columns`
    table_info = function(table){

      if(self$dbtype == "sqlite"){
        message("Not supported for SQLite")
        return(NA)
      }

      query <- glue::glue("select * from information_schema.columns ",
                    "where table_schema = '{self$schema}' and ",
                    "table_name = '{table}'")

      self$query(query)

    },

    #' @description Get columns of a table
    #' @param table Table name
    #' @param empty_table If TRUE, returns a table with 0 rows, otherwise
    #' a vector of table column names,
    table_columns = function(table, empty_table = FALSE){

      query <- glue::glue("select * from {self$schema}.{table} where false")

      out <- self$query(query)

      if(empty_table){
        return(out)
      } else {
        return(names(out))
      }

    },

    #' @description Delete rows from a table given a single logical condition
    #' @param table Table name
    #' @param col_compare Column where to make logical comparison
    #' @param val_compare Values in `col_compare` where rows will be deleted
    #' @details Do not use inside shiny apps or otherwise, only as a tool to clean up tables.
    delete_rows_where = function(table, col_compare, val_compare){

      query <- glue("delete from {self$schema}.{table} where {col_compare}= ?val")
      query <- DBI::sqlInterpolate(DBI::ANSI(), query, val = val_compare)

      self$execute_query(query)

    }
  ),

  private = list(

    #' @description Make a SQL representation of a vector
    #' @param x Vector
    to_sql_string = function(x){

        paste0(
          "('",
          paste(x, collapse="','"),
          "')"
        )

    },

    log = function(...){

      if(self$log_level == "all"){
        futile.logger::flog.info(..., name = "DBR6")
      }

    },

    write_output_log = function(what = NULL, query, data = NULL){

      if(!is.null(what)){
        private$log(what)
      }

      if(!is.null(query)){
        private$log(query)
      }

      if(!is.null(data)){
        private$log(glue::glue("Downloaded table of size : {nrow(data)} rows, {ncol(data)} columns"))
      }

    }

  )


)


