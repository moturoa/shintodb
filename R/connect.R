
#' Make a Shinto Database Connection
#' @description Make a connection with RPostgres to one of the Shinto Labs Postgres databases.
#' @param what Which database connection in the config file? (Name of the block in the YAML)
#' @param file Path to the config.yml file, or set global option `options(shintodb_config_file = /path/to/file)`
#' @param pool Logical. When TRUE, uses [pool::dbPool()], otherwise [DBI::dbConnect()]
#' @param config_entry Corresponds to `config` argument to [config::get()], no need to set usually
#' as it is guessed from the environment variable R_CONFIG_ACTIVE
#' @export
#' @importFrom config get
#' @importFrom DBI dbConnect
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' @importFrom glue glue
#' @importFrom futile.logger flog.info flog.warn
connect <- function(what,
                    file = getOption("shintodb_config_file", "conf/config.yml"),
                    pool = FALSE,
                    config_entry = NULL){

  if(is.null(config_entry)){
    config_entry <- Sys.getenv("R_CONFIG_ACTIVE", "default")
  }

  if(config_entry == "productionlocal"){
    localized <- TRUE
    config_entry <- "production"
  } else {
    localized <- FALSE
  }

  conf <- config::get(value = what,
                      config = config_entry,
                      file = file)

  if(localized){
    conf$dbport <- get_user_local_port("p2")
    conf$dbhost <- "localhost"
  }

  if(is.null(conf)){
    stop(glue::glue("Connection entry '{what}' not found in file '{file}' (section '{config_entry}')"))
  }

  futile.logger::flog.info(glue::glue("Connecting to {conf$dbname} on {conf$dbhost} with user {conf$dbuser}"))

  # If password is encrypted, decrypt it before connecting
  if(string_is_encrypted(conf$dbpassword)){
    conf$dbpassword <- decrypt(conf$dbpassword)
  } else {
    futile.logger::flog.warn(glue::glue("Password is not encrypted - run shintodb::encrypt_config_file()"))
  }



  if(!pool){
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = conf$dbname,
                   host = conf$dbhost,
                   port = conf$dbport,
                   user = conf$dbuser,
                   password = conf$dbpassword)
  } else {
    pool::dbPool(RPostgres::Postgres(),
                 dbname = conf$dbname,
                 host = conf$dbhost,
                 port = conf$dbport,
                 user = conf$dbuser,
                 password = conf$dbpassword)
  }

}


