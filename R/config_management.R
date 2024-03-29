#' Adds an entry to a DB config.yml file
#' @description Manage your DB config file with ease. Adds one or more config blocks,
#' in default, development, or production (or default+development at the same time since
#' they are both for the dev3 postgres server). Prompts to enter the password.
#' The password is not yet encrypted - working on it.
#' @details A shinto config file (as of 2022) is a YAML (typically `conf/config.yml`) contains postgres
#' database connection details including passwords. An entry looks like:
#'
#' \preformatted{
#'
#'   Eindhoven:
#'     dbname: "wbm_eindhoven"
#'     dbhost: "127.0.0.1"
#'     dbport: 2222
#'     dbuser: "wbm_eindhoven"
#'     dbpassword: "<<PASSWORD>>"
#' }
#'
#' Details:
#' * `dbname` is the name of the Postgres database
#' * `dbhost` is the address; here it is localhost used in the port forwarding scenario (see below)
#' * `dbport` is the port used in local forwarding (varies with user, see below)
#' * `dbpassword` is the password as stored in 1PassWord. Not yet encrypted but watch this space!
#'
#' A connection can then be made with `shintobag::shinto_db_connection("Eindhoven")`.
#'
#' The port used in local forwarding is read from the environment variable `SHINTO_DEV3_LOCAL_PORT`.
#' Or, a menu appears where you can enter it (but it is not remembered).
#'
#' A config file consists of three blocks: default, development and production. Which block
#' is used in by [shintobag::shinto_db_connection()]) depends on the environment variable `R_CONFIG_ACTIVE`.
#' If it is not set, `default` is used, and otherwise the value of the variable. On the
#' shinto rsconnect servers, `devapp.shintolabs.net` has the value `development` for
#' this environment variable, and `app.shintolabs.net` a value of `production`. More
#' could follow in the future.
#'
#' It is a convention for shinto postgres databases to have equal username and database names (for use in applications).
#' However this is not always the case!
#' @seealso [connect()]
#' @param name Entry name in the config file
#' @param dbname Database name
#' @param dbuser User name. Default is database name.
#' @param file Path to config file
#' @param where Development or production. If developments, adds an entry block to both default and development.
#' @param encrypt If TRUE, encrypts the config entry
#' @param ... Further arguments passed to `db_entry_list`
#' @export
#' @examples
#' \dontrun{
#' add_config_entry("Testdatabase", "data_test", file = "test/config.yml", where = "development")
#' }
add_config_entry <- function(name, dbname,  dbuser = dbname,
                             file = "conf/config.yml",
                             where = c("development","production","eindhoven_premium"),
                             encrypt = TRUE,
                             ...){


  where <- match.arg(where)

  conf <- read_config(file)

  if(where == "development"){

    if(!has_config_entry(name, "development", conf)){

      lis_default <- db_entry_list(name, dbname, dbuser, infra = "dev3",
                                   local = TRUE, encrypt = encrypt)

      # pressed cancel
      if(is.null(lis_default)){
        cli::cli_alert_info("shintodb::add_config_entry() : password entry cancelled")
        return(invisible(NULL))
      }

      lis_dev3 <- db_entry_list(name, dbname, dbuser, infra = "dev3", local = FALSE,
                                encrypt = FALSE,  # <- already encrypted in previous list (or not)
                                password = lis_default[[1]]$dbpassword)


      conf$default <- c(conf$default, lis_default)
      conf$development <- c(conf$development, lis_dev3)
    } else {
      cli::cli_alert_info("shintodb::add_config_entry() : Config entry already in config file; skipped")
    }

  } else {

    if(!has_config_entry(name, where, conf)){

      if(where == "production"){
        infra <- "p3"
      } else {
        infra <- where
      }

      lis_prod <- db_entry_list(name, dbname, dbuser, infra = infra,
                                local = FALSE, encrypt = encrypt)

      conf[[where]] <- c(conf[[where]], lis_prod)

    } else {
      cli::cli_alert_info("shintodb::add_config_entry() : Config entry already in config file; skipped")
      return(invisible(NULL))
    }

  }


  yaml::write_yaml(conf, file)


}



#' Make config file
#' @description If we don't have `conf/config.yml` yet, generate it  (empty).
#' @export
make_config <- function(){

  dir.create("conf", showWarnings = FALSE)

  fn <- "conf/config.yml" # hardcoded
  if(!file.exists(fn)){
    writeLines("", fn)
  } else {
    cli::cli_alert_info("make_config() ignored, conf/config.yml already exists.")
  }

}

#' Check if a config entry is available
#' @param name The config entry, for example "data_bag"
#' @param where The block to look for the entry (development, production)
#' @param conf Optional: a config list read with [read_config()]
#' @param config_file Path to the config file
#' @returns TRUE if a config entry is found
#' @export
has_config_entry <- function(name, where, conf = NULL, config_file = "conf/config.yml"){

  if(is.null(conf)){
    conf <- read_config(config_file)
  }

  !is.null(conf[[where]][[name]])

}

#' Read a config file (with read_yaml)
#' @param file Typically conf/config.yml
#' @importFrom yaml read_yaml
#' @export
read_config <- function(file = "conf/config.yml"){

  if(!file.exists(file)){
    stop("File not found. First add a file like conf/config.yml")
  }

  yaml::read_yaml(file)

}



#=------- Utils







db_entry_list <- function(name, dbname, dbuser = dbname, infra,
                          local = FALSE, password = NULL, encrypt = FALSE){

  msg <- glue("Password voor {dbname}, user {dbuser}, infra: {infra}")

  if(interactive() && is.null(password)){
    password <- rstudioapi::askForPassword(msg)
  }

  if(is.null(password)){
    return(NULL)
  }

  if(encrypt){
    secr <- get_shinto_pass_secret()
    password <- encrypt(password, secr)
  }

  stats::setNames(
    list(
      list(
        dbname = dbname,
        dbhost = get_host(infra, local),
        dbport = get_port(infra, local),
        dbuser = dbuser,
        #old method; for when still @.... was needed behind the dbuser
        #kept for one more cycle to be sure, can be deleted after 2024-06-01
        #dbuser = get_dbuser(dbuser, infra, local),
        dbpassword = password
      )
    ), name)

}




#---- Unexported utils
get_user_local_port <- function(infra = c("dev3","p3")){

  infra <- match.arg(infra)


  if(infra == "dev3"){
    dev3_port <- Sys.getenv("SHINTO_DEV3_LOCAL_PORT")

    if(dev3_port == ""){
      dev3_port <- rstudioapi::showPrompt("Portforwarding : dev3",
                                          "Welke port gebruik je voor lokaal forwarden naar postgres-dev3? Gebruik SHINTO_DEV3_LOCAL_PORT env. var. om dit menu niet te zien"
      )
    }

    return(as.integer(dev3_port))
  }

  if(infra == "p3"){

    p3_port <- Sys.getenv("SHINTO_P3_LOCAL_PORT")

    if(p3_port == ""){
      p3_port <- rstudioapi::showPrompt("Portforwarding : p3",
                                        "Welke port gebruik je voor lokaal forwarden naar postgres-p3? Gebruik SHINTO_P3_LOCAL_PORT env. var. om dit menu niet te zien"
      )
    }

    return(as.integer(p3_port))
  }

}


get_port <- function(infra, local = FALSE){

  if(!local){
    6432L   # postgres default (integer!)
  } else {
    get_user_local_port(infra)
  }

}

get_host <- function(infra = c("dev3","p3","eindhoven_premium"), local = FALSE){

  infra <- match.arg(infra)

  if(infra == "eindhoven_premium"){
    "postgres-ehv01.postgres.database.azure.com"
  } else {
    if(!local){
      glue("postgres-{infra}.postgres.database.azure.com")
    } else {
      "localhost"
    }
  }

}

## decrepated
#kept for one more cycle to be sure, can be deleted after 2024-06-01
# get_dbuser <- function(dbuser, infra, local){
#
#   # local=FALSE omdat de db user altijd de azure host heeft (dus niet @localhost)
#   paste0(dbuser, "@", get_host(infra, local = FALSE))
#
# }





