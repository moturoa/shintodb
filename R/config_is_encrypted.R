

#' Check if a config file is encrypted
#' @description Checks whether a `config.yml` file is completely or partially encrypted.
#' Returns TRUE when all `dbpassword` (or other `password_names`) entries are already encrypted,
#' FALSE when one or more are not yet encrypted (with a message)
#' @param file Path to config file (no default ued)
#' @param password_names Field names in YAML with passwords (can be vector)
#' @param secret The encryption secret
#' @seealso \code{\link{encrypt_config_file}}
#' @examples
#' \dontrun{
#' Sys.setenv(SHINTO_PASS_SECRET = "somesecret")
#' config_is_encrypted("conf/config.yml")
#' }
#' @export
config_is_encrypted <- function(file = "conf/config.yml",
                                password_names = "dbpassword",
                                secret = Sys.getenv("SHINTO_PASS_SECRET")){

  obj <- unlist(yaml::read_yaml(file))
  pass <- obj[grepl(password_names, names(obj))]

  i_enc <- string_is_encrypted(pass, secret)

  if(all(i_enc)){
    cli::cli_alert_info("All passwords are encrypted in this file.")
    return(TRUE)
  } else if(sum(i_enc) > 0){
    cli::cli_alert_info("Some passwords are encrypted in this file, run shintodb::encrypt_config_file() to try again.")
    return(FALSE)
  } else {
    cli::cli_alert_info("None of the passwords are encrypted in this file, run shintodb::encrypt_config_file()")
    return(FALSE)
  }

}
