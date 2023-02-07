
#' Encrypts a config file
#' @description Finds password fields in a YAML config file, and encrypts them.
#' The results are always written to a new file, with suffix ".encrypted" (or provided
#' `suffix` argument).
#' @param file A YAML file with password entries
#' @param secret Secret used in symmetric encryption (see \code{\link{encrypt}})
#' @param suffix Suffix for new file name
#' @param password_names Entries to look for with passwords.
#' @export
#' @rdname encrypt_file
#' @examples
#' \dontrun{
#' Sys.setenv(SHINTO_PASS_SECRET = "mysecret")
#' encrypt_config_file("config.yml")
#' }
encrypt_config_file <- function(file = "conf/config.yml",
                                secret = Sys.getenv("SHINTO_PASS_SECRET"),
                                suffix = "",
                                password_names = "dbpassword"){

  out_file <- paste0(file, suffix)
  cfg <- read_config(file)

  n_dec <- 0
  n_notdec <- 0

  for(i in seq_along(cfg)){

    x <- cfg[[i]]

    for(j in seq_along(x)){

      ind <- which(names(x[[j]]) %in% password_names)
      if(length(ind) > 0){
        pass <- cfg[[i]][[j]][[ind]]

        # if not already encrypted, encrypt it now
        if(is.na(decrypt(pass))){
          cfg[[i]][[j]][[ind]] <- encrypt(pass, secret = secret)
          n_dec <- n_dec + 1
        } else{
          n_notdec <- n_notdec + 1
        }
      }
    }
  }

  message(glue::glue("{n_dec} passwords encrypted"))
  message(glue::glue("{n_notdec} passwords were already encrypted"))

  yaml::write_yaml(cfg, out_file)
}

