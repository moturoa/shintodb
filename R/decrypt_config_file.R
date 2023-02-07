
#' Decrypt a config file
#' @description This function is normally not needed by the user, as [shintodb::connect()] decrypts
#' automatically on connecting. It is used when deploying an app to rsconnect, by `shintoshiny::make_deploy_project`
#' @param file Path to config.yml file
#' @param file_out Path to output file (can be same as `file`)
#' @param password_names Names (can be vector) of fields to be encrypted. Shinto default is `dbpassword`
#' @param secret Symmetric encryption secret, read from environ. var. `SHINTO_PASS_SECRET` if not set
#' @export
# @param config_block Keep only these blocks (e.g. 'default', 'development' etc.)
decrypt_config_file <- function(file, file_out,
                                #config_block = NULL,  # keep only entries that appear in this list
                                secret = Sys.getenv("SHINTO_PASS_SECRET"),
                                password_names = "dbpassword"){

  # normally, the global config
  cfg <- read_config(file)

  # db_config list (app db config)
  #dbc <- cfg[config_block]

  # loop over sections (development, production, etc.)
  for(i in seq_along(cfg)){

    x <- cfg[[i]]
    section <- names(cfg)[i]

    # db_config has no default section; defaults must appear in development
    if(section == "default")section <- "development"

    for(k in seq_along(x)){

      # if(!is.null(config_block) && !names(x)[k] %in% names(dbc[[section]])){
      #   cfg[[i]][[k]] <- NULL
      # } else {

        ind <- which(names(cfg[[i]][[k]]) %in% password_names)

        if(length(ind) > 0){
          pass <- cfg[[i]][[k]][[ind]]

          if(!isTRUE(string_is_encrypted(pass))){
            message(paste("Password for",names(x)[k],"not encrypted or corrupt, skipping"))
          }

          cfg[[i]][[k]][[ind]] <- decrypt(pass, secret = secret)
        }

      #}
    }
  }

  yaml::write_yaml(cfg, file_out)

}
