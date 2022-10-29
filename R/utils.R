
# Utils
# - not exported




get_shinto_pass_secret <- function(){
  Sys.getenv("SHINTO_PASS_SECRET")
}



validate_shinto_pass_secret <- function(secret){

  if(is.null(secret) || secret == ""){
    stop("Set environment variable SHINTO_PASS_SECRET or provide a secret. See ?shintodb::encrypt.")
  }

}
