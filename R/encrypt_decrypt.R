
#' Encrypt a password
#' @description Symmetric encryption/decryption with a secret key.
#' Uses the `libsodium` encryption library (in the `sodium` package).
#' Unlike `safer::encrypt_string` on which this function is based, it takes a
#' vector argument.
#' @details Set the environment variable SHINTO_PASS_SECRET. This is user-specific.
#' Do not reuse. Otherwise, provide a `secret` as an
#' @param x A character string or character vector.
#' @param secret The secret used for decrypting/encrypting
#' @export
#' @rdname encrypt
#' @importFrom safer encrypt_string decrypt_string
encrypt <- function(x, secret = Sys.getenv("SHINTO_PASS_SECRET")){

  validate_shinto_pass_secret(secret)

  vapply(x, safer::encrypt_string, key = secret,
         USE.NAMES = FALSE, FUN.VALUE = character(1)
  )
}

#' @export
#' @rdname encrypt
decrypt <- function(x, secret = Sys.getenv("SHINTO_PASS_SECRET")){

  validate_shinto_pass_secret(secret)

  decrypt_try <- function(x, key){
    tryCatch(
      safer::decrypt_string(x,key),
      error = function(e)NA_character_
    )
  }

  vapply(x,
         decrypt_try,
         key = secret,
         USE.NAMES = FALSE,
         FUN.VALUE = character(1)
  )

}

#' @export
#' @rdname encrypt
string_is_encrypted <- function(string, secret = Sys.getenv("SHINTO_PASS_SECRET")){
  if(secret == "")return(FALSE)
  !is.na(decrypt(string, secret = secret))
}












