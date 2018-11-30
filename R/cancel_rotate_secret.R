#' @title Cancel Rotate Secret
#' @description AWS Secrets Manager - Disables automatic scheduled rotation and cancels the rotation of a secret if one is currently in progress.
#' @param secret_id The character string ID of the secret to retrieve
#' @param verbose TRUE to print out debug information. Defaults to FALSE
#' @details Disables automatic scheduled rotation and cancels the rotation of a secret if one is currently in progress.
#' @return The meta data about the secret stored in Secrets Manager (Name, ARN, Version)
#' @export
cancel_rotate_secret<- function(secret_id,
                                         verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id), auto_unbox = TRUE)

  b = prepare_request("CancelRotateSecret", request_body)

  if (isTRUE(verbose)) {
    message(paste("Making HTTP 1.1 request to Secrets Manager endpoint at region", b$region))
  }

  httr::set_config(httr::config(http_version = 0))
  r <- httr::POST(b$url, b$headers, body = request_body)

  content <- httr::content(r, "text", encoding = "UTF-8")
  status_code = httr::status_code(r)

  if (status_code == 200) {
    return(content)
  }
  else {
    if (isTRUE(verbose)) {
      message(paste("Requested failed with code ", status_code, " and message ", content))
    }
    else {
      message(paste("Requested failed with code ", status_code))
    }
  }
}
