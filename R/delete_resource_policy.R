#' @title Delete Resource Policy
#' @description AWS Secrets Manager Delete Resource Policy
#' @param secret_id The Secret ID of the secret that you want to delete the attached resource-based policy for.
#' @param verbose TRUE to print out debug information. Defaults to FALSE
#' @details Deletes the resource-based permission policy that's attached to the secret.
#' @return The meta data about the secret stored in Secrets Manager
#' @export
delete_resource_policy<- function(secret_id,
                           verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id), auto_unbox = TRUE)

  b = prepare_request("DeleteResourcePolicy", request_body)

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
