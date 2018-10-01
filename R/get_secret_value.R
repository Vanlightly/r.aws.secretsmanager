#' @title Get Secret String Value
#' @description AWS Secrets Manager Get Secret Value
#' @param string The ID of the secret to retrieve
#' @param string The version ID of the target secret (optional)
#' @param string The version stage of the target secret (optional)
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details This function returns the corresponding secret string
#' @return The secret and related fields returned by the Secrets Manager API
#' @export
get_secret_value<- function(secret_id,
                             version_id,
                             version_stage,
                             verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id), auto_unbox = TRUE)

  req = prepare_request("GetSecretValue", request_body)

  if (isTRUE(verbose)) {
    message(paste("Making HTTP 1.1 request to Secrets Manager endpoint at region", b$region))
  }

  httr::set_config(httr::config(http_version = 0))
  res <- httr::POST(req$url, req$headers, body = request_body)

  content <- httr::content(res, "text", encoding = "UTF-8")
  status_code = httr::status_code(res)

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
