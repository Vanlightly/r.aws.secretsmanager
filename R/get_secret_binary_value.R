#' @title Get Secret Binary Value
#' @description AWS Secrets Manager Get Secret Binary Value
#' @param string The ID of the secret to retrieve
#' @param string The version ID of the target secret (optional)
#' @param string The version stage of the target secret (optional)
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details This function returns the corresponding secret binary represented as a Base64-encoded string.
#' @return The secret binary represented as a Base64-encoded string.
#' @export
get_secret_binary_value <- function(secret_id,
                                    version_id,
                                    version_stage,
                                    verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id), auto_unbox = TRUE)

  req = prepare_request("GetSecretValue", request_body)
  if (isTRUE(req$hasError)) {
    # read up on error handling
  }

  if (isTRUE(verbose)) {
    message(paste("Making HTTP 1.1 request to Secrets Manager endpoint at region", b$region))
  }

  httr::set_config(httr::config(http_version = 0))
  res <- httr::POST(req$url, req$headers, body = request_body)

  content <- httr::content(res, "text", encoding = "UTF-8")
  status_code = httr::status_code(res)

  if (status_code == 200) {
    json <- jsonlite::fromJSON(content)
    return(json$SecretBinary)
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
