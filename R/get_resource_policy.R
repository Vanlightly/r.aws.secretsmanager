#' @title Get Resource Policy
#' @description AWS Secrets Manager Get Resource Policy
#' @param string The ID of the secret whose policy you which to retrieve
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Retrieves the JSON text of the resource-based policy document that's attached to the specified secret. The JSON request string input and response output are shown formatted with white space and line breaks for better readability.
#' @return The JSON text of the resource-based policy document
#' @export
get_resource_policy<- function(secret_id,
                           verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id), auto_unbox = TRUE)

  b = prepare_request("GetResourcePolicy", request_body)

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
