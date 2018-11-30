#' @title List Secret Version Ids
#' @description AWS Secrets Manager List Secret Version Ids
#' @param string The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
#' @param boolean (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
#' @param int (Optional) Limits the number of results that you want to include in the response.
#' @param string (Optional) Used for retrieving results beyond the first page. Use the NextToken value from the previous response to get the next page of data.
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Lists all of the versions attached to the specified secret. The output does not include the SecretString or SecretBinary fields. By default, the list includes only versions that have at least one staging label in VersionStage attached.
#' @return A lists of all of the versions attached to the specified secret.
#' @export
list_secret_version_ids<- function(secret_id,
                        include_deprecated,
                        max_results,
                        next_token,
                        verbose = getOption("verbose", FALSE)) {

  fields = list()
  fields$SecretId = secret_id

  if(!missing(max_results)) {
    fields$MaxResults = max_results
  }

  if(!missing(next_token)) {
    fields$NextToken = next_token
  }

  if(!missing(include_deprecated)) {
    fields$IncludeDeprecated = include_deprecated
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("ListSecretVersionIds", request_body)

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
