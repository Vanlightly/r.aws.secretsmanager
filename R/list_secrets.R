#' @title List Secrets
#' @description AWS Secrets Manager List Secrets
#' @param int (Optional) Limits the number of results that you want to include in the response.
#' @param string (Optional) Used for retrieving results beyond the first page. Use the NextToken value from the previous response to get the next page of data.
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Lists all of the secrets that are stored by Secrets Manager in the AWS account. The encrypted fields SecretString and SecretBinary are not included in the output. If additional items exist beyond the maximum you specify, the NextToken response element is present and has a value (isn't null). Include that value as the NextToken request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check NextToken after every operation to ensure that you receive all of the results.
#' @return A list of secrets without the actual secrets themselves
#' @export
list_secrets<- function(max_results,
                        next_token,
                        verbose = getOption("verbose", FALSE)) {

  fields = list()

  if(!missing(max_results)) {
    fields$MaxResults = max_results
  }

  if(!missing(next_token)) {
    fields$NextToken = next_token
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("ListSecrets", request_body)

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
