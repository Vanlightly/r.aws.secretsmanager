#' @title Put Resource Policy
#' @description AWS Secrets Manager Put Resource Policy
#' @param string A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions.
#' @param string Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Attaches the contents of the specified resource-based permission policy to a secret. A resource-based policy is optional.
#' @return The ARN and name of the new policy
#' @export
put_resource_policy<- function(resource_policy,
                               secret_id,
                                verbose = getOption("verbose", FALSE)) {

  fields = list(SecretId = secret_id, ResourcePolicy = resource_policy)

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("PutResourcePolicy", request_body)

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
