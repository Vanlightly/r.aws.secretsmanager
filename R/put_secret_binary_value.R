#' @title Put Secret Binary Value
#' @description AWS Secrets Manager Put Secret Binary Value
#' @param string Specifies the secret to which you want to add a new version. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret. The secret must already exist.
#' @param string The secret binary, Base64 encoded, to be stored and encrypted.
#' @param array (Optional) An array of strings. Specifies a list of staging labels that are attached to this version of the secret. These staging labels are used to track the versions through the rotation process by the Lambda rotation function.
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Stores a new encrypted secret value in the specified secret. To do this, the operation creates a new version and attaches it to the secret.
#' @return The meta data about the secret stored in Secrets Manager
#' @export
put_secret_binary_value<- function(secret_id,
                                   secret_binary,
                                   version_stages,
                                   verbose = getOption("verbose", FALSE)) {

  requestId = uuid::UUIDgenerate()
  fields = list(SecretId = secret_id, SecretBinary = secret_string, ClientRequestToken = requestId)

  if(!missing(version_stages)) {
    fields$VersionStages = version_stages
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("PutSecretValue", request_body)

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
