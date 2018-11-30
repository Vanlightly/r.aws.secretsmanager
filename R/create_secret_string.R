#' @title Create Secret String
#' @description AWS Secrets Manager Create Secret String
#' @param secret_id The character string ID of the secret to create. It should be the friendly name of the new secret. Minimum length of 1. Maximum length of 512.
#' @param secret_string The character string secret text to be stored and encrypted. To encrypt multiple values, it is recommended to use use a JSON text string argument and specify key/value pairs.
#' @param description (Optional) The character string description of the secret. Maximum length of 2048
#' @param cmk (Optional) Character string that specifies the ARN, Key ID, or alias of the AWS KMS customer master key (CMK) to be used to encrypt the SecretString or SecretBinary values in the versions stored in this secret.
#' @param tags (Optional) A data frame with Key and Value fields that specifies a list of user-defined tags that are attached to the secret. Each tag is a "Key" and "Value" pair of strings. This operation only appends tags to the existing list of tags.
#' @param verbose TRUE to print out debug information. Defaults to FALSE
#' @details Creates a new secret. A secret in Secrets Manager consists of both the protected secret data and the important information needed to manage the secret.
#' @return The meta data about the secret stored in Secrets Manager
#' @export
create_secret_string<- function(secret_id,
                                secret_string,
                                description = getOption("description", ""),
                                cmk = getOption("cmk_id", ""),
                                tags = getOption("tags", data.frame()),
                                verbose = getOption("verbose", FALSE)) {

  requestId = uuid::UUIDgenerate()
  fields = list(Name = secret_id, SecretString = secret_string, ClientRequestToken = requestId)

  if(description != "") {
    fields$Description = description
  }

  if(cmk != "") {
    fields$KmsKeyId = cmk
  }

  if(length(tags) > 0) {
    fields$Tags = tags
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("CreateSecret", request_body)

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
