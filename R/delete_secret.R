#' @title Delete Secret
#' @description AWS Secrets Manager Delete Secret
#' @param secret_id The character string ID of the secret to retrieve
#' @param force_delete_without_recovery (Optional) A boolean that specifies that the secret is to be deleted without any recovery window. Defaults to FALSE
#' @param recovery_window (Optional) An integer that specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the ForceDeleteWithoutRecovery parameter in the same API call. This value can range from 7 to 30 days. The default value is 30.
#' @param verbose (Optional) TRUE to print out debug information. Defaults to FALSE
#' @details Deletes an entire secret and all of its versions. You can optionally include a recovery window during which you can restore the secret. If you don't specify a recovery window value, the operation defaults to 30 days.
#' @return The meta data about the secret stored in Secrets Manager
#' @export
delete_secret<- function(secret_id,
                           force_delete_without_recovery,
                           recovery_window,
                           verbose = getOption("verbose", FALSE)) {

  fields = list(SecretId = secret_id)

  if(!missing(force_delete_without_recovery)) {
    fields$ForceDeleteWithoutRecovery = force_delete_without_recovery
  }

  if(!missing(recovery_window)) {
    fields$RecoveryWindow = recovery_window
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("DeleteSecret", request_body)

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
