#' @title Get Random Password
#' @description AWS Secrets Manager Get Random Password
#' @param string (Optional) A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
#' @param boolean (Optional) Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
#' @param boolean (Optional) Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
#' @param boolean (Optional) Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
#' @param boolean (Optional) Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
#' @param boolean (Optional) Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
#' @param int (Optional) The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
#' @param boolean (Optional) A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is True and the operation requires at least one of every character type.
#' @param boolean (Optional) TRUE to print out debug information. Defaults to FALSE
#' @details Generates a random password of the specified complexity. This operation is intended for use in the Lambda rotation function. Per best practice, we recommend that you specify the maximum length and include every character type that the system you are generating a password for can support.
#' @return A string with the generated password.
#' @export
get_random_password<- function(exclude_characters,
                               exclude_lowercase,
                               exclude_numbers,
                               exclude_punctuation,
                               exclude_uppercase,
                               include_space,
                               password_length,
                               require_each_included_type,
                               verbose = getOption("verbose", FALSE)) {

  fields = list()

  if(!missing(exclude_characters)) {
    fields$ExcludeCharacters = exclude_characters
  }

  if(!missing(exclude_lowercase)) {
    fields$ExcludeLowercase = exclude_lowercase
  }

  if(!missing(exclude_numbers)) {
    fields$ExcludeNumbers = exclude_numbers
  }

  if(!missing(exclude_punctuation)) {
    fields$ExcludePunctuation = exclude_punctuation
  }

  if(!missing(exclude_uppercase)) {
    fields$ExcludeUppercase = exclude_uppercase
  }

  if(!missing(include_space)) {
    fields$IncludeSpace = include_space
  }

  if(!missing(password_length)) {
    fields$PasswordLength = password_length
  }

  if(!missing(require_each_included_type)) {
    fields$RequireEachIncludedType = require_each_included_type
  }

  if(length(fields) == 0) {
    fields$PasswordLength = 32
  }

  request_body <- jsonlite::toJSON(fields, auto_unbox = TRUE)

  b = prepare_request("GetRandomPassword", request_body)

  if (isTRUE(verbose)) {
    message(paste("Making HTTP 1.1 request to Secrets Manager endpoint at region", b$region))
  }

  httr::set_config(httr::config(http_version = 0))
  r <- httr::POST(b$url, b$headers, body = request_body)

  content <- httr::content(r, "text", encoding = "UTF-8")
  status_code = httr::status_code(r)

  if (status_code == 200) {
    json <- jsonlite::fromJSON(content)
    return(json$RandomPassword)
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
