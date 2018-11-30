#' @title Tag Resource
#' @description AWS Tag Resource
#' @param string The ID of the secret to retrieve
#' @param dataframe A dataframe with columns Key and Value
#' @param boolean TRUE to print out debug information. Defaults to FALSE
#' @details Attaches one or more tags, each consisting of a key name and a value, to the specified secret. Tags are part of the secret's overall metadata, and are not associated with any specific version of the secret. This operation only appends tags to the existing list of tags.
#' @return The meta data about the secret stored in Secrets Manager (Name, ARN, Version)
#' @example
#' keys = c("Tag1","Tag2")
#' values = c("Redshift","Athena")
#' tags = data.frame(Key=keys, Value=values)
#' tag_resource("my-secret", tags)
#' @export
tag_resource<- function(secret_id,
                        tags,
                        verbose = getOption("verbose", FALSE)) {

  request_body <- jsonlite::toJSON(list(SecretId = secret_id, Tags = tags), auto_unbox = TRUE)

  if (isTRUE(verbose)) {
    print(request_body)
  }

  b = prepare_request("TagResource", request_body)

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
