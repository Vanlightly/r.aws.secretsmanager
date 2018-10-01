prepare_request <- function(target_method,
                     request_body,
                     verbose = getOption("verbose", FALSE)) {
  headers = list()
  credentials <- aws.signature::locate_credentials()
  key <- credentials$key
  secret <- credentials$secret
  session_token <- credentials$session_token
  region <- credentials$region

  if (is.null(key)) {
    return(structure(list(hasError = TRUE, error = "No AWS access key was found.")))
  }

  if (is.null(secret)) {
    return(structure(list(hasError = TRUE, error = "No AWS secret key was found.")))
  }

  if (isTRUE(verbose)) {
    message("Signing request with v4")
  }

  url <- paste0("https://secretsmanager.", region, ".amazonaws.com")
  p <- httr::parse_url(url)
  action <- if (p$path == "") "/" else paste0("/", p$path)
  hostname <- paste0("secretsmanager.", region, ".amazonaws.com")
  current <- Sys.time()
  d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
  amz_target <- paste0("secretsmanager.", target_method)
  content_type <- 'application/x-amz-json-1.1'

  # parse headers
  canonical_headers <- c(list(host = hostname,
                              `x-amz-date` = d_timestamp,
                              `x-amz-target` = amz_target,
                              `content-type` = content_type),
                         headers)

  query <- NULL

  Sig <- aws.signature::signature_v4_auth(
    datetime = d_timestamp,
    region = region,
    service = "secretsmanager",
    verb = "POST",
    action = "/",
    query_args = query,
    canonical_headers = canonical_headers,
    request_body = request_body,
    key = key,
    secret = secret,
    session_token = session_token,
    verbose = verbose)
  headers[["X-Amz-Date"]] <- d_timestamp
  headers[["X-Amz-Content-Sha256"]] <- Sig$BodyHash
  headers[["X-Amz-Target"]] <- amz_target
  headers[["Content-Type"]] <- content_type
  headers[["Content-Length"]] <- nchar(request_body)

  if (!is.null(session_token) && session_token != "") {
    headers[["X-Amz-Security-Token"]] <- session_token
  }
  headers[["Authorization"]] <- Sig[["SignatureHeader"]]
  H <- do.call(httr::add_headers, headers)

  return(structure(list(headers = H, url = url, region = region, hasError = FALSE)))
}
