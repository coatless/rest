
#' REST API Access
#'
#' Minimal wrapper to work with REST APIs.
#'
#' @docType package
#' @name rest
"_PACKAGE"

#' Query REST API's on the Web
#'
#' This is an extremely minimal client. You need to know the API
#' to be able to use this client. All this function does is:
#' * Try to substitute each listed parameter into `endpoint`, using the
#'   `:parameter` notation.
#' * If a GET request (the default), then add all other listed parameters
#'   as query parameters.
#' * If not a GET request, then send the other parameters in the request
#'   body, as JSON.
#' * Convert the response to an R list using [jsonlite::fromJSON()].
#'
#' @param endpoint API endpoint. Must be one of the following forms:
#'    * `METHOD path`, e.g. `GET /rate_limit`,
#'    * `path`, e.g. `/rate_limit`,
#'    * `METHOD url`, e.g. `GET https://api.domain.com/v1/rate_limit`,
#'    * `url`, e.g. `https://api.domain.com/v1/rate_limit`.
#'
#'    If the method is not supplied, will use `.method`, which defaults
#'    to `"GET"`.
#' @param ... Name-value pairs giving API parameters. Will be matched
#'   into `endpoint` placeholders, sent as query parameters in GET
#'   requests, and as a JSON body of POST requests. If there is only one
#'   unnamed parameter, and it is a raw vector, then it will not be JSON
#'   encoded, but sent as raw data, as is. This can be used for example to
#'   add assets to releases. Named `NULL` values are silently dropped,
#'   and named `NA` values trigger an error.
#' @param per_page Number of items to return per page. If omitted,
#'   will be substituted by `max(.limit, 100)` if `.limit` is set,
#'   otherwise determined by the API (never greater than 100).
#' @param .destfile path to write response to disk.  If NULL (default), response will
#'   be processed and returned as an object.  If path is given, response will
#'   be written to disk in the form sent.
#' @param .overwrite if `.destfile` is provided, whether to overwrite an
#'   existing file.  Defaults to FALSE.
#' @param .token Authentication token. Defaults to `GITHUB_PAT` or
#'   `GITHUB_TOKEN` environment variables, in this order if any is set.
#'   See [rest_token()] if you need more flexibility, e.g. different tokens
#'   for different GitHub Enterprise deployments.
#' @param .api_url Github API url (default: <https://api.github.com>). Used
#'   if `endpoint` just contains a path. Defaults to `GITHUB_API_URL`
#'   environment variable if set.
#' @param .method HTTP method to use if not explicitly supplied in the
#'    `endpoint`.
#' @param .limit Number of records to return. This can be used
#'   instead of manual pagination. By default it is `NULL`,
#'   which means that the defaults of the GitHub API are used.
#'   You can set it to a number to request more (or less)
#'   records, and also to `Inf` to request all records.
#'   Note, that if you request many records, then multiple GitHub
#'   API calls are used to get them, and this can take a potentially
#'   long time.
#' @param .accept The value of the `Accept` HTTP header. Defaults to
#'   `"application/vnd.github.v3+json"` . If `Accept` is given in
#'   `.send_headers`, then that will be used. This paramter can be used to
#'   provide a custom media type, in order to access a preview feature of
#'   the API.
#' @param .send_headers Named character vector of header field values
#'   (except `Authorization`, which is handled via `.token`). This can be
#'   used to override or augment the default `User-Agent` header:
#'   `"https://github.com/r-lib/gh"`.
#' @param .progress Whether to show a progress indicator for calls that
#'   need more than one HTTP request.
#'
#' @return Answer from the API as a `rest_response` object, which is also a
#'   `list`. Failed requests will generate an R error. Requests that
#'   generate a raw response will return a raw vector.
#'
#' @importFrom httr content add_headers headers
#'   status_code http_type GET POST PATCH PUT DELETE
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils URLencode capture.output
#' @importFrom cli cli_status cli_status_update
#'
#' @export
rest <- function(endpoint, ..., per_page = NULL, .token = NULL, .destfile = NULL,
               .overwrite = FALSE, .api_url = NULL, .method = "GET",
               .limit = NULL, .accept = "application/vnd.github.v3+json",
               .send_headers = NULL, .progress = TRUE) {

  params <- list(...)
  params <- drop_named_nulls(params)
  check_named_nas(params)

  if (is.null(per_page)) {
    if (!is.null(.limit)) {
      per_page <- max(min(.limit, 100), 1)
    }
  }

  if (!is.null(per_page)) {
    params <- c(params, list(per_page = per_page))
  }

  req <- rest_build_request(endpoint = endpoint, params = params,
                          token = .token, destfile = .destfile,
                          overwrite = .overwrite, accept = .accept,
                          send_headers = .send_headers,
                          api_url = .api_url, method = .method)

  if (.progress) prbr <- make_progress_bar(req)

  raw <- rest_make_request(req)

  res <- rest_process_response(raw)

  while (!is.null(.limit) && length(res) < .limit && rest_has_next(res)) {
    update_progress_bar(prbr, res)
    res2 <- rest_next(res)
    res3 <- c(res, res2)
    attributes(res3) <- attributes(res2)
    res <- res3
  }

  if (! is.null(.limit) && length(res) > .limit) {
    res_attr <- attributes(res)
    res <- res[seq_len(.limit)]
    attributes(res) <- res_attr
  }

  res
}

rest_make_request <- function(x) {

  method_fun <- list("GET" = GET, "POST" = POST, "PATCH" = PATCH,
                     "PUT" = PUT, "DELETE" = DELETE)[[x$method]]
  if (is.null(method_fun)) throw(new_error("Unknown HTTP verb"))

  raw <- do.call(method_fun,
                 compact(list(url = x$url, query = x$query, body = x$body,
                              add_headers(x$headers), x$dest)))
  raw
}
