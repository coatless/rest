
extract_link <- function(rest_response, link) {
  headers <- attr(rest_response, "response")
  links <- headers$link
  if (is.null(links)) {
    return(NA_character_)
  }
  links <- trim_ws(strsplit(links, ",")[[1]])
  link_list <- lapply(links, function(x) {
    x <- trim_ws(strsplit(x, ";")[[1]])
    name <- sub("^.*\"(.*)\".*$", "\\1", x[2])
    value <- sub("^<(.*)>$", "\\1", x[1])
    c(name, value)
  })
  link_list <- structure(
    vapply(link_list, "[", "", 2),
    names = vapply(link_list, "[", "", 1)
  )

  if (link %in% names(link_list)) {
    link_list[[link]]
  } else {
    NA_character_
  }
}

rest_has <- function(rest_response, link) {
  url <- extract_link(rest_response, link)
  !is.na(url)
}

rest_has_next <- function(rest_response) {
  rest_has(rest_response, "next")
}

rest_link_request <- function(rest_response, link) {

  stopifnot(inherits(rest_response, "rest_response"))

  url <- extract_link(rest_response, link)
  if (is.na(url)) throw(new_error("No ", link, " page"))

  list(method = attr(rest_response, "method"),
       url = url,
       headers = attr(rest_response, ".send_headers"))

}

rest_link <- function(rest_response, link) {
  req <- rest_link_request(rest_response, link)
  raw <- rest_make_request(req)
  rest_process_response(raw)
}

rest_extract_pages <- function(rest_response) {
  last <- extract_link(rest_response, "last")
  if (grepl("&page=[0-9]+$", last)) {
    as.integer(sub("^.*page=([0-9]+)$", "\\1", last))
  }
}

#' Get the next, previous, first or last page of results
#'
#' @details
#' Note that these are not always defined. E.g. if the first
#' page was queried (the default), then there are no first and previous
#' pages defined. If there is no next page, then there is no
#' next page defined, etc.
#'
#' If the requested page does not exist, an error is thrown.
#'
#' @param rest_response An object returned by a [gh()] call.
#' @return Answer from the API.
#'
#' @seealso The `.limit` argument to [gh()] supports fetching more than
#'   one page.
#'
#' @name rest_next
#' @export
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' x <- gh("/users")
#' vapply(x, "[[", character(1), "login")
#' x2 <- rest_next(x)
#' vapply(x2, "[[", character(1), "login")

rest_next <- function(rest_response) rest_link(rest_response, "next")

#' @name rest_next
#' @export

rest_prev <- function(rest_response) rest_link(rest_response, "prev")

#' @name rest_next
#' @export

rest_first <- function(rest_response) rest_link(rest_response, "first")

#' @name rest_next
#' @export

rest_last <- function(rest_response) rest_link(rest_response, "last")

make_progress_bar <- function(rest_request) {
  state <- new.env(parent = emptyenv())
  state$pageno <- 0L
  state$got <- 0L
  state$status <- NULL
  state
}

update_progress_bar <- function(state, rest_response) {
  state$pageno <- state$pageno + 1L
  state$got <- state$got + length(rest_response)
  state$pages <- rest_extract_pages(rest_response) %||% state$pages

  if (is.null(state$status)) {
    state$status <- cli_status(
      "{.alert-info Running gh query}",
      .envir = parent.frame()
    )
  }

  total <- NULL
  if (!is.null(state$pages)) {
    est <- state$pages * (state$got / state$pageno)
    if (est >= state$got) total <- est
  }

  cli_status_update(
    state$status,
    c("{.alert-info Running gh query, got {state$got} record{?s}}",
      if (!is.null(total)) " of about {total}")
  )

  invisible(state)
}
