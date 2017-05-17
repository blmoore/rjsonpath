

parse_jpath <- function(json, path, zero_index = TRUE) {

  parts <- unlist(strsplit(path, "\\.|\\["))[-1]
  offset <- if (zero_index) 1 else 0
  result <- json

  for (p in parts) {
    if (grepl('\\]', p)) {
      p <- as.numeric(sub('\\]', '', p)) + offset
    }
    result <- `[[`(result, p)
  }
  result
}

#' Apply JSONPath to a json object
#'
#' Takes a JSON object as loaded via the
#' \code{read_json} function and applies
#' a JSONPath query.
#'
#' JSONPath is an XPath-like language for
#' querying JSON data.
#'
#' @param json a JSON object loaded
#'   by \code{\link[jsonlite]{read_json}}
#' @param path a JSONPath expression string,
#'   see details
#'
#' @return a \code{data.frame} of JSONPath results
#'
#' @export
json_path <- function(json, path,
  strict = TRUE, zero_index = TRUE) {

  if (!class(json) == "list") {
    stop("json must be a list produced by read_json")
  }

  if (strict & substr(path, 0, 2) != "$.") {
    stop("JSONPath expression must start with '$.'")
  }

  results <- parse_jpath(json, path)

  results
}
