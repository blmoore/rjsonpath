
#' Apply JSONPath to a json object
#'
#' Takes a JSON object as loaded via the
#' \code{read_json} function and applies
#' a JSONPath query.
#'
#' JSONPath is an XPath-like language for
#' querying JSON data, see \url{http://goessner.net/articles/JsonPath/}
#' for details.
#'
#' @param json a JSON object loaded
#'   by \code{\link[jsonlite]{read_json}}
#' @param path a JSONPath expression string,
#'   see details
#' @param strict require JSONPath starts with '$.'
#' @param zero_index numeric indices start from 0
#'   rather than 1
#' @param simplify convert simple list results to
#'   vectors
#'
#' @return a list or vector of JSONPath results
#'
#' @examples
#' \dontrun{
#' json <- read_json("bookstore.json")
#' authors <- json_path(json, "$.store.books[*].author")
#' # [1] "Nigel Rees"       "Evelyn Waugh"     "Herman Melville"  "J. R. R. Tolkien"
#' }
#'
#'
#' @export
json_path <- function(json, path, strict = TRUE,
  zero_index = TRUE, simplify = TRUE) {

  if (!class(json) == "list") {
    stop("json must be a list produced by read_json")
  }

  if (strict & substr(path, 0, 2) != "$.") {
    stop("JSONPath expression must start with '$.'")
  }

  normed_path <- format_path(path)
  if (zero_index) {
    normed_path <- adjust_indices(normed_path)
  }

  message("processing ", normed_path)
  results <- process_piece(normed_path, json, "")

  if (simplify) {
    if (!is_nested(results)) {
      results <- unlist(results)
    }
  }

  results
}

