
test_num <- function(charnum) {
  num <- as.numeric(charnum)
  if (any(is.na(num))) {
    stop("cannot convert ", charnum, " to an index")
  }
  num
}

#' Extract sub-list from list
get_listitem <- function(json_list, part, zero_index) {

  subparts <- stringr::str_split_fixed(
    substr(part, 2, nchar(part)), "\\]", 2)
  index <- subparts[[1]]
  field <- subparts[[2]]

  # range subset
  if (grepl(":", index)) {
    indices <- stringr::str_split_fixed(":", index, 2)
    range <- as.numeric(stringr::str_split_fixed(field_id, ":", 2))
    range <- if (zero_index) range + 1 else range
    print(range)
    return(`[`(json, seq(range[[1]], range[[2]])))
  } else {
    # * or index
    if (index == "*") {
      this_json <- lapply(json, `[[`, field_id)
    } else {
      index <- test_num(index)
      index <- if (zero_index) index + 1 else index
      this_json <- json[[index]]
    }
  }

  this_json
}

#' Extract named object from object
get_obj <- function(json, name) {

  if (is.null(names(json))) {
    stop("attempted named query on array, index the ",
      name, " part with [*]")
  }

  if (!name %in% names(json)) {
    stop(name, " not found in json (",
      paste(names(json), collapse = ","))
  }

  `[[`(json, name)
}

# recurse through each part with each json subset
# index needs to associate with RHS:
# x[*].y means get y from all x
parse_part <- function(part, json, zero_index) {

  if (grepl("^\\[", part)) {
    # indexing a list
    get_fn <- get_listitem(json, part, zero_index = zero_index)
  } else {
    # requesting a single named object
    this_json <- get_obj(json, part)
  }

  this_json
}

parse_jpath <- function(json, path, zero_index = TRUE) {

  # if not strict, don't skip first
  parts <- stringr::str_split(path, "\\.|\\[", simplify = TRUE)[-1]
  parts <- gsub("^(.*?)\\]", "[\\1]", parts)
  print(parts)

  this_json <- json
  for (p in parts) {
    message(p)
    next_json <- parse_part(p, this_json, zero_index)
    this_json <- next_json
    # print(next_json)
  }

  this_json
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

  results <- parse_jpath(json, path, zero_index)

  results
}


json <- read_json("tests/testthat/bookstore.json")
books <- json_path(json, "$.store.book[1].author")
