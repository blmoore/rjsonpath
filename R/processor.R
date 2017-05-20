
test_num <- function(charnum) {
  num <- as.numeric(charnum)
  if (any(is.na(num))) {
    stop("cannot convert ", charnum, " to an index")
  }
  num
}

# http://stackoverflow.com/a/15382299/1274516
is_nested <- function(l) {
  if (!is.list(l)) {
    return(FALSE)
  }
  for (i in l) {
    if (is.list(i)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Extract sub-list from list
get_listitem <- function(json, part, zero_index) {

  index <- gsub("\\[|\\]", "", part)
  # range subset
  if (grepl(":", index)) {
    indices <- stringr::str_split_fixed(":", index, 2)
    range <- as.numeric(stringr::str_split_fixed(index, ":", 2))
    range <- if (zero_index) range + 1 else range
    return(`[`(json, seq(range[[1]], range[[2]])))
  } else {
    # * or index
    if (index == "*") {
      this_json <- lapply(json, `[[`, field_id)
    } else {
      index <- test_num(index)
      index <- if (zero_index) index + 1 else index
      if (index > length(json)) {
        stop("subscript ", part, " longer than array ",
          "length (", length(json), ")")
      }
      this_json <- json[[index]]
    }
  }

  this_json
}

#' Uses parts like [1:5]author to get a named subset
get_namedlistitem <- function(json, part, zero_index) {

  # print(part)
  index <- sub("\\[(.*?)\\].*", "\\1", part)
  name <- sub(".*?\\]", "" , part)
  # print(index)
  if (index == "*") {
    this_json <- lapply(json, `[[`, name)
  } else {
    if (grepl(":", index)) {
      indices <- stringr::str_split_fixed(":", index, 2)
      range <- as.numeric(stringr::str_split_fixed(index, ":", 2))
      range <- if (zero_index) range + 1 else range
      this_json <- lapply(this_json, `[[`, name)[range]
    } else {
      index <- test_num(index)
      index <- if (zero_index) index + 1 else index
      if (index > length(json)) {
        stop("subscript ", part, " longer than array ",
          "length (", length(json), ")")
      }
      this_json <- lapply(json, `[[`, name)[[index]]
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
    stop(name, " not found in object (names are: ",
      paste(names(json), collapse = ","), ")")
  }

  `[[`(json, name)
}

# recurse through each part with each json subset
# index needs to associate with RHS:
# x[*].y means get y from all x
parse_part <- function(part, json, zero_index) {

  if (grepl("\\]", part)) {
    if (grepl("\\]$", part)) {
      # simple index to end query
      this_json <- get_listitem(json, part, zero_index = zero_index)
    } else {
      # index by named
      message("named list item")
      this_json <- get_namedlistitem(json, part, zero_index = zero_index)
    }
  } else {
    # requesting a single named object
    this_json <- get_obj(json, part)
  }

  this_json
}

parse_jpath <- function(json, path, zero_index = TRUE) {

  # TODO: if not strict, don't skip first
  parts <- stringr::str_split(path, "(?<!\\])\\.", simplify = TRUE)[-1]
  parts <- unlist(strsplit(parts, "\\["))
  parts <- gsub("^(.*?)\\]\\.", "[\\1]", parts)
  message("parts: ", paste(parts, collapse = ", "))

  for (p in parts) {
    json <- parse_part(p, json, zero_index)
  }

  json
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
  strict = TRUE, zero_index = TRUE, simplify = TRUE) {

  if (!class(json) == "list") {
    stop("json must be a list produced by read_json")
  }

  if (strict & substr(path, 0, 2) != "$.") {
    stop("JSONPath expression must start with '$.'")
  }

  results <- parse_jpath(json, path, zero_index)

  if (simplify) {
    if (!is_nested(results)) {
      results <- unlist(results)
    }
  }

  results
}


# js <- read_json("tests/testthat/bookstore.json")
# json_path(js, "$.store.book[*].author")

