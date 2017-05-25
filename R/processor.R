
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

# Extract sub-list from list
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

# Uses parts like [1:5]author to get a named subset
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

# Extract named object from object
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

  message("processing this part: ", part)
  if (grepl("\\]", part)) {
    if (grepl("\\]$", part)) {
      # simple index to end query
      this_json <- get_listitem(json, part, zero_index = zero_index)
    } else {
      # index by named
      # message("named list item")
      this_json <- get_namedlistitem(json, part, zero_index = zero_index)
    }
  } else {
    # requesting a single named object
    this_json <- get_obj(json, part)
  }

  this_json
}

parse_jpath <- function(json, path, first = TRUE, zero_index = TRUE) {

  # TODO: if not strict, don't skip first
  parts <- stringr::str_split(path, "(?<!\\])\\.", simplify = TRUE)
  if (first) {
    parts <- parts[-1]
  }
  parts <- unlist(strsplit(parts, "\\["))
  parts <- gsub("^(.*?)\\]\\.", "[\\1]", parts)
  # message("parts: ", paste(parts, collapse = ", "))

  for (p in parts) {
    if (p != "") {
      json <- parse_part(p, json, zero_index)
    }
  }

  json
}


node_test <- function(json, path) {
  # path something like $..field(?[index])
  # normalise path by recursive descent
  first_field <- sub("\\$?\\.{2}([[:alpha:]]*).*", "\\1", path)
  # message("finding ", first_field)
  new_path <- sub(paste0("\\$?\\.{2}", first_field), "", path)

  while (TRUE) {
    if (first_field %in% names(json)) {
      return(list(
        json = json[[first_field]],
        "path" = new_path)
      )
    } else {
      new_json <- purrr::flatten(json)
      if (!is_nested(new_json) && setequal(names(new_json), names(json))) {
        stop("field ", first_field, "not found")
      }
      json <- new_json
    }
  }
}

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
  message("processing ", normed_path)
  results <- process_piece(normed_path, json, "")

  if (simplify) {
    if (!is_nested(results)) {
      results <- unlist(results)
    }
  }

  results
}

#

#
