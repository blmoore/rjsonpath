field_check <- function(json, field) {

  if (is.null(names(json))) {
    stop("attempted to index array by name, use [*] for all items")
  }

  if (!field %in% names(json)) {
    stop(field, " not found in json (",
      paste(names(json), collapse = ","))
  }
}

# recurse through each part with each json subset
# index needs to associate with RHS:
# x[*].y means get y from all x
parse_part <- function(part, json, zero_index) {

  if (grepl("^\\[", part)) {
    # indexing a list

    subparts <- stringr::str_split_fixed(substr(part, 2, nchar(part)),
      "\\[", 2)
    index <- subparts[[1]]
    field <- subparts[[2]]

    # range subset
    if (grepl(":", index)) {
      indices <- stringr::str_split_fixed(":", index, 2)
    } else {
      # * or index
      if (index == "*") {
        this_json <- lapply(json, `[[`, field_id)
      } else {
        index <- if (zero_index) index + 1 else index
        this_json <- json[[index]]
      }
    }

  } else {
    # requesting a single named object

    field_id <- part

    if (grepl("\\]$", field_id)) {
      # single index on previous field
      field_id <- sub("\\]", "", field_id)
      simple_index <- suppressWarnings(as.numeric(field_id))
      if (!is.na(simple_index)) {
        simple_index <- if (zero_index) simple_index + 1 else simple_index
        return(`[[`(json, simple_index))
      } else {
        if (grepl(":", field_id)) {
          # range using seq
          range <- as.numeric(stringr::str_split_fixed(field_id, ":", 2))
          range <- if (zero_index) range + 1 else range
          # print(range)
          return(`[`(json, seq(range[[1]], range[[2]])))
        } else {
          stop("not yet implemented")
        }
      }
    }

    # print(paste0("Field: ", field_id))
    field_check(json, field_id)

    this_json <- `[[`(json, field_id)

  }

  this_json
}

parse_jpath <- function(json, path, zero_index = TRUE) {

  # negative lookahead: split on '.' but not '].'
  parts <- stringr::str_split(path, "(?<!\\])\\.", simplify = TRUE)[-1]
  parts <- unlist(strsplit(parts, "\\["))
  parts <- gsub("^(.*?)\\]\\.", "[\\1]", parts)

  this_json <- json
  for (p in parts) {
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


# json <- read_json("tests/testthat/bookstore.json")
# books <- json_path(json, "$.store.book[0]")
