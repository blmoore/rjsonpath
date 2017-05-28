# traversal functions for jsonpath

# core args to functions:
#      piece: current path part being processed
#  remaining: RHS of unprocessed jsonpath
#       json: current list object
#  processed: LHS of process jsonpath

# take a path and split into a vector of (piece, remaining)
consume_part <- function(path) {
  stringr::str_split_fixed(sub("^(.*?);(.*)$", "\\1 \\2", path), " ", 2)
}

add_pieces <- function(piece1, piece2, ...) {
  paste(piece1, piece2, ..., sep = ";")
}

get_piece <- function(json, piece) {

  if (piece %in% names(json)) {
    if (!anyDuplicated(names(json))) {
      out_json <- json[[piece]]
    } else {
      # TODO fix this output structure
      out_json <- json[names(json) == piece]
    }
  } else {
    array_names <- unique(unlist(lapply(json, names)))
    if (piece %in% array_names) {
      out_json <- lapply(json, `[[`, piece)
    } else {
      stop(piece, " not found in json")
    }
  }

  out_json
}

# walk + get all for part (..X)
get_anywhere <- function(jpath, json) {

  results <- vector("list", 1e5)
  i <- 1L
  nested <- is_nested(json)
  while (TRUE) {
    try({
      results[[i]] <- get_piece(json, jpath)
      if (class(search_result) == "list") {
        results[[i]] <- search_result
      }
    }, silent = TRUE)

    if (!nested) {
      break
    }

    json <- purrr::flatten(json)
    nested <- is_nested(json)
  }

  if (length(!is.null(results)) == 0) {
    stop("get_anywhere: no matching elements found for: ", jpath)
  }

  unname(results[[!is.null(results)]])
}

# if json object is an array or has named objects,
# apply fn to each item
walk_tree <- function(piece, remaining, json, processed, recurse = FALSE) {

  # print(paste0("walk_tree got piece: ", piece))
  downstream <- consume_part(remaining)
  next_piece <- downstream[[1]]
  todo <- downstream[[2]]

  if (next_piece %in% names(json)) {
    out_json <- json[[next_piece]]
  } else {
    # assume array
    array_names <- unique(unlist(lapply(json, names)))
    # print(paste(array_names, collapse = ", "))
    if (next_piece %in% array_names) {
      out_json <- lapply(json, `[[`, next_piece)
    } else {
      stop("walk_tree: ", next_piece, " not found in json")
    }
  }

  process_piece(todo, out_json, add_pieces(processed, piece, next_piece))
}

# apply a python-style [start:end:step] index
slice <- function(piece, json, zero_index=TRUE) {
  index <- sub("\\[(.*?)\\].*", "\\1", piece)
  indices <- stringr::str_split_fixed(":", index, 2)
  range <- as.numeric(stringr::str_split_fixed(index, ":", 2))
  range <- if (zero_index) range + 1 else range
  `[`(json, seq(range[[1]], range[[2]]))
}

# reformat jsonpath with delimiters, allowing quoted names
#' @importFrom magrittr "%>%"
format_path <- function(jsonpath) {
#jsonpath = "$.store.book[*].author"
  normed_path <- jsonpath %>%
    # $.x -> $;..;x
    gsub("\\.{2}", ";..;", .) %>%
    # lookbehind: . or @, lookahead for .
    stringr::str_replace_all("(?<![\\.@])\\.(?!\\.)", ";") %>%
    # expression separators -> ;
    gsub("\\[(.*?)\\]\\.?", ";\\1", .) %>%
    # from here on, just tidying up
    sub(";$", "", .)

  normed_path
}

# dispatch on piece and process simple subsetting
# primary recursive function
process_piece <- function(jsonpath, json, processed, zero_index=TRUE) {

  # message("current path: ", jsonpath, "\n",
  #   "processed: ", processed, "\n",
  #   "json: ", str(json))

  if (jsonpath != "" & jsonpath != "*") {
    # split chunk off path
    bits <- consume_part(jsonpath)
    this_piece <- bits[[1]]
    todo <- bits[[2]]

    if (this_piece != "$") {
      # process piece against json
      if (this_piece == "..") {
        # message("walking: ", this_piece)
        return(get_anywhere(todo, json))
      } else {
        if (this_piece == "*") {
          json <- walk_tree(this_piece, todo, json, processed, recurse=TRUE)
        } else {
          if (grepl(":", this_piece)) {
            json <- slice(this_piece, json, zero_index = zero_index)
            #stop("range not implemented")
          } else {
            # piece label
            json <- Recall(todo, get_piece(json, this_piece),
              add_pieces(processed, this_piece))
          }
        }
      }
    } else {
      # skip first piece and process
      json <- Recall(todo, json, add_pieces(processed, this_piece))
    }
  }
  json
}

