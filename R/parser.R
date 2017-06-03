# traversal functions for jsonpath

# take a path and split into a vector of (piece, remaining)
consume_part <- function(path) {
  stringr::str_split_fixed(sub("^(.*?);(.*)$", "\\1 \\2", path), " ", 2)
}

add_pieces <- function(piece1, piece2, ...) {
  paste(piece1, piece2, ..., sep = ";")
}


get_piece <- function(json, piece) {

  if (is.null(json)) {
    stop("searching empty json object for: ", piece)
  }

  # numeric index of list
  if (is_number(piece)) {
    return(json[[as.numeric(piece)]])
  }

  # json subset
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
      has_names <- grepl(piece, lapply(json, names))
      if (!all(has_names)) {
        # select only sublists with name:
        this_json <- grepl(piece, lapply(json, names))
        out_json <- lapply(json[this_json], `[[`, piece)
      } else {
        out_json <- lapply(json, `[[`, piece)
      }
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

  # might be followed by an index or range
  jpath <- consume_part(jpath)[[1]]
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
slice <- function(piece, json) {
  index <- sub("\\[(.*?)\\].*", "\\1", piece)
  indices <- stringr::str_split_fixed(":", index, 2)
  range <- as.numeric(stringr::str_split_fixed(index, ":", 2))
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

# convert normalised 0-indexed path to 1-indexed
adjust_indices <- function(jsonpath) {
  path_parts <- stringr::str_split(jsonpath, ";", simplify = TRUE)
  numbered <- vapply(path_parts, rjsonpath:::is_number, logical(1))
  path_parts[numbered] <- as.numeric(path_parts[numbered]) + 1
  paste(path_parts, collapse = ";")
}

# dispatch on piece and process simple subsetting
# primary recursive function
process_piece <- function(jsonpath, json, processed) {

  message("\n --- new recursion --- ")
  message("current path: ", jsonpath, "\n",
    "processed: ", processed, "\n")
  if (is.null(json)) {
    stop("reached null json object after processing ", processed,
      " (remaining: ", jsonpath, ")")
  }

  if (jsonpath != "" & jsonpath != "*") {
    # split chunk off path
    bits <- consume_part(jsonpath)
    this_piece <- bits[[1]]
    todo <- bits[[2]]
    message("this: ", this_piece, "\ntodo: ", todo)

    if (this_piece != "$") {
      # process piece against json
      if (this_piece == "..") {
        # message("walking: ", this_piece)
        json <- Recall(consume_part(todo)[[2]], get_anywhere(todo, json),
          add_pieces(processed, this_piece, todo))
      } else {
        if (this_piece == "*") {
          json <- walk_tree(this_piece, todo, json, processed, recurse = TRUE)
        } else {
          if (grepl(":", this_piece)) {
            json <- slice(this_piece, json)
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

