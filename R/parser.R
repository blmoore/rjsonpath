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
    out_json <- json[[piece]]
  } else {
    array_names <- unique(unlist(lapply(json, names)))
    if (piece %in% array_names) {
      out_json <- lapply(json, `[[`, piece)
    } else {
      #stop(piece, " not found in json")
      return(FALSE)
    }
  }
  out_json
}

get_anywhere <- function(jpath, json) {

  stop("$..<x> search not implemented")
  search_part <- gsub("^$;\\.\\.;(.*?);", "\\1", jpath)

  results <- list()
  search_result <- get_piece(json, search_part)
  if (search_result) {
    results <- c(results, search_result)
  }

  if (!any(grepl(search, names))) {
    stop("no matching elements found for: ", search_part)
  }

  has_part <- grepl(search, names)
  paths <- unique(sub(paste0("^(.*?", search, ")"), "\\1", names[has_part]))

  results <- list()
  for (p in paths) {
    pieces <- strsplit(p, "\\.")

  }
}

# if json object is an array or has named objects,
# apply fn to each item
walk_tree <- function(piece, remaining, json, processed, recurse = FALSE) {
  downstream <- consume_part(remaining)
  next_piece <- downstream[[1]]
  todo <- downstream[[2]]

  if (next_piece %in% names(json)) {
    out_json <- json[[piece]]
  } else {
    # assume array
    array_names <- unique(unlist(lapply(json, names)))
    if (next_piece %in% array_names) {
      out_json <- lapply(json, `[[`, next_piece)
    } else {
      stop(piece, " not found in json")
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
        json <- walk_tree(this_piece, todo, json, processed)
      } else {
        if (this_piece == "*") {
          json <- walk_tree(this_piece, todo, json, processed, recurse=TRUE)
        } else {
          if (grepl(":", this_piece)) {
            json <- slice(this_piece, json, zero_index = zero_index)
            #stop("range not implemented")
          } else {
            # message("processing: ", todo)
            json <- Recall(todo, get_piece(json, this_piece),
              add_pieces(processed, this_piece))
          }
        }
      }
    } else {
      # process piece
      json <- Recall(todo, json, add_pieces(processed, this_piece))
    }
  }
  json
}

