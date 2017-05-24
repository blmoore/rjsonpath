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

# if json object is an array or has named objects,
# apply fn to each item
walk_tree <- function(piece, remaining, json, processed) {
  downstream <- consume_part(remaining)
  next_piece <- downstream[[1]]
  todo <- downstream[[2]]

  if (next_piece %in% names(json)) {
    out_json <- json[[piece]]
  } else {
    # assume array
    array_names <- unique(unlist(lapply(json, names)))
    if (next_piece %in% array_names) {
      out_json <- lapply(json, `[[`, piece)
    } else {
      stop(piece, " not found in json")
    }
  }

  process_piece(todo, out_json, paste(processed, piece, next_piece, sep=";"))
}

# apply a python-style [start:end:step] index
slice <- function(piece, remaining, array, processed) {
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
process_piece <- function(jsonpath, json, processed) {

  # print(jsonpath)
  message("current path: ", jsonpath, "\n",
    "processed: ", processed)

  if (jsonpath != "") {
    # split chunk off path
    bits <- consume_part(jsonpath)
    this_piece <- bits[[1]]
    todo <- bits[[2]]

    if (this_piece != "$") {
      # process piece against json
      if (this_piece == "..") {
        message("walking: ", this_piece)
        json <- walk_tree(this_piece, todo, json, processed)
      } else {
        message("processing: ", todo)
        json <- process_piece(todo, json, this_piece)
      }
    } else {
      json <- process_piece(todo, json, this_piece)
    }
    return(json)
  }
}

