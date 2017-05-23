# traversal functions for jsonpath

# core args to functions:
#      piece: current path part being processed
#  remaining: RHS of unprocessed jsonpath
#       json: current list object
#  processed: LHS of process jsonpath

# if json object is an array or has named objects,
# apply fn to each item
walk_tree <- function(piece, remaining, json, processed, fn) {
}

# apply a python-style [start:end:step] index
slice <- function(piece, remaining, array, processed) {
}

# reformat jsonpath with delimiters, allowing quoted names
#' @importFrom magrittr "%>%"
format_path <- function(jsonpath) {
#jsonpath = "$.store.book[*].author"
  normed_path <-
    jsonpath %>%
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
  if (jsonpath != "") {
    # split chunk off path
  } else {
    return(json)
  }
}
