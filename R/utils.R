# utility funs

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

is_number <- function(string) {
  suppressWarnings(!any(is.na(as.numeric(string))))
}
