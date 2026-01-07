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

# Evaluate a filter expression against an array element
# Filter expressions use ?() syntax, e.g., ?(@.price<10)
evaluate_filter <- function(filter_expr, element) {
  # Remove ?() wrapper
  expr <- sub("^\\?\\((.*)\\)$", "\\1", filter_expr)
  
  # Handle logical operators FIRST, before individual comparisons
  # This ensures && and || are processed correctly
  # Handle negation first
  if (grepl("^!\\s*", expr)) {
    inner <- sub("^!\\s*", "", expr)
    return(!evaluate_filter(paste0("?(", inner, ")"), element))
  }
  
  # Handle && (must check before & and before individual comparisons)
  if (grepl("&&", expr)) {
    parts <- strsplit(expr, "\\s*&&\\s*")[[1]]
    parts <- trimws(parts)
    if (length(parts) == 0) return(FALSE)
    results <- vapply(parts, function(p) {
      p_clean <- trimws(p)
      if (grepl("^\\?", p_clean)) {
        evaluate_filter(p_clean, element)
      } else {
        evaluate_filter(paste0("?(", p_clean, ")"), element)
      }
    }, logical(1), USE.NAMES = FALSE)
    return(all(results))
  }
  
  # Handle || (must check before | and before individual comparisons)
  if (grepl("\\|\\|", expr)) {
    parts <- strsplit(expr, "\\s*\\|\\|\\s*")[[1]]
    parts <- trimws(parts)
    if (length(parts) == 0) return(FALSE)
    results <- vapply(parts, function(p) {
      p_clean <- trimws(p)
      if (grepl("^\\?", p_clean)) {
        evaluate_filter(p_clean, element)
      } else {
        evaluate_filter(paste0("?(", p_clean, ")"), element)
      }
    }, logical(1), USE.NAMES = FALSE)
    return(any(results))
  }
  
  # Now handle individual comparisons and property checks
  # Check for property existence: @.key
  if (grepl("^@\\.[a-zA-Z_][a-zA-Z0-9_]*$", expr)) {
    key <- sub("^@\\.", "", expr)
    return(key %in% names(element) && !is.null(element[[key]]))
  }
  
  # Handle comparisons: @.key < value, @.key > value, etc.
  # Match patterns like @.price<10, @.price==8.95, @.price!="value", @.category=='fiction'
  
  # Extract operator and operands for ==
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*==\\s*", expr)) {
    parts <- strsplit(expr, "\\s*==\\s*", perl = TRUE)[[1]]
    if (length(parts) != 2) return(FALSE)
    key <- sub("^@\\.", "", parts[1])
    key <- trimws(key)
    value_str <- parts[2]
    value_str <- trimws(value_str)
    # Remove quotes if present (both single and double)
    # Handle both "value" and \"value\" formats
    # First try removing regular quotes
    if (grepl("^[\"']", value_str) && grepl("[\"']$", value_str)) {
      value_str <- sub("^[\"']", "", value_str)
      value_str <- sub("[\"']$", "", value_str)
    }
    if (key %in% names(element)) {
      # Try numeric comparison first
      if (is_number(value_str)) {
        val_num <- as.numeric(value_str)
        elem_val <- element[[key]]
        if (is.null(elem_val)) return(FALSE)
        if (is.numeric(elem_val)) {
          # Use near-equality for floating point
          return(abs(elem_val - val_num) < 1e-10)
        }
        return(FALSE)
      } else {
        return(!is.null(element[[key]]) && as.character(element[[key]]) == value_str)
      }
    }
    return(FALSE)
  }
  
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*!=\\s*", expr)) {
    parts <- strsplit(expr, "\\s*!=\\s*")[[1]]
    key <- sub("^@\\.", "", parts[1])
    key <- trimws(key)
    value_str <- parts[2]
    value_str <- trimws(value_str)
    value_str <- sub("^['\"](.*)['\"]$", "\\1", value_str)
    if (key %in% names(element)) {
      elem_val <- element[[key]]
      if (is.null(elem_val)) return(TRUE)  # NULL != value is TRUE
      if (is_number(value_str)) {
        val_num <- as.numeric(value_str)
        if (is.numeric(elem_val)) {
          # Use near-inequality for floating point
          return(abs(elem_val - val_num) >= 1e-10)
        }
        return(TRUE)  # Type mismatch means !=
      } else {
        return(as.character(elem_val) != value_str)
      }
    }
    return(TRUE)  # Key not found means !=
  }
  
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*<=\\s*", expr)) {
    parts <- strsplit(expr, "\\s*<=\\s*")[[1]]
    key <- sub("^@\\.", "", parts[1])
    value_str <- parts[2]
    if (key %in% names(element) && is_number(value_str)) {
      return(!is.null(element[[key]]) && is.numeric(element[[key]]) && element[[key]] <= as.numeric(value_str))
    }
    return(FALSE)
  }
  
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*>=\\s*", expr)) {
    parts <- strsplit(expr, "\\s*>=\\s*")[[1]]
    key <- sub("^@\\.", "", parts[1])
    value_str <- parts[2]
    if (key %in% names(element) && is_number(value_str)) {
      return(!is.null(element[[key]]) && is.numeric(element[[key]]) && element[[key]] >= as.numeric(value_str))
    }
    return(FALSE)
  }
  
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*<\\s*", expr) && !grepl("&&|\\|\\|", expr)) {
    parts <- strsplit(expr, "\\s*<\\s*")[[1]]
    if (length(parts) != 2) return(FALSE)
    key <- sub("^@\\.", "", parts[1])
    value_str <- parts[2]
    if (key %in% names(element) && is_number(value_str)) {
      return(!is.null(element[[key]]) && is.numeric(element[[key]]) && element[[key]] < as.numeric(value_str))
    }
    return(FALSE)
  }
  
  if (grepl("@\\.[a-zA-Z_][a-zA-Z0-9_]*\\s*>\\s*", expr)) {
    parts <- strsplit(expr, "\\s*>\\s*")[[1]]
    key <- sub("^@\\.", "", parts[1])
    value_str <- parts[2]
    if (key %in% names(element) && is_number(value_str)) {
      return(!is.null(element[[key]]) && is.numeric(element[[key]]) && element[[key]] > as.numeric(value_str))
    }
    return(FALSE)
  }
  
  
  # Default: treat as property existence check
  key <- sub("^@\\.", "", expr)
  key <- sub("^@", "", key)
  if (nchar(key) > 0) {
    return(key %in% names(element))
  }
  
  FALSE
}

# Evaluate a script expression (non-filter expressions in brackets)
# Script expressions use () syntax, e.g., (@.length-1)
evaluate_script <- function(script_expr, array_length) {
  # Remove () wrapper
  expr <- sub("^\\((.*)\\)$", "\\1", script_expr)
  
  # Handle @.length patterns
  if (grepl("@\\.length", expr)) {
    # Replace @.length with actual length
    expr <- gsub("@\\.length", as.character(array_length), expr)
    # Evaluate simple arithmetic
    tryCatch({
      result <- eval(parse(text = expr))
      return(as.integer(result))
    }, error = function(e) {
      stop("Error evaluating script expression: ", script_expr)
    })
  }
  
  # If it's just a number, return it
  if (is_number(expr)) {
    return(as.integer(expr))
  }
  
  stop("Unsupported script expression: ", script_expr)
}
