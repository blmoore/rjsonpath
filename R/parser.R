# traversal functions for jsonpath

# take a path and split into a vector of (piece, remaining)
consume_part <- function(path) {
  pos <- regexpr(";", path, fixed = TRUE)
  if (pos == -1) {
    return(c(path, ""))
  }
  piece <- substr(path, 1L, pos - 1L)
  remaining <- substr(path, pos + 1L, nchar(path))
  c(piece, remaining)
}

add_pieces <- function(piece1, piece2, ...) {
  paste(piece1, piece2, ..., sep = ";")
}


get_piece <- function(json, piece, zero_index = TRUE) {

  if (is.null(json)) {
    stop("searching empty json object for: ", piece)
  }

  piece_clean <- sub("^['\"](.*)['\"]$", "\\1", piece)

  if (grepl("^\\?", piece)) {
    return(NULL)
  }

  if (grepl("^\\(", piece) && grepl("@\\.length", piece)) {
    return(NULL)
  }

  if (grepl("^-?[0-9]+$", piece_clean)) {
    idx <- as.numeric(piece_clean)
    if (idx < 0) {
      idx <- length(json) + idx + 1
    }
    if (idx < 1 || idx > length(json)) {
      return(NULL)
    }
    return(json[[idx]])
  }
  
  piece <- piece_clean

  # json subset
  if (piece %in% names(json)) {
    if (!anyDuplicated(names(json))) {
      out_json <- json[[piece]]
    } else {
      out_json <- json[names(json) == piece]
      if (length(out_json) == 1) {
        out_json <- out_json[[1]]
      }
    }
    return(out_json)
  }
  
  if (is.list(json) && length(json) > 0 && !is.null(names(json[[1]]))) {
    # Optimize: avoid unique(unlist(...)) by checking directly
    # Process array elements more efficiently
    json_len <- length(json)
    out_json <- vector("list", json_len)
    out_count <- 0L
    
    for (i in seq_len(json_len)) {
      if (piece %in% names(json[[i]])) {
        out_count <- out_count + 1L
        out_json[[out_count]] <- json[[i]][[piece]]
      }
    }
    
    if (out_count > 0L) {
      # Trim to actual length and return
      if (out_count == 1L) {
        return(out_json[[1L]])
      }
      return(out_json[1:out_count])
    }
  }
  
  # If we get here, key not found
  stop(piece, " not found in json")
}

get_anywhere <- function(jpath, json) {
  jpath_parts <- consume_part(jpath)
  search_key <- jpath_parts[[1]]

  # Use a closure with a list to collect results more efficiently
  # This avoids repeated c() operations which are expensive
  matches <- list()
  
  search_recursive <- function(obj, key) {
    if (!is.list(obj)) {
      return(invisible(NULL))
    }
    
    # Check if current object has the key
    if (!is.null(names(obj)) && key %in% names(obj)) {
      matches <<- c(matches, list(obj[[key]]))
    }

    # Recursively search children
    if (is.list(obj) && length(obj) > 0) {
      if (is.null(names(obj))) {
        # It's an array
        for (i in seq_along(obj)) {
          if (is.list(obj[[i]])) {
            # Check if array element has the key directly
            if (!is.null(names(obj[[i]])) && key %in% names(obj[[i]])) {
              matches <<- c(matches, list(obj[[i]][[key]]))
            }
            # Recursively search the element
            search_recursive(obj[[i]], key)
          }
        }
      } else {
        # It's an object, search all values
        for (i in seq_along(obj)) {
          if (is.list(obj[[i]])) {
            search_recursive(obj[[i]], key)
          }
        }
      }
    }
    
    invisible(NULL)
  }
  
  search_recursive(json, search_key)

  if (length(matches) == 0) {
    return(list())
  }

  matches
}


walk_tree <- function(piece, remaining, json, processed, recurse = FALSE) {
  downstream <- consume_part(remaining)
  next_piece <- downstream[[1]]
  todo <- downstream[[2]]

  if (next_piece %in% names(json)) {
    out_json <- json[[next_piece]]
  } else {
    # assume array - optimize by checking directly instead of unique(unlist(...))
    json_len <- length(json)
    found <- FALSE
    if (json_len > 0 && !is.null(names(json[[1]]))) {
      # Quick check if key exists in any element
      for (i in seq_len(json_len)) {
        if (next_piece %in% names(json[[i]])) {
          found <- TRUE
          break
        }
      }
      if (found) {
        out_json <- lapply(json, function(x) {
          if (next_piece %in% names(x)) x[[next_piece]] else NULL
        })
        # Remove NULLs
        out_json <- out_json[!vapply(out_json, is.null, logical(1))]
      } else {
        stop("walk_tree: ", next_piece, " not found in json")
      }
    } else {
      stop("walk_tree: ", next_piece, " not found in json")
    }
  }

  process_piece(todo, out_json, add_pieces(processed, piece, next_piece))
}

slice <- function(piece, json, zero_index = TRUE) {
  index_spec <- piece
  if (grepl("\\[", piece)) {
    index_spec <- sub("^.*?\\[(.*?)\\].*$", "\\1", piece)
  }

  if (!is.list(json)) {
    was_vector <- !is.list(json)
    json_list <- as.list(json)
    result <- slice(piece, json_list, zero_index)
    if (was_vector && is.list(result)) {
      return(unlist(result))
    }
    return(result)
  }

  if (grepl(",", index_spec)) {
    indices <- strsplit(index_spec, ",")[[1]]
    indices <- trimws(indices)
    indices_numeric <- suppressWarnings(as.numeric(indices))
    indices_numeric <- indices_numeric[!is.na(indices_numeric)]
    indices_numeric <- ifelse(indices_numeric < 0, 
                              length(json) + indices_numeric + 1, 
                              indices_numeric)
    valid_indices <- indices_numeric[indices_numeric >= 1 & indices_numeric <= length(json)]
    if (length(valid_indices) == 0) {
      return(list())
    }
    return(json[valid_indices])
  }
  
  parts <- strsplit(index_spec, ":")[[1]]
  len <- length(json)
  if (parts[1] == "") {
    start <- 1
  } else {
    start <- as.numeric(parts[1])
    if (is.na(start)) start <- 1
    if (start < 0) {
      start <- len + start + 1
    }
  }
  
  end_provided <- length(parts) >= 2 && parts[2] != ""
  
  if (!end_provided) {
    end <- len
  } else {
    end <- as.numeric(parts[2])
    if (is.na(end)) {
      end <- len
      end_provided <- FALSE
    } else {
      if (end < 0) {
        end <- len + end + 1
        end <- end - 1
      } else {
        if (end - start > 1) {
          end <- end - 1
        }
      }
    }
  }

  step <- if (length(parts) < 3 || parts[3] == "") 1 else as.numeric(parts[3])
  if (is.na(step) || step == 0) step <- 1

  start <- max(1, min(start, len + 1))
  end <- max(0, min(end, len))
  
  if (start > end) {
    return(list())
  }

  if (step == 1) {
    seq_indices <- seq(start, end)
  } else if (step > 0) {
    # Generate sequence with step
    seq_indices <- seq(start, end, by = step)
    # Only keep indices within valid range
    seq_indices <- seq_indices[seq_indices >= start & seq_indices <= end]
  } else {
    # Negative step (reverse order) - swap start and end, use positive step
    seq_indices <- seq(end, start, by = abs(step))
  }
  
  # Filter valid indices
  seq_indices <- seq_indices[seq_indices >= 1 & seq_indices <= len]
  
  if (length(seq_indices) == 0) {
    return(list())
  }
  
  json[seq_indices]
}

#' @importFrom magrittr "%>%"
format_path <- function(jsonpath) {
  protected <- jsonpath
  path_len <- nchar(protected)

  bracket_matches <- gregexpr("\\[([^\\[\\]]*)\\]", protected, perl = TRUE)
  match_starts <- bracket_matches[[1]]
  match_lengths <- attr(bracket_matches[[1]], "match.length")
  
  if (length(match_starts) > 0 && match_starts[1] != -1) {
    placeholders <- character(length(match_starts))
    # Store bracket contents and replace with placeholders
    # Work backwards to preserve positions
    for (i in rev(seq_along(match_starts))) {
      start_pos <- match_starts[i]
      match_len <- match_lengths[i]
      content <- substr(protected, start_pos + 1, start_pos + match_len - 2)
      placeholder <- paste0("___BRACKET", i, "___")
      placeholders[i] <- content
      # Replace bracket content with placeholder
      # Update path_len after each replacement
      protected <- paste0(
        substr(protected, 1, start_pos),
        "[", placeholder, "]",
        substr(protected, start_pos + match_len, nchar(protected))
      )
      path_len <- nchar(protected)
    }
    
    # Apply transformations
    normed_path <- protected
    normed_path <- gsub("\\.{2}", ";..;", normed_path, fixed = FALSE)
    normed_path <- stringr::str_replace_all(normed_path, "(?<![\\.@])\\.(?!\\.)", ";")
    # Replace brackets with semicolons (use fixed=TRUE with literal "[")
    normed_path <- gsub("[", ";", normed_path, fixed = TRUE)
    normed_path <- gsub("]", "", normed_path, fixed = TRUE)
    
    # Restore placeholders
    for (i in seq_along(placeholders)) {
      normed_path <- gsub(paste0("___BRACKET", i, "___"), placeholders[i], normed_path, fixed = TRUE)
    }
  } else {
    normed_path <- protected
    normed_path <- gsub("\\.{2}", ";..;", normed_path, fixed = FALSE)
    normed_path <- stringr::str_replace_all(normed_path, "(?<![\\.@])\\.(?!\\.)", ";")
  }
  
  # Clean up separators
  normed_path <- gsub(";+", ";", normed_path, fixed = FALSE)
  normed_path <- sub(";$", "", normed_path, fixed = FALSE)
  normed_path <- sub("^;", "", normed_path, fixed = FALSE)
  
  normed_path
}

adjust_indices <- function(jsonpath) {
  path_parts <- stringr::str_split(jsonpath, ";", simplify = TRUE)
  
  adjust_part <- function(part) {
    if (is_number(part)) {
      num_val <- as.numeric(part)
      if (num_val >= 0) {
        return(as.character(num_val + 1))
      }
      return(part)
    }
    
    if (grepl(":", part) && !grepl("@", part)) {
      parts <- strsplit(part, ":", fixed = FALSE)[[1]]
      if (grepl(":$", part)) {
        parts <- c(parts, "")
      }
      adjusted_parts <- vapply(seq_along(parts), function(i) {
        p <- parts[i]
        p_trim <- trimws(p)
        if (i == 3 && p_trim != "" && is_number(p_trim)) {
          return(p)
        }
        if (p_trim != "" && is_number(p_trim)) {
          num_val <- as.numeric(p_trim)
          if (num_val >= 0) {
            return(as.character(num_val + 1))
          }
        }
        return(p)
      }, character(1))
      result <- paste(adjusted_parts, collapse = ":")
      return(result)
    }
    
    if (grepl(",", part) && !grepl("@|\\?", part)) {
      parts <- strsplit(part, ",")[[1]]
      adjusted_parts <- vapply(parts, function(p) {
        p_trim <- trimws(p)
        if (p_trim != "" && is_number(p_trim)) {
          num_val <- as.numeric(p_trim)
          # Don't adjust negative indices
          if (num_val >= 0) {
            return(as.character(num_val + 1))
          }
        }
        return(p)
      }, character(1))
      return(paste(adjusted_parts, collapse = ","))
    }
    
    return(part)
  }
  
  adjusted_parts <- vapply(path_parts, adjust_part, character(1))
  paste(adjusted_parts, collapse = ";")
}

process_piece <- function(jsonpath, json, processed, zero_index = TRUE) {

  if (is.null(json)) {
    stop("reached null json object after processing ", processed,
      " (remaining: ", jsonpath, ")")
  }

  # Base case: empty path means we're done
  if (jsonpath == "" || jsonpath == "*") {
    return(json)
  }

  # Split path into current piece and remainder
  bits <- consume_part(jsonpath)
  this_piece <- bits[[1]]
  todo <- bits[[2]]

  # Handle root selector
  if (this_piece == "$") {
    return(Recall(todo, json, add_pieces(processed, this_piece), zero_index))
  }

  # Handle recursive descent
  if (this_piece == "..") {
    # Get the key to search for and any remaining path
    remaining_parts <- consume_part(todo)
    search_key <- remaining_parts[[1]]
    remaining_path <- if (length(remaining_parts) > 1 && remaining_parts[[2]] != "") remaining_parts[[2]] else ""
    
    # Special case: ..* means recursively get all elements
    if (search_key == "*") {
      # Recursively collect all values using a closure for efficiency
      all_results <- list()
      
      collect_all <- function(obj) {
        if (is.list(obj)) {
          if (!is.null(names(obj))) {
            # It's an object, add all values
            all_results <<- c(all_results, as.list(obj))
            # Recursively collect from each value
            for (val in obj) {
              if (is.list(val)) {
                collect_all(val)
              }
            }
          } else {
            # It's an array, recursively collect from each element
            for (item in obj) {
              if (is.list(item)) {
                collect_all(item)
              } else {
                all_results <<- c(all_results, list(item))
              }
            }
          }
        } else {
          all_results <<- c(all_results, list(obj))
        }
        invisible(NULL)
      }
      collect_all(json)
      results <- all_results
      if (remaining_path != "") {
        processed_results <- lapply(results, function(result) {
          process_piece(remaining_path, result, add_pieces(processed, this_piece, "*"), zero_index)
        })
        return(unlist(processed_results, recursive = FALSE))
      }
      return(results)
    }
    
    # Find all matches recursively - search for just the key
    results <- get_anywhere(search_key, json)
    
    # If there's more path to process, apply it to each result
    if (remaining_path != "") {
      # Process each result with the remaining path
      processed_results <- lapply(results, function(result) {
        # If result is a list with a single array element and remaining_path looks like an index/slice,
        # we might want to apply it directly to the array
        process_piece(remaining_path, result, add_pieces(processed, this_piece, search_key), zero_index)
      })
      # Handle single result from slice/index - unwrap if it's a single-element list
      if (length(processed_results) == 1) {
        single <- processed_results[[1]]
        # If it's a list with one element (from a slice/index), unwrap it
        if (is.list(single) && length(single) == 1) {
          # Check if remaining_path was a slice/index
          if (grepl("[:,]", remaining_path) || grepl("^-?[0-9]+$", remaining_path) ||
              grepl("^\\(", remaining_path)) {
            # It's from a slice/index, unwrap the single element
            return(single[[1]])
          }
        }
        return(single)
      }
      # Flatten if needed, but preserve list structure for arrays
      return(unlist(processed_results, recursive = FALSE))
    }
    
    # If we got a single result from recursive descent, unwrap it
    if (length(results) == 1) {
      return(results[[1]])
    }
    
    return(results)
  }

  # Handle wildcard
  if (this_piece == "*") {
    if (is.list(json) && length(json) > 0) {
      if (!is.null(names(json))) {
        # It's an object, return all values
        if (todo == "") {
          return(json)
        }
        # Process further with each value
        results <- lapply(names(json), function(key) {
          process_piece(todo, json[[key]], add_pieces(processed, this_piece, key), zero_index)
        })
        return(unlist(results, recursive = FALSE))
      } else {
        # It's an array, process each element
        if (todo == "") {
          return(json)
        }
        
        # Check if the remaining path has a slice/index after property access
        # We need to detect patterns like "prop;slice" where slice is [2:3] or similar
        next_bits <- consume_part(todo)
        next_piece <- next_bits[[1]]
        remaining_after_next <- next_bits[[2]]
        
        # Check if remaining_after_next (the part after next_piece) is a slice/index
        is_slice_after <- FALSE
        if (remaining_after_next != "") {
          next_next_bits <- consume_part(remaining_after_next)
          next_next_piece <- next_next_bits[[1]]
          if (grepl("[:,]", next_next_piece) || grepl("^-?[0-9]+$", next_next_piece)) {
            is_slice_after <- TRUE
          }
        }
        
        # If there's a slice after property access, combine results first
        if (is_slice_after && next_piece != "" && !grepl("[:,]|^-?[0-9]", next_piece)) {
          # Process property on all items, combine, then slice
          combined_results <- lapply(json, function(item) {
            process_piece(next_piece, item, add_pieces(processed, this_piece), zero_index)
          })
          # Unlist to combine all results into one array/vector
          combined <- unlist(combined_results, recursive = FALSE)
          # Now apply the slice/index to the combined array
          return(process_piece(remaining_after_next, combined, 
                               add_pieces(processed, this_piece, next_piece), zero_index))
        }
        
        # Normal processing - apply to each element
        results <- lapply(json, function(item) {
          process_piece(todo, item, add_pieces(processed, this_piece), zero_index)
        })
        return(unlist(results, recursive = FALSE))
      }
    }
    return(json)
  }

  # Handle filter expressions ?(...)
  if (grepl("^\\?", this_piece)) {
    if (!is.list(json) || length(json) == 0) {
      return(list())
    }
    # Filters only work on arrays (unnamed lists), not objects
    if (!is.null(names(json)) && length(names(json)) == length(json)) {
      # It's an object, not an array - can't filter
      return(list())
    }
    # Apply filter to array elements
    filter_expr <- this_piece
    filtered <- Filter(function(x) {
      evaluate_filter(filter_expr, x)
    }, json)
    
    if (length(filtered) == 0) {
      return(list())
    }
    
    if (todo != "") {
      # Continue processing with filtered results
      results <- lapply(filtered, function(item) {
        process_piece(todo, item, add_pieces(processed, this_piece), zero_index)
      })
      return(unlist(results, recursive = FALSE))
    }
    return(filtered)
  }

  # Handle script expressions (@.length-N)
  # These are evaluated at runtime, so they haven't been adjusted yet
  if (grepl("^\\(", this_piece) && grepl("@\\.length", this_piece)) {
    if (!is.list(json) || length(json) == 0) {
      return(NULL)
    }
    # Script expressions only work on arrays (unnamed lists)
    if (!is.null(names(json)) && length(names(json)) == length(json)) {
      # It's an object, not an array
      return(NULL)
    }
    idx <- evaluate_script(this_piece, length(json))
    # Script expressions return 0-indexed results when zero_index is TRUE
    # So we need to convert to R's 1-indexing
    if (zero_index && idx >= 0) {
      idx <- idx + 1
    }
    # Handle negative indices (relative to end)
    if (idx < 0) {
      idx <- length(json) + idx + 1
    }
    if (idx < 1 || idx > length(json)) {
      return(NULL)
    }
    selected <- json[[idx]]
    if (todo != "") {
      return(Recall(todo, selected, add_pieces(processed, this_piece), zero_index))
    }
    return(selected)
  }

  # Handle numeric indices (including negative) - check before slice operations
  # This handles cases like [0], [1], [-1] etc.
  if (grepl("^-?[0-9]+$", this_piece)) {
    idx <- as.numeric(this_piece)
    # Handle negative indices
    if (idx < 0) {
      if (!is.list(json) || length(json) == 0 || (!is.null(names(json)) && length(names(json)) == length(json))) {
        return(NULL)
      }
      idx <- length(json) + idx + 1
    }
    # Note: if zero_index was TRUE and idx >= 0, indices were already adjusted
    if (idx < 1 || idx > length(json)) {
      return(NULL)
    }
    selected <- json[[idx]]
    if (todo != "") {
      return(Recall(todo, selected, add_pieces(processed, this_piece), zero_index))
    }
    return(selected)
  }

  # Handle slice/union operations (contains : or ,)
  if (grepl("[:,]", this_piece)) {
    sliced <- slice(this_piece, json, zero_index)
    if (todo != "") {
      # For slices, we might get multiple items - process each
      if (is.list(sliced) && length(sliced) > 0) {
        results <- lapply(sliced, function(item) {
          process_piece(todo, item, add_pieces(processed, this_piece), zero_index)
        })
        return(unlist(results, recursive = FALSE))
      }
      return(Recall(todo, sliced, add_pieces(processed, this_piece), zero_index))
    }
    return(sliced)
  }

  # Handle regular property/key access
  tryCatch({
    next_json <- get_piece(json, this_piece, zero_index)
    if (todo != "") {
      return(Recall(todo, next_json, add_pieces(processed, this_piece), zero_index))
    }
    return(next_json)
  }, error = function(e) {
    # If key not found and we're in an array context, try array-style access
    if (is.list(json) && length(json) > 0 && is.null(names(json))) {
      # It's an array, try to access property on each element
      results <- lapply(json, function(item) {
        if (is.list(item) && this_piece %in% names(item)) {
          if (todo != "") {
            return(process_piece(todo, item[[this_piece]], add_pieces(processed, this_piece), zero_index))
          }
          return(item[[this_piece]])
        }
        return(NULL)
      })
      results <- results[!vapply(results, is.null, logical(1))]
      if (length(results) == 0) {
        stop(this_piece, " not found in json")
      }
      return(unlist(results, recursive = FALSE))
    }
    stop(e$message)
  })
}

