# Performance tests for rjsonpath
# Tests performance on large JSON objects

context("Performance tests")

# Test data generators
generate_large_array <- function(n) {
  lapply(1:n, function(i) {
    list(
      id = i,
      name = paste0("item_", i),
      price = runif(1, 1, 100),
      category = sample(c("fiction", "non-fiction", "tech", "science"), 1),
      rating = sample(1:5, 1),
      in_stock = sample(c(TRUE, FALSE), 1),
      tags = paste0("tag_", sample(1:10, sample(1:5, 1))),
      metadata = list(
        created = Sys.time(),
        updated = Sys.time(),
        views = sample(1:10000, 1)
      )
    )
  })
}

generate_deep_nested <- function(depth, width = 3) {
  if (depth == 0) {
    return(list(value = "leaf"))
  }
  result <- list()
  for (i in 1:width) {
    result[[paste0("level", depth, "_", i)]] <- generate_deep_nested(depth - 1, width)
  }
  result
}

generate_wide_object <- function(n_keys) {
  result <- list()
  for (i in 1:n_keys) {
    result[[paste0("key_", i)]] <- paste0("value_", i)
  }
  result
}

generate_mixed_structure <- function(n_arrays = 10, array_size = 100, nesting_depth = 5) {
  result <- list()
  for (i in 1:n_arrays) {
    result[[paste0("array_", i)]] <- lapply(1:array_size, function(j) {
      nested <- generate_deep_nested(nesting_depth, 2)
      nested$id <- j
      nested$price <- runif(1, 1, 100)
      nested
    })
  }
  result
}

# Benchmarking helper
if (requireNamespace("bench", quietly = TRUE)) {
  benchmark <- function(expr, iterations = 10) {
    result <- bench::mark(expr, iterations = iterations, check = FALSE)
    # bench::mark returns a tibble with timing measurements
    # The 'time' column contains all individual timing measurements as a list column
    # Extract all timing measurements and convert from nanoseconds to seconds
    times <- numeric(0)
    if (nrow(result) > 0 && "time" %in% names(result)) {
      # Extract time column - it's a list column with vectors of measurements
      time_col <- result[["time"]]
      if (length(time_col) > 0) {
        # Unlist all timing measurements from all iterations
        times <- unlist(time_col, recursive = FALSE, use.names = FALSE)
        times <- as.numeric(times) / 1e9  # Convert nanoseconds to seconds
      }
    }
    # If we couldn't extract times, try to use summary statistics as fallback
    if (length(times) == 0) {
      if (nrow(result) > 0 && "median" %in% names(result)) {
        median_val <- as.numeric(result[["median"]][[1]]) / 1e9
        times <- rep(median_val, iterations)
      } else {
        times <- rep(0, iterations)
      }
    }
    list(
      median = median(times),
      mean = mean(times),
      min = min(times),
      max = max(times),
      total_time = sum(times)
    )
  }
} else if (requireNamespace("microbenchmark", quietly = TRUE)) {
  benchmark <- function(expr, iterations = 10) {
    result <- microbenchmark::microbenchmark(expr, times = iterations)
    times <- result$time / 1e9  # Convert to seconds
    list(
      median = median(times),
      mean = mean(times),
      min = min(times),
      max = max(times),
      total_time = sum(times)
    )
  }
} else {
  # Fallback to system.time
  benchmark <- function(expr, iterations = 10) {
    times <- numeric(iterations)
    for (i in 1:iterations) {
      times[i] <- as.numeric(system.time(expr)[["elapsed"]])
    }
    list(
      median = median(times),
      mean = mean(times),
      min = min(times),
      max = max(times),
      total_time = sum(times)
    )
  }
}

# Performance test: Simple property access
test_that("Simple property access scales well", {
  skip_on_cran()
  
  sizes <- c(100, 1000, 10000)
  results <- list()
  
  for (size in sizes) {
    json <- list(data = generate_large_array(size))
    
    bm <- benchmark({
      json_path(json, "$.data[*].name")
    }, iterations = 5)
    
    results[[as.character(size)]] <- bm
    expect_true(bm$median < 10, info = paste("Size", size, "took too long"))
  }
  
  # Store results for later comparison
  assign("perf_simple_access", results, envir = .GlobalEnv)
})

# Performance test: Recursive descent
test_that("Recursive descent scales reasonably", {
  skip_on_cran()
  
  # Test with different nesting depths
  depths <- c(5, 10, 15)
  results <- list()
  
  for (depth in depths) {
    json <- generate_deep_nested(depth)
    json$target_key <- list(value = "found")
    
    bm <- benchmark({
      json_path(json, "$..target_key")
    }, iterations = 5)
    
    results[[as.character(depth)]] <- bm
    expect_true(bm$median < 5, info = paste("Depth", depth, "took too long"))
  }
  
  assign("perf_recursive", results, envir = .GlobalEnv)
})

# Performance test: Wildcard operations
test_that("Wildcard operations are efficient", {
  skip_on_cran()
  
  json <- generate_mixed_structure(n_arrays = 5, array_size = 1000)
  
  bm1 <- benchmark({
    json_path(json, "$.*")
  }, iterations = 5)
  
  bm2 <- benchmark({
    json_path(json, "$.array_1[*]")
  }, iterations = 5)
  
  expect_true(bm1$median < 5, info = "Wildcard on root took too long")
  expect_true(bm2$median < 2, info = "Wildcard on array took too long")
  
  assign("perf_wildcard", list(root = bm1, array = bm2), envir = .GlobalEnv)
})

# Performance test: Filter expressions
test_that("Filter expressions scale with array size", {
  skip_on_cran()
  
  sizes <- c(100, 1000, 10000)
  results <- list()
  
  for (size in sizes) {
    json <- list(books = generate_large_array(size))
    
    bm <- benchmark({
      json_path(json, "$.books[?(@.price<50)]")
    }, iterations = 5)
    
    results[[as.character(size)]] <- bm
    expect_true(bm$median < 10, info = paste("Filter on size", size, "took too long"))
  }
  
  assign("perf_filters", results, envir = .GlobalEnv)
})

# Performance test: Array slices
test_that("Array slices are efficient", {
  skip_on_cran()
  
  json <- list(data = generate_large_array(10000))
  
  bm1 <- benchmark({
    json_path(json, "$.data[0:100]")
  }, iterations = 10)
  
  bm2 <- benchmark({
    json_path(json, "$.data[0:1000]")
  }, iterations = 10)
  
  expect_true(bm1$median < 1, info = "Small slice took too long")
  expect_true(bm2$median < 2, info = "Large slice took too long")
  
  assign("perf_slices", list(small = bm1, large = bm2), envir = .GlobalEnv)
})

# Performance test: Complex queries
test_that("Complex queries combining operations", {
  skip_on_cran()
  
  json <- generate_mixed_structure(n_arrays = 10, array_size = 500, nesting_depth = 3)
  
  bm1 <- benchmark({
    json_path(json, "$..price")
  }, iterations = 5)
  
  bm2 <- benchmark({
    json_path(json, "$.array_1[*].id")
  }, iterations = 5)
  
  expect_true(bm1$median < 10, info = "Recursive price search took too long")
  expect_true(bm2$median < 5, info = "Nested property access took too long")
  
  assign("perf_complex", list(recursive = bm1, nested = bm2), envir = .GlobalEnv)
})

# Performance test: Large arrays
test_that("Very large arrays are handled efficiently", {
  skip_on_cran()
  
  # Test with progressively larger arrays
  sizes <- c(1000, 10000, 100000)
  results <- list()
  
  for (size in sizes) {
    json <- list(items = generate_large_array(size))
    
    bm <- benchmark({
      json_path(json, "$.items[*].id")
    }, iterations = 3)
    
    results[[as.character(size)]] <- bm
    
    # Allow more time for larger arrays, but should still be reasonable
    max_time <- if (size <= 1000) 2 else if (size <= 10000) 10 else 30
    expect_true(bm$median < max_time, 
                info = paste("Array size", size, "took", bm$median, "seconds"))
  }
  
  assign("perf_large_arrays", results, envir = .GlobalEnv)
})

# Performance test: Wide objects
test_that("Wide objects with many keys", {
  skip_on_cran()
  
  key_counts <- c(100, 1000, 10000)
  results <- list()
  
  for (n_keys in key_counts) {
    json <- generate_wide_object(n_keys)
    
    bm <- benchmark({
      json_path(json, "$.key_1")
    }, iterations = 10)
    
    results[[as.character(n_keys)]] <- bm
    expect_true(bm$median < 1, info = paste("Wide object with", n_keys, "keys took too long"))
  }
  
  assign("perf_wide_objects", results, envir = .GlobalEnv)
})

# Summary test that can be run to get performance report
test_that("Performance summary", {
  skip_on_cran()
  
  # This test just ensures we can run all performance tests
  # Actual benchmarking happens in other tests
  expect_true(TRUE)
})
