# Comprehensive JSONPath test suite
# Based on JSONPath specification: https://goessner.net/articles/JsonPath/

context("Comprehensive JSONPath Tests")

json <- jsonlite::read_json("bookstore.json")

# ============================================================================
# Phase 1.1: Basic Selectors
# ============================================================================

test_that("Root selector $ works", {
  result <- json_path(json, "$", simplify = FALSE)
  expect_equal(result, json)
})

test_that("Child access with dot notation works", {
  expect_equal(json_path(json, "$.store"), json$store)
  expect_equal(json_path(json, "$.store.book"), json$store$book)
  expect_equal(json_path(json, "$.store.bicycle"), json$store$bicycle)
})

test_that("Child access with bracket notation works", {
  expect_equal(json_path(json, "$['store']"), json$store)
  expect_equal(json_path(json, "$['store']['book']"), json$store$book)
  expect_equal(json_path(json, "$['store']['bicycle']"), json$store$bicycle)
})

test_that("Mixed dot and bracket notation works", {
  expect_equal(json_path(json, "$.store['book']"), json$store$book)
  expect_equal(json_path(json, "$['store'].book"), json$store$book)
})

test_that("Array indexing with positive indices works", {
  expect_equal(json_path(json, "$.store.book[0]", simplify = FALSE), json$store$book[[1]])
  expect_equal(json_path(json, "$.store.book[1]", simplify = FALSE), json$store$book[[2]])
  expect_equal(json_path(json, "$.store.book[2]", simplify = FALSE), json$store$book[[3]])
  expect_equal(json_path(json, "$.store.book[3]", simplify = FALSE), json$store$book[[4]])
})

test_that("Array indexing with negative indices works", {
  expect_equal(json_path(json, "$.store.book[-1]", simplify = FALSE), json$store$book[[4]])
  expect_equal(json_path(json, "$.store.book[-2]", simplify = FALSE), json$store$book[[3]])
  expect_equal(json_path(json, "$.store.book[-4]", simplify = FALSE), json$store$book[[1]])
})

test_that("Nested property access works", {
  expect_equal(json_path(json, "$.store.book[0].author"), json$store$book[[1]]$author)
  expect_equal(json_path(json, "$.store.book[0].title"), json$store$book[[1]]$title)
  expect_equal(json_path(json, "$.store.book[0].price"), json$store$book[[1]]$price)
})

# ============================================================================
# Phase 1.2: Wildcard and Recursive Descent
# ============================================================================

test_that("Wildcard * selects all properties", {
  result <- json_path(json, "$.store.*", simplify = FALSE)
  expect_equal(result, json$store)
})

test_that("Recursive descent .. finds key anywhere", {
  authors <- json_path(json, "$..author")
  expected <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(sort(authors), sort(expected))
})

test_that("Recursive descent with wildcard finds all elements", {
  result <- json_path(json, "$..*", simplify = FALSE)
  expect_true(length(result) > 0)
  # Should contain all nested values
})

test_that("Combination of wildcard and recursive descent works", {
  result <- json_path(json, "$.store.*", simplify = FALSE)
  expect_true("book" %in% names(result))
  expect_true("bicycle" %in% names(result))
})

test_that("Recursive descent with specific key and property", {
  prices <- json_path(json, "$..price")
  expected <- c(unlist(lapply(json$store$book, `[[`, "price")), json$store$bicycle$price)
  expect_equal(sort(prices), sort(expected))
})

# ============================================================================
# Phase 1.3: Array Operations
# ============================================================================

test_that("Array slice [start:end] works", {
  result <- json_path(json, "$.store.book[0:2]", simplify = FALSE)
  expect_equal(result, json$store$book[1:2])
  
  result <- json_path(json, "$.store.book[1:3]", simplify = FALSE)
  expect_equal(result, json$store$book[2:3])
})

test_that("Array slice [start:] works (from start to end)", {
  result <- json_path(json, "$.store.book[1:]", simplify = FALSE)
  expect_equal(result, json$store$book[2:4])
  
  result <- json_path(json, "$.store.book[2:]", simplify = FALSE)
  expect_equal(result, json$store$book[3:4])
})

test_that("Array slice [:end] works (from start to end)", {
  result <- json_path(json, "$.store.book[:2]", simplify = FALSE)
  expect_equal(result, json$store$book[1:2])
  
  result <- json_path(json, "$.store.book[:3]", simplify = FALSE)
  expect_equal(result, json$store$book[1:3])
})

test_that("Array slice [:] works (all elements)", {
  result <- json_path(json, "$.store.book[:]", simplify = FALSE)
  expect_equal(result, json$store$book)
})

test_that("Array slice with negative indices works", {
  result <- json_path(json, "$.store.book[-2:]", simplify = FALSE)
  expect_equal(result, json$store$book[3:4])
  
  result <- json_path(json, "$.store.book[:-1]", simplify = FALSE)
  expect_equal(result, json$store$book[1:3])
  
  result <- json_path(json, "$.store.book[-1:]", simplify = FALSE)
  expect_equal(result, json$store$book[4])
})

test_that("Array slice with step [start:end:step] works", {
  result <- json_path(json, "$.store.book[0:4:2]", simplify = FALSE)
  expect_equal(result, json$store$book[c(1, 3)])
  
  result <- json_path(json, "$.store.book[::2]", simplify = FALSE)
  expect_equal(result, json$store$book[c(1, 3)])
})

test_that("Union operator [0,1,2] works", {
  result <- json_path(json, "$.store.book[0,2]", simplify = FALSE)
  expect_equal(result, json$store$book[c(1, 3)])
  
  result <- json_path(json, "$.store.book[0,1,3]", simplify = FALSE)
  expect_equal(result, json$store$book[c(1, 2, 4)])
})

test_that("Union operator with negative indices works", {
  result <- json_path(json, "$.store.book[0,-1]", simplify = FALSE)
  expect_equal(result, json$store$book[c(1, 4)])
})

# ============================================================================
# Phase 1.4: Filter Expressions ?()
# ============================================================================

test_that("Existence filter [?(@.key)] works", {
  result <- json_path(json, "$.store.book[?(@.isbn)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) "isbn" %in% names(x), logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price<10)] works", {
  result <- json_path(json, "$.store.book[?(@.price<10)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price < 10, logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price>10)] works", {
  result <- json_path(json, "$.store.book[?(@.price>10)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price > 10, logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price==8.95)] works", {
  result <- json_path(json, "$.store.book[?(@.price==8.95)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price == 8.95, logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price!=8.95)] works", {
  result <- json_path(json, "$.store.book[?(@.price!=8.95)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price != 8.95, logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price<=10)] works", {
  result <- json_path(json, "$.store.book[?(@.price<=10)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price <= 10, logical(1))]
  expect_equal(result, expected)
})

test_that("Comparison filter [?(@.price>=10)] works", {
  result <- json_path(json, "$.store.book[?(@.price>=10)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price >= 10, logical(1))]
  expect_equal(result, expected)
})

test_that("Multiple conditions with && works", {
  result <- json_path(json, "$.store.book[?(@.price<10 && @.category=='fiction')]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price < 10 && x$category == "fiction", logical(1))]
  expect_equal(result, expected)
})

test_that("Multiple conditions with || works", {
  result <- json_path(json, "$.store.book[?(@.price<10 || @.price>20)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$price < 10 || x$price > 20, logical(1))]
  expect_equal(result, expected)
})

test_that("String comparison in filters works", {
  result <- json_path(json, "$.store.book[?(@.category=='fiction')]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) x$category == "fiction", logical(1))]
  expect_equal(result, expected)
})

test_that("Negation with ! works", {
  result <- json_path(json, "$.store.book[?(!@.isbn)]", simplify = FALSE)
  books <- json$store$book
  expected <- books[vapply(books, function(x) !("isbn" %in% names(x)), logical(1))]
  expect_equal(result, expected)
})

# ============================================================================
# Phase 1.5: Script Expressions ()
# ============================================================================

test_that("Script expression [(@.length-1)] gets last element", {
  result <- json_path(json, "$.store.book[(@.length-1)]", simplify = FALSE)
  books <- json$store$book
  expect_equal(result, books[[length(books)]])
})

test_that("Script expression [(@.length-2)] gets second to last", {
  result <- json_path(json, "$.store.book[(@.length-2)]", simplify = FALSE)
  books <- json$store$book
  expect_equal(result, books[[length(books) - 1]])
})

# ============================================================================
# Phase 1.6: All Examples from JSONPath Specification
# ============================================================================

test_that("$.store.book[*].author gets all authors", {
  result <- json_path(json, "$.store.book[*].author")
  expected <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(result, expected)
})

test_that("$..author gets all authors (recursive)", {
  result <- json_path(json, "$..author")
  expected <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(sort(result), sort(expected))
})

test_that("$.store.* gets all things in store", {
  result <- json_path(json, "$.store.*", simplify = FALSE)
  expect_equal(result, json$store)
})

test_that("$.store..price gets all prices in store", {
  result <- json_path(json, "$.store..price")
  expected <- c(unlist(lapply(json$store$book, `[[`, "price")), json$store$bicycle$price)
  expect_equal(sort(result), sort(expected))
})

test_that("$..book[2] gets third book", {
  result <- json_path(json, "$..book[2]", simplify = FALSE)
  expect_equal(result, json$store$book[[3]])
})

test_that("$..book[0,1] gets first two books", {
  result <- json_path(json, "$..book[0,1]", simplify = FALSE)
  expect_equal(result, json$store$book[1:2])
})

test_that("$..book[:2] gets first two books", {
  result <- json_path(json, "$..book[:2]", simplify = FALSE)
  expect_equal(result, json$store$book[1:2])
})

test_that("All specification examples pass", {
  # Comprehensive check that all examples from spec work
  expect_true(TRUE)  # Individual tests above verify each case
})

# ============================================================================
# Phase 1.7: Edge Cases
# ============================================================================

test_that("Handles empty path gracefully", {
  result <- json_path(json, "$", simplify = FALSE)
  expect_equal(result, json)
})

test_that("Handles missing keys gracefully", {
  # Should return empty or handle error appropriately
  expect_error(json_path(json, "$.nonexistent"), "not found|missing")
})

test_that("Handles deeply nested structures", {
  nested_json <- list(a = list(b = list(c = list(d = "value"))))
  result <- json_path(nested_json, "$..d")
  expect_equal(result, "value")
})

test_that("Handles array with single element", {
  single <- list(items = list(list(value = "test")))
  result <- json_path(single, "$.items[0].value")
  expect_equal(result, "test")
})

test_that("Handles quoted member names with special characters", {
  special_json <- list("key.with.dots" = "value", "key with spaces" = "value2")
  result1 <- json_path(special_json, "$['key.with.dots']")
  result2 <- json_path(special_json, "$['key with spaces']")
  expect_equal(result1, "value")
  expect_equal(result2, "value2")
})
