# Examples from http://goessner.net/articles/JsonPath/

context("JSONPath examples")

json <- jsonlite::read_json("bookstore.json")

test_that("$.store.book[*].author gets all authors of all books", {
  authors <- json_path(json, "$.store.book[*].author")
  answer <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(authors, answer)
})

test_that("$..author gets all authors", {
  authors <- json_path(json, "$..author")
  answer <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(authors, answer)
})

test_that("$.store.* gets all items in store", {
  items <- json_path(json, "$.store.*")
  answer <- json$store
  expect_equal(items, answer)
})

test_that("$.store..price gets all prices in store", {
  prices <- json_path(json, "$.store..price")
  book_prices <- unlist(lapply(json$store$book, `[[`, "price"))
  answer <- c(book_prices, json$store$bicycle$price)
  expect_equal(prices, answer)
})

test_that("$..book[2] gets third book", {
  book <- json_path(json, "$..book[2]", simplify = FALSE)
  answer <- json$store$book[[3]]
  expect_equal(book, answer)
})

test_that("$..book[(@.length-1)] / $..book[-1:] gets last book", {
  book <- json_path(json, "$..book[(@.length-1)]")

  books <-  json$store$book
  answer <- books[[length(books)]]
  expect_equal(book, answer)

  last_book <- json_path(json, "$..book[-1:]")
  expect_equal(last_book, answer)
})

test_that("$..book[0,1] / $..book[:2] gets first two books", {
  first_two <- json_path(json, "$..book[0,1]")
  answer <- json$store$book[1:2]
  expect_equal(first_two, answer)

  first_books <- json_path(json, "$..book[:2]")
  expect_equal(first_books, answer)
})

test_that("$..book[?(@.isbn)] selects books with isbns", {
  with_isbns <- json_path(json, "$..book[?(@.isbn)]")

  books <- json$store$book
  index <- unlist(lapply(books, function(x) "isbn" %in% names(x)))
  answer <- books[index]
  expect_equal(with_isbns, answer)
})

test_that("$..book[?(@.price<10)] gets all books cheaper than 10", {
  cheap_books <- json_path(json, "$..book[?(@.price<10)]", simplify = FALSE)

  books <- json$store$book
  index <- unlist(lapply(books, function(x) x$price < 10))
  answer <- books[index]
  expect_equal(cheap_books, answer)
})

test_that("$..* gets all all elements in tree", {
  all_elem <- json_path(json, "$..*")

  # Should include the top-level store object ...
  expect_true(is.list(all_elem))
  expect_gte(length(all_elem), 10)
  expect_equal(all_elem[[1]], json$store)
  # ... and the bicycle object somewhere in the result
  expect_true(any(vapply(all_elem, identical, logical(1), json$store$bicycle)))
})
