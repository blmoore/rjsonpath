context("Basic JSONPath")

json <- jsonlite::read_json("bookstore.json")

test_that("json_path can select single items", {

  expect_equal(json_path(json, "$.store.book[0].author"),
    json$store$book[[1]]$author)

})

test_that("json_path can select multiple items with wildcards", {

  all_authors <- unlist(lapply(json$store$book, `[[`, "author"))
  expect_equal(json_path(json, "$.store.book[*].author"), all_authors)

})

test_that("json_path can select a range of items", {

  from_range <- json_path(json, "$.store.book[*].author[2:3]")
  expect_length(from_range, 2)

})

test_that("zero indexing can be switched on or off", {
  zero <- json_path(json, "$.store.book[0]")
  nonzero <- json_path(json, "$.store.book[1]", zero_index = FALSE)
  expect_equal(zero, nonzero)
})

test_that("json_path works with magrittr pipes", {
  if (require(magrittr)) {
    first_book <- read_json("bookstore.json") %>%
      json_path("$.store.book[1]")
    nonmagrittr <- json_path(json, "$.store.book[1]")
    expect_equal(first_book, nonmagrittr)
  }
})

context("Advanced JSONPath")

test_that("named objects can be omitted", {
  name_omitted <- json_path(json, "$..book[0].title")
  with_name <- json_path(json, "$.store.book[0].title")
  expect_equal(name_omitted, with_name)

  name_omitted <- json_path(json, "$..book[0].title")
  with_name <- json_path(json, "$.store.book[0].title")
  expect_equal(name_omitted, with_name)

})

test_that("single-quoted dots work in member names", {
})
