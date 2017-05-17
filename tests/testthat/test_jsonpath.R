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

context("Advanced JSONPath")

test_that("single-quoted dots work in member names", {
})