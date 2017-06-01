# test parts of jsonpath processor

context("path processing")

test_that("normalise path reformats jsonpath", {
  norm <- rjsonpath:::format_path

  expect_equal(norm("$.store.book[*].author"), "$;store;book;*;author")
  expect_equal(norm("$..*"), "$;..;*")
  expect_equal(norm("$..book[2]"), "$;..;book;2")
  expect_equal(norm("$..book[?(@.price<10)]"), "$;..;book;?(@.price<10)")
})
