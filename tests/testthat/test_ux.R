context("Error messages")

test_that("errors report missing nested fields", {
  json <- jsonlite::read_json("bookstore.json")

  # Requesting a missing field should give a helpful error mentioning the field
  expect_error(
    json_path(json, "$.store.nonexistent"),
    "nonexistent not found in json"
  )

  # Deeply nested missing field should also mention the missing component
  expect_error(
    json_path(json, "$.store.book[0].nonexistent_field"),
    "nonexistent_field not found in json"
  )
})
