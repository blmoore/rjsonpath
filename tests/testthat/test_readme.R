# test readme examples with different JSON

json <- '{"menu": {
  "id": "file",
"value": "File",
"popup": {
"menuitem": [
{"value": "New", "onclick": "CreateNewDoc()"},
{"value": "Open", "onclick": "OpenDoc()"},
{"value": "Close", "onclick": "CloseDoc()"}
]
}
}}'
json <- RJSONIO::fromJSON(json)

context("README examples")

test_that("readme full example gives correct answer", {
  readme1 <- json_path(json, "$.menu.popup.menuitem[*].onclick")
  readme2 <- sapply(json$menu$popup$menuitem, `[[`, "onclick")
  expect_identical(readme1, readme2)
})

test_that("readme short example gives correct answer", {
  readme1 <- json_path(json, "$..onclick")
  readme2 <- sapply(json$menu$popup$menuitem, `[[`, "onclick")
  expect_identical(readme1, readme2)
})
