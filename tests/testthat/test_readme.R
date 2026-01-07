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
json <- jsonlite::fromJSON(json, simplifyVector = FALSE)

store_json <- '{
  "store": {
    "book": [
      {
        "category": "reference",
        "author": "Nigel Rees",
        "title": "Sayings of the Century",
        "price": 8.95
      },
      {
        "category": "fiction",
        "author": "Evelyn Waugh",
        "title": "Sword of Honour",
        "price": 12.99
      },
      {
        "category": "fiction",
        "author": "Herman Melville",
        "title": "Moby Dick",
        "isbn": "0-553-21311-3",
        "price": 8.99
      },
      {
        "category": "fiction",
        "author": "J. R. R. Tolkien",
        "title": "The Lord of the Rings",
        "isbn": "0-395-19395-8",
        "price": 22.99
      }
    ],
    "bicycle": {
      "color": "red",
      "price": 19.95
    }
  }
}'
store <- jsonlite::fromJSON(store_json, simplifyVector = FALSE)

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

test_that("readme filter example: cheap books", {
  titles <- json_path(store, "$.store.book[?(@.price < 10)].title")
  expected <- vapply(
    store$store$book,
    function(x) if (x$price < 10) x$title else NA_character_,
    character(1)
  )
  expected <- stats::na.omit(expected)
  expect_identical(titles, unname(expected))
})

test_that("readme filter example: books with isbn", {
  titles <- json_path(store, "$.store.book[?(@.isbn)].title")
  expected <- vapply(
    store$store$book,
    function(x) if ("isbn" %in% names(x)) x$title else NA_character_,
    character(1)
  )
  expected <- stats::na.omit(expected)
  expect_identical(titles, unname(expected))
})

test_that("readme recursive price example matches manual extraction", {
  prices <- json_path(store, "$.store..price")
  expected <- c(
    vapply(store$store$book, `[[`, numeric(1), "price"),
    store$store$bicycle$price
  )
  expect_equal(sort(prices), sort(expected))
})

test_that("readme recursive author example matches manual extraction", {
  authors <- json_path(store, "$..author")
  expected <- vapply(store$store$book, `[[`, character(1), "author")
  expect_equal(sort(authors), sort(expected))
})

test_that("readme slice example: second and third titles", {
  titles <- json_path(store, "$.store.book[1:3].title")
  expected <- vapply(
    store$store$book[2:3],
    `[[`,
    character(1),
    "title"
  )
  expect_identical(titles, unname(expected))
})

test_that("readme union example: first and last titles", {
  titles <- json_path(store, "$.store.book[0,3].title")
  books <- store$store$book
  expected <- vapply(
    books[c(1, length(books))],
    `[[`,
    character(1),
    "title"
  )
  expect_identical(titles, unname(expected))
})
