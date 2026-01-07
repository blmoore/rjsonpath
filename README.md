
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjsonpath

[![travis_status](https://travis-ci.org/blmoore/rjsonpath.svg?branch=master)](https://travis-ci.org/blmoore/rjsonpath)
[![codecov](https://codecov.io/gh/blmoore/rjsonpath/branch/master/graph/badge.svg)](https://codecov.io/gh/blmoore/rjsonpath)
[![docs_badge](https://img.shields.io/badge/docs-latest-blue.svg)](http://blm.io/rjsonpath)
[![CRAN_badge](http://www.r-pkg.org/badges/version/rjsonpath)](https://cran.r-project.org/package=rjsonpath)

Reading JSON into R usually leaves you with some deeply-nested list
objects which are a pain to munge and navigate — applying path
expressions is one way to make this easier. `rjsonpath` implements
[JSONPath](http://goessner.net/articles/JsonPath/), a selector /
querying language for JSON, similar to XPath for XML.

## Install

Install from github with:

``` r
devtools::install_github("blmoore/rjsonpath")
```

## Usage

As an example, take this simple JSON:

``` js
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
```

<script>
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
</script>

Via `read_json` this can be read into R as:

``` r
json
#> $menu
#> $menu$id
#> [1] "file"
#> 
#> $menu$value
#> [1] "File"
#> 
#> $menu$popup
#> $menu$popup$menuitem
#> $menu$popup$menuitem[[1]]
#>            value          onclick 
#>            "New" "CreateNewDoc()" 
#> 
#> $menu$popup$menuitem[[2]]
#>       value     onclick 
#>      "Open" "OpenDoc()" 
#> 
#> $menu$popup$menuitem[[3]]
#>        value      onclick 
#>      "Close" "CloseDoc()"
```

Pretty horrible right? To gather all the `onclick` methods into a vector
with base R you might write:

``` r
sapply(json$menu$popup$menuitem, `[[`, "onclick")
#> [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
```

Using `rjsonpath` this could instead be:

``` r
json_path(json, "$.menu.popup.menuitem[*].onclick")
#> [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
```

Or even just:

``` r
json_path(json, "$..onclick")
#> NULL
```

For more more complex examples, see [below](#Advanced%20expressions).

## Advanced expressions

For more complex JSON documents it’s common to combine filters, array
slices and recursive descent. The following examples use a simple
“store” object similar to the one in the original JSONPath
specification:

### Filter expressions

Select only books cheaper than 10:

``` r
json_path(store, "$.store.book[?(@.price < 10)].title")
#> NULL
```

Select books that have an `isbn` field:

``` r
json_path(store, "$.store.book[?(@.isbn)].title")
#> [1] "Moby Dick"             "The Lord of the Rings"
```

### Recursive descent

Get all prices anywhere under `store`:

``` r
json_path(store, "$.store..price")
#> [1]  8.95 12.99  8.99 22.99 19.95
```

Get all authors anywhere in the document:

``` r
json_path(store, "$..author")
#> [1] "Nigel Rees"       "Evelyn Waugh"     "Herman Melville"  "J. R. R. Tolkien"
```

### Array slices and unions

Select a slice of books (second and third) and return their titles:

``` r
json_path(store, "$.store.book[1:3].title")
#> [1] "Sword of Honour" "Moby Dick"
```

Select a non‑contiguous subset of books by index:

``` r
json_path(store, "$.store.book[0,3].title")
#> [1] "Sayings of the Century" "The Lord of the Rings"
```
