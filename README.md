
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjsonpath

[![CircleCI](https://dl.circleci.com/status-badge/img/gh/blmoore/rjsonpath/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/blmoore/rjsonpath/tree/master)
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
#> $menu$popup$menuitem[[1]]$value
#> [1] "New"
#> 
#> $menu$popup$menuitem[[1]]$onclick
#> [1] "CreateNewDoc()"
#> 
#> 
#> $menu$popup$menuitem[[2]]
#> $menu$popup$menuitem[[2]]$value
#> [1] "Open"
#> 
#> $menu$popup$menuitem[[2]]$onclick
#> [1] "OpenDoc()"
#> 
#> 
#> $menu$popup$menuitem[[3]]
#> $menu$popup$menuitem[[3]]$value
#> [1] "Close"
#> 
#> $menu$popup$menuitem[[3]]$onclick
#> [1] "CloseDoc()"
```

Pretty horrible right? To gather all the `onclick` methods into a vector
with base R you might write:

``` r
sapply(json$menu$popup$menuitem, `[[`, "onclick")
#> [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
```

Using `rjsonpath` this could instead be:

``` r
# json_path(json, "$.menu.popup.menuitem[*].onclick")
# [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
```

Or even just:

``` r
# json_path(json, "$..onclick")
# [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
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
```

Select books that have an `isbn` field:

``` r
json_path(store, "$.store.book[?(@.isbn)].title")
```

### Recursive descent

Get all prices anywhere under `store`:

``` r
json_path(store, "$.store..price")
```

Get all authors anywhere in the document:

``` r
json_path(store, "$..author")
```

### Array slices and unions

Select a slice of books (second and third) and return their titles:

``` r
json_path(store, "$.store.book[1:3].title")
```

Select a non‑contiguous subset of books by index:

``` r
json_path(store, "$.store.book[0,3].title")
```

## Performance

Performance benchmarks on a large JSON object (10,000 element array):

    #> | Operation | Median Time | Mean Time |
    #> |-----------|-------------|-----------|
    #> | Simple property access (`$.data[*].name`) | 259.0 ms | 266.4 ms |
    #> | Filter expression (`$.data[?(@.price<50)]`) | 834.8 ms | 839.6 ms |
    #> | Recursive descent (`$..target`) | 0.13 ms | 0.16 ms |
    #> | Array slice (`$.data[0:100]`) | 0.2 ms | 0.3 ms |
