
<!-- README.md is generated from README.Rmd. Please edit that file -->
rjsonpath
=========

[![travis\_status](https://travis-ci.org/blmoore/rjsonpath.svg?branch=master)](https://travis-ci.org/blmoore/rjsonpath) [![codecov](https://codecov.io/gh/blmoore/rjsonpath/branch/master/graph/badge.svg)](https://codecov.io/gh/blmoore/rjsonpath) [![docs\_badge](https://img.shields.io/badge/docs-latest-blue.svg)](http://blm.io/rjsonpath) [![CRAN\_badge](http://www.r-pkg.org/badges/version/rjsonpath)](https://cran.r-project.org/package=rjsonpath)

Reading JSON into R usually leaves you with some deeply-nested list objects which are a pain to munge and navigate — applying path expressions is one way to make this easier. `rjsonpath` implements [JSONPath](http://goessner.net/articles/JsonPath/), a selector / querying language for JSON, similar to XPath for XML.

Install
-------

Install from github with:

``` r
devtools::install_github("blmoore/rjsonpath")
```

Usage
-----

As an example, take this simple JSON:

``` javascript
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

Pretty horrible right? To gather all the `onclick` methods into a vector with base R you might write:

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
#> [1] "CreateNewDoc()" "OpenDoc()"      "CloseDoc()"
```

For more more complex examples, see [below](#Advanced%20expressions).

Advanced expressions
--------------------
