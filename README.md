
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfpu (Stuff For Personal Usage)

<!-- badges: start -->

[![R-CMD-check](https://github.com/DominikZabinski/sfpu/actions/workflows/check-release.yaml/badge.svg)](https://github.com/DominikZabinski/sfpu/actions/workflows/check-release.yaml)

[![test-coverage](https://github.com/DominikZabinski/sfpu/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/DominikZabinski/sfpu/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Overview

The aim of this package to gather most of relevant, useful, repetitive
functions and datasets in one place. So the scope is:

-   simplified shapefiles of Poland, on different level
-   calculated shortest paths between points in graph (i.e. distance
    between centers of polygons based on neighbourhood, not proximity)
-   functions for generatvie art projects
-   etc.

## Usage

`library(sfpu)` will load the most useful packages:

-   data.table
-   ggplot2
-   magrittr

and give access to some prepared datasets:

-   simplified shapefiles of Poland
-   neighbourhood
-   precalcutaed distances based on Djikstra algorithm

## Packages

-   [data.table](https://github.com/Rdatatable/data.table), for
    manipulating data.
-   [ggplot2](https://github.com/tidyverse/ggplot2), for graphics.
-   [magrittr](https://github.com/tidyverse/magrittr), for pipe.

## To do

-   replace magrittr pipe for native R pipe
-   add tests to every function
-   unify naming of functions and arguments
-   add comments in both English and Polish
-   use github actions for learning purposes
