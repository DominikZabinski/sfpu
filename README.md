
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfpu (Stuff For Personal Usage)

<!-- badges: start -->

[![R-CMD-check](https://github.com/DominikZabinski/sfpu/actions/workflows/check-release.yaml/badge.svg)](https://github.com/DominikZabinski/sfpu/actions/workflows/check-release.yaml)

[![test-coverage](https://github.com/DominikZabinski/sfpu/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/DominikZabinski/sfpu/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Overview

The aim of this package to gather most of relevant, useful, repetitive
functions and datasets in one place. So the scope of this package is:

-   simplified shapefiles of Poland, at different administrative levels
-   calculated shortest paths between points in graph (i.e. distance
    between centers of polygons based on neighbourhood, not proximity)
-   functions for generatvie art projects
-   etc.

## Usage

`library(sfpu)` will load the most useful packages:

-   data.table;
-   ggplot2;
-   sf;
-   stringr.

and give access to some prepared datasets:

-   simplified shapefiles of Poland
-   neighbourhood
-   precalcutaed distances based on Djikstra algorithm ([R
    implementation](https://blog.ephorie.de/finding-the-shortest-path-with-dijkstras-algorithm),
    [Wiki
    article](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm))

## Packages

-   [data.table](https://github.com/Rdatatable/data.table) for
    manipulating data;
-   [ggplot2](https://github.com/tidyverse/ggplot2) for graphics;
-   [magrittr](https://github.com/tidyverse/magrittr) for pipe;
-   [stringr](https://github.com/tidyverse/stringr) for string
    manipulation;
-   [sf](https://github.com/r-spatial/sf) for simple features of spatial
    data.

## GitHub Actions

For learning purposes this package is using GitHub Actions. Great post
about GitHub Actions can be found here: [Monitoring quarto-dev
repositories: Creating a workflow with GitHub Actions for R
users](https://beamilz.com/posts/series-gha/2022-series-gha-2-creating-your-first-action/en/)
