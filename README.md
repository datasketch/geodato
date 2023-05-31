
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geodato

<!-- badges: start -->
<!-- badges: end -->

An R package to hold and facilitate geographies for spatial analysis.

## Installation

You can install the development version of geodato from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("datasketch/geodato")
```

## Available maps

The available maps inside the package can be checked by running
`available_maps_df()`:

``` r
library(geodato)
#> Loading required package: geojsonio
#> Warning: package 'geojsonio' was built under R version 4.2.3
#> Registered S3 method overwritten by 'geojsonsf':
#>   method        from   
#>   print.geojson geojson
#> 
#> Attaching package: 'geojsonio'
#> The following object is masked from 'package:base':
#> 
#>     pretty
#> Loading required package: sf
#> Warning: package 'sf' was built under R version 4.2.3
#> Linking to GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1; sf_use_s2() is TRUE
#> Loading required package: yaml
#> Warning: package 'yaml' was built under R version 4.2.3

available_maps_df
#> # A tibble: 209 × 4
#>    map_name             type  main_map region
#>    <chr>                <chr> <chr>    <chr> 
#>  1 col_bog_localidades  main  <NA>     <NA>  
#>  2 col_bog_localidades2 main  <NA>     <NA>  
#>  3 col_departments      main  <NA>     <NA>  
#>  4 col_municipalities   main  <NA>     <NA>  
#>  5 gtm_departments      main  <NA>     <NA>  
#>  6 gtm_municipalities   main  <NA>     <NA>  
#>  7 mex_cdmx_ageb        main  <NA>     <NA>  
#>  8 mex_cdmx_colonies    main  <NA>     <NA>  
#>  9 mex_cdmx_localities  main  <NA>     <NA>  
#> 10 mex_cdmx_mayors      main  <NA>     <NA>  
#> # ℹ 199 more rows
```

## How to contribute

A new map can be added by following the steps described on the article
`new-geography-layer`, available by running
`vignette("new-geography-layer.Rmd")`
