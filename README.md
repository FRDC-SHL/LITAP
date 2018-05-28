LITAP
================

**Landscape Integrated Terrain Analysis Package (LITAP)** for the analysis of waterflow based on elevation data and pit removals. Based on R. A. (Bob) MacMillan's FlowMapR program.

See the companion website for more details: <http://steffilazerte.github.io/LITAP>

Installation
------------

Use the `devtools` package to directly install R packages from github:

``` r
install.packages("devtools")  # If not already installed
devtools::install_github("steffilazerte/LITAP")
```

Example files
-------------

LITAP comes with several example files (`testELEV.dbf` and `testELEV_mini.dbf`). These are found in the "extdata" folder inside the LITAP package folder. If you are unsure where this folder is, use the function `system.file()`:

``` r
system.file("extdata", package = "LITAP")
```

Now you can copy and paste these files to your working folder to try out the following examples.

    ## [1] TRUE

Basic Usage
-----------

Load the package:

``` r
library(LITAP)
```

    ## Loading required package: magrittr

First, specify the dem file and the number of rows and columns:

``` r
complete_run(file = "testELEV.dbf", nrow = 150, ncol = 150)
```

    ## CALCULATING DIRECTIONS

    ## CALCULATING WATERSHEDS

    ## REMOVING INITIAL PITS

    ## CALCULATING POND (GLOBAL) WATERSHEDS

    ## CALCULATING FILL PATTERNS

    ## INVERTING DEM

    ## CALCULATING INVERTED DIRECTIONS

    ## CALCULATING INVERTED WATERSHEDS

    ## REMOVING INVERTED INITIAL PITS

    ## Run took: 0.36 min

Can also specify pit removal parameters:

``` r
complete_run(file = "testELEV.dbf", nrow = 150, ncol = 150, max_area = 5, max_depth = 0.2)
```

    ## CALCULATING DIRECTIONS

    ## CALCULATING WATERSHEDS

    ## REMOVING INITIAL PITS

    ## CALCULATING POND (GLOBAL) WATERSHEDS

    ## CALCULATING FILL PATTERNS

    ## INVERTING DEM

    ## CALCULATING INVERTED DIRECTIONS

    ## CALCULATING INVERTED WATERSHEDS

    ## REMOVING INVERTED INITIAL PITS

    ## Run took: 0.31 min

As well as the location of output files:

``` r
complete_run(file = "testELEV.dbf", nrow = 150, ncol = 150, folder_out = "./Output/")
```

    ## CALCULATING DIRECTIONS

    ## CALCULATING WATERSHEDS

    ## REMOVING INITIAL PITS

    ## CALCULATING POND (GLOBAL) WATERSHEDS

    ## CALCULATING FILL PATTERNS

    ## INVERTING DEM

    ## CALCULATING INVERTED DIRECTIONS

    ## CALCULATING INVERTED WATERSHEDS

    ## REMOVING INVERTED INITIAL PITS

    ## Run took: 0.3 min

Multiple file types
-------------------

LITAP accepts multiple file types and can automatically assess the number of rows and columns, depending on the type (see `?load_file` for details and requirements):

``` r
complete_run(file = "testELEV.csv")
complete_run(file = "testELEV.grd")
complete_run(file = "testELEV.flt")
```

Output
------

Output files include .rds (R data files) and .csv files in the "Final" folder, .dbf files in the "dbf" folder and backup files (for resuming runs) in the "Backup" folder. Additionally, an html report summaring the run is included in the output folder (`testELEV_final_report.html`).

See the [LITAP website](http://steffilazerte.github.io/LITAP/) hosted on github for more details and examples
