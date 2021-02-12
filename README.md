LITAP
================

**Landscape Integrated Terrain Analysis Package (LITAP)** for the
analysis of waterflow based on elevation data and pit removals. Based on
R. A. (Bob) MacMillan’s LandMapR suite of programs.

Currently LITAP includes functions

-   `flow_mapper()` - based on FlowMapR
-   `form_mapper()` - based on FormMapR
-   `facet_mapper()` - based on FacetMapR
-   `wepp_mapper()` - based on WeppMapR

See the companion website for more details:
<http://steffilazerte.github.io/LITAP>

## Installation

Use the `remotes` package to directly install R packages from github:

``` r
install.packages("remotes") # If not already installed
remotes::install_github("steffilazerte/LITAP") 
```

## Example files

LITAP comes with several example files (`testELEV.dbf`,
`testELEV_mini.dbf`, `arule.dbf`, and `crule.dbf`). These are found in
the “extdata” folder inside the LITAP package folder. If you are unsure
where this folder is, use the function `system.file()`:

``` r
system.file("extdata", package = "LITAP")
```

Now you can copy and paste these files to your working folder to try out
the following examples.

## Basic Usage: `flow_mapper()`

Load the package:

``` r
library(LITAP)
```

    ## LITAP v0.4.0
    ## Please note that 'LITAP' is based on Bob MacMillan's LandMapper suite of programs.
    ## LITAP is still in development; Help us by submitting bugs/feature requests: 
    ## http://github.com/steffilazerte/LITAP/issues

First, specify the dem file and the number of rows and columns:

``` r
flow_mapper(file = "testELEV.dbf", nrow = 150, ncol = 150)
```

Can also specify pit removal parameters:

``` r
flow_mapper(file = "testELEV.dbf", nrow = 150, ncol = 150, 
             max_area = 5, max_depth = 0.2)
```

As well as the location of output files:

``` r
flow_mapper(file = "testELEV.dbf", nrow = 150, ncol = 150, out_folder = "./Output/")
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

    ## SAVING OUTPUT

    ## CREATING REPORT

    ## Run took: 0.49 min

## Basic Usage: `form_mapper()`

`form_mapper()` uses output from `flow_mapper()` and requires a grid
size.

``` r
form_mapper(folder = "./Output/", grid = 5)
```

    ## SETUP

    ## CALCULATING FORM

    ## CALCULATING WETNESS INDICES

    ## CALCULATING RELIEF DERIVITIVES

    ## CALCULATING SLOPE LENGTH

    ## Run took: 0.23 min

Optionally, users can also define channels and ridges according to the
number of up-/down-slope cells that flow through the cell in question.

``` r
form_mapper(folder = "./Output/", str_val = 10000, ridge_val = 10000, grid = 5)
```

## Basic Usage: `facet_mapper()`

`facet_mapper()` uses output from `flow_mapper()` and `form_mapper()`
and requires a CRULE file and, optionally, an ARULE file.

``` r
facet_mapper(folder = "./Output/", crule = "crule.dbf", arule = "arule.dbf")
```

    ## Formatting arule file

    ##   - Renaming 'slope' to 'slope_pct'

    ## Formatting crule file

    ## 

    ## CALCULATING FUZZY ATTRIBUTES

    ## CALCULATING CLASSES

    ## Run took: 0 min

If an ARULE file is not provided, LITAP will derive the rules based on
the input files (as in Li et al. 2011, Canadian Journal of Soil Science
91(2), 251-266). The derived A rules will be output to
“afile\_derived.csv”.

``` r
facet_mapper(folder = "./Output/", crule = "crule.dbf")
```

## Basic Usage: `wepp_mapper()`

`wepp_mapper()` uses output from `flow_mapper()` and requires a grid
size.

``` r
wepp_mapper(folder = "./Output/", grid = 5)
```

Optionally, users can also define the maximum length of channel cells
(after which they will be split into separate segments), as well as the
upslope threshold to define channel cells

``` r
wepp_mapper(folder = "./Output/", chan_length = 500, upslope_threshold = 500, grid = 5)
```

## Multiple file types

LITAP accepts multiple file types and can automatically assess the
number of rows and columns, depending on the type (see `?load_file` for
details and requirements):

``` r
flow_mapper(file = "testELEV.csv")
flow_mapper(file = "testELEV.grd")
flow_mapper(file = "testELEV.flt")
```

## Output

Output files can be .csv, .dbf, or .rds (R data files) in the output
folders “flow”, “form”, “facet” and “wepp”. Backup files (for
`form_mapper()`) are stored in the “Backup” folder. Additionally, an
html report summarizing the `flow_mapper()` run is included in the
output folder (`testELEV_final_report.html`).

See the [LITAP website](http://steffilazerte.github.io/LITAP/) hosted on
github for more details and examples
