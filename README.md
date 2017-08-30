---
title: "README"
---

# LITAP

__Landscape Integrated Terrain Analysis Package (LITAP)__ for the analysis of waterflow based on elevation data and pit removals. Based on R. A. (Bob) MacMillan's FlowMapR program.

## Installation

Use the `devtools` package to directly install R packages from github:

```{r}
install.packages("devtools") # If not already installed
devtools::install_github("steffilazerte/LITAP") 
```

## Basic Usage

Must specify the dem file and the number of rows and columns:

```{r}
complete_run(file = "testElev.dem", nrow = 100, ncol = 100)
```

Can also specify pit removal parameters:

```{r}
complete_run(file = "testElev.dem", nrow = 100, ncol = 100, 
             max_area = 5, max_depth = 0.2)
```

As well as the location of output files:
```{r}
complete_run(file = "testElev.dem", nrow = 100, ncol = 100, folder_out = "./Output/")
```

See the [LITAP website](http://steffilazerte.github.io/LITAP/) hosted on github for more details and examples
