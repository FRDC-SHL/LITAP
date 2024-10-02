
# Create local data
source("data-raw/create_rules.R")
source("data-raw/internal_data.R")
source("data-raw/create_data.R")

# Run package checks
devtools::check()


devtools::build_readme()

# Create reports and save to vignettes
source("data-raw/formal_tests.R")

# Update package Version

# Update NEWS

# Update website and reports - BUILD PACKAGE FIRST!!!
pkgdown::build_site(lazy = TRUE)
pkgdown::build_reference_index()
pkgdown::build_reference()
pkgdown::build_home()
pkgdown::build_article("variables")
pkgdown::build_article("articles/flow_plots")
pkgdown::build_articles_index()


