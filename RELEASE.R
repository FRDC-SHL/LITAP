
# Create local data
source("data-raw/create_rules.R")
source("data-raw/internal_data.R")
source("data-raw/create_data.R")

# Run package checks
devtools::check()


# Create reports and save to vignettes
# source("../formal_tests_reports_flow.R")
# source("../formal_tests_reports_form.R")
# source("../formal_tests_reports_facet.R")
# source("../formal_tests_reports_wepp.R")

# Update package Version

# Update NEWS

# Update website and reports
pkgdown::build_site(lazy = TRUE)
pkgdown::build_reference_index()
pkgdown::build_reference()
file.copy(from = "docs", to = "../LITAP_docs/", recursive = TRUE)
# Move site to LITAP_docs


