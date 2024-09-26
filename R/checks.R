
check_facets <- function(folder,
                         nm = c("cst", "ups", "mid", "low", "dep")) {

  c1 <- list_previous(folder, where = "facet", step = "fuzc", check_only = TRUE)
  if(c1) facet <- get_previous(folder, where = "facet", step = "fuzc")
  c2 <- file.exists(file.path(folder, "topographic_derivatives.xlsx"))

  if(!c1 || !c2 || !all(nm %in% names(facet))) {
    warning("Skipping Topographic summary...\n",
            "  This step requires a facets output file `dem_fuzc` with facets '",
            paste0(nm, collapse = "', '"), ",\n",
            "  as well as a topographic_derivatives.xlsx file.\n",
            "  Use `facet_mapper()` with a `crule` file defining the relevant facets and *no* `arule` file.",
            call. = FALSE)
    r <- FALSE
  } else r <- TRUE
  r
}

check_folder <- function(folder, fun = warning) {
  if(!dir.exists(folder)) {
    fun("This folder doesn't exist: ", folder, call. = FALSE)
  }
}

