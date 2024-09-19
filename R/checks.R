
check_facets <- function(facet,
                         nm = c("cst", "ups", "mid", "low", "dep"),
                         fun = warning) {

  if(!all(nm %in% names(facet))) {
    fun("This step requires facets '",
            paste0(nm, collapse = "', '"),
            ". Use `facet_mapp()` with a `crule` file with the relevant facets",
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
