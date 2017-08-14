#' Dealing with CRAN Notes due to Non-standard evaluation
.onLoad <- function(libname = find.package("feedr"), pkgname = "feedr"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
      c("drec", "flatcell", "ldir", "n", "local_shed",

        # piping requires '.' at times
        ".")

    )
  invisible()
}
