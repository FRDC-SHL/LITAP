check_rules <- function(arule, crule) {

  if(any(!arule$attr_in %in% c(arule_weti, arule_relief))) {
    a <- arule$attr_in[!arule$attr_in %in% c(arule_weti, arule_relief)]
    stop("Invalid 'arule' attribute(s) ('attr_in'): ",
         paste0(a, collapse = ", "), call. = FALSE)
  }

  if(any(!crule$fuzattr %in% arule$class_out)) {
    a <- crule$fuzattr[!crule$fuzattr %in% arule$class_out]
    stop("Some arule classes ('class_out') don't match crule fuzzy attributes ",
         "('fuzzattr'): ", paste0(a, collpase = ", "), call. = FALSE)
  }

}
