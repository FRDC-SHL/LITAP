#' Test DEM
#'
#' A small data frame for testing and running examples from helper functions.
#' This is how DEMs look once imported by LITAP.
#'
#' @format A data frame with 35154 rows and 6 variables:
#' \describe{
#'   \item{seqno}{Cell number}
#'   \item{elev}{Cell elevation}
#'   \item{row}{Cell row}
#'   \item{col}{Cell column}
#'   \item{missing}{Missing?}
#'   \item{buffer}{Buffer cell? Buffers are padding added to edges of a dem}
#' }
"test_dem"
