####
#############################################################################
#' Generate interaction terms for lm formulas
#'
#' Specify two-way or three-way interactions from a set of independent variables.
#'
#'@param vars Character vector of variables
#'@param upto The maximum way of interactions which should be created.
#'
#'@return A character string (snippet) useful for formula generation.
#'
#'@author Sebastian Weirich
#'
#'@examples
#' vars <- paste0("Var_", letters[1:5])
#' ### only two way interactions
#' frml <- intGen(vars = vars, upto = 2)
#'
#'@export
intGen <- function ( vars, upto = 3) {
  snip <- paste(lapply(2:upto, FUN = function ( v ) {
  spl <- data.frame ( combinat::combn(vars,v), stringsAsFactors = FALSE)
  sni <- paste(lapply(spl, FUN = function ( x ) {
    paste(x, collapse = " : ")}), collapse = " + ")
  return(sni)
  }), collapse = " + ")
return(snip)
}
