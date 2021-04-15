#############################################################################
#' Round data frame.
#'
#' Round all numeric variables in a \code{data.frame}.
#'
#'@param df A \code{data.frame}.
#'@param roundN Integer indicating the number of decimal places.
#'
#'@return Returns the rounded \code{data.frame}.
#'
#'@examples
#'roundDF(mtcars, roundN = 0)
#'
#'@export
roundDF <- function(df, roundN = 3) {
  roundCol <- function(var, roundN) {
    if(!is.numeric(var)) return(var)
    round(var, digits = roundN)
  }

  out <- as.data.frame(lapply(df, roundCol, roundN = roundN), stringsAsFactors = FALSE)
  names(out) <- names(df)
  out
}
