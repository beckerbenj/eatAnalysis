####
#############################################################################
#' Transform SPSS variable and value labels into old ZKD convention.
#'
#' By default, 'read.spss' from the foreign package uses variable.labels as attributes of the whole data.frame, value.labels as attribute of each specific
#'variable in the data.frame. \code{convertLabel} provides variable and value labels as variable attributes.
#'
#'@param spssList An object created by \code{\link[foreign]{read.spss}}. Important: Using \code{\link[foreign]{read.spss}}, \code{to.data.frame} has to be FALSE.
#'@param stringsAsFactors Transform character variables into factors?
#'@param useZkdConvention Logical: Use ZKD convention, i.e. variable.labels are named 'varLabel', value.labels are named 'valLabel'.
#'
#'@return A \code{data.frame}.
#'
#'@author Sebastian Weirich
#'
#'@examples
#'file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
#'dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
#'dat  <- convertLabel(dat)
#'str(dat)
#'@export
convertLabel <- function ( spssList , stringsAsFactors = TRUE, useZkdConvention = TRUE) {
  if ( "data.frame" %in% class(spssList )) { stop ( "'spssList' must not be of class 'data.frame'.\n")}
  varLabs<- attr(spssList, "variable.labels")
  valLabs<- lapply ( spssList, attr, "value.labels")
  if ( useZkdConvention == TRUE ) { zielnam <- c("varLabel", "valLabel") }  else  { zielnam <- c("variable.labels", "value.labels") }
  datFr  <- data.frame ( spssList, stringsAsFactors = stringsAsFactors)
  for ( u in 1:ncol(datFr)) {
    if ( !is.null(varLabs[colnames(datFr)[u]]) && !is.na(varLabs[colnames(datFr)[u]] )) { attr(datFr[,u], zielnam[1]) <- varLabs[[u]]}
    if ( !is.null(valLabs[[colnames(datFr)[u]]] )) { attr(datFr[,u], zielnam[2]) <- valLabs[[u]]}
  }
  return(datFr)
}
