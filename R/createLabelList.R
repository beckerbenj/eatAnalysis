####
#############################################################################
#' Creates a data.frame with variable and value labels from variable attributes
#'
#' By default, \code{\link[foreign]{read.spss}} from the foreign package uses variable.labels
#' as attributes of the whole \code{data.frame}, value.labels as attribute of each specific
#' variable in the \code{data.frame}. \code{\link{convertLabel}} provides variable and value
#' labels as variable attributes. \code{createLabelList} creates a data.frame with
#' variable and value labels.
#'
#'@param dfr A data.frame with variable and value labels stored as attributes using the convention of \code{\link{convertLabel}}.
#'
#'@return A \code{data.frame}.
#'
#'@author Sebastian Weirich
#'
#'@examples
#'\dontrun{
#'  dat <- foreign::read.spss("c:/exampleFolder/exampleDataSet.sav",
#'                            to.data.frame=FALSE, use.value.labels = FALSE,
#'                            reencode = "65001")
#'  dat <- convertLabel(dat)
#'  atts<- createLabelList(dat)
#'}
#'
#'@export
createLabelList <- function ( dfr ) {
stopifnot ( class(dfr) == "data.frame")
varList<- do.call("rbind.fill", lapply(colnames(dfr), FUN = function ( v ) {
  lbs  <- attributes(dfr[,v])
  if (!is.null(lbs[["varLabel"]]))  {
    varLab <- unlist(lbs[["varLabel"]])
  }  else  {
    varLab <- NA
  }
  if(!is.null(lbs[["valLabel"]]) && length(lbs[["valLabel"]])>0)  {
    vals <- data.frame ( value = as.vector(unlist(lbs[["valLabel"]])), valLabel = names(lbs[["valLabel"]]), stringsAsFactors = FALSE)
    ret <- data.frame ( varName = v, class = class(dfr[,v]), varLabel = varLab , vals[,c("value", "valLabel")], stringsAsFactors = FALSE)
  }  else  {
    ret <- data.frame ( varName = v, class = class(dfr[,v]), varLabel = varLab , stringsAsFactors = FALSE)
  }
  return(ret)}))
return(varList)
}



