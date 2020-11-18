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
#'@param additionalAttributes Optional: Vector of names of additional attributes which should be collected in the returned data frame.
#'
#'@return A \code{data.frame}.
#'
#'@author Sebastian Weirich
#'
#'@examples
#'file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
#'dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
#'dat  <- convertLabel(dat)
#'atts <- createLabelList(dat)
#'@export
createLabelList <- function ( dfr, additionalAttributes = NULL ) {
         stopifnot ( class(dfr) == "data.frame")
         varList<- do.call(plyr::rbind.fill, lapply(colnames(dfr), FUN = function ( v ) {
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
                   if (!is.null(additionalAttributes)) {
                        for ( i in additionalAttributes) {
                              ret[,i] <- lbs[[i]]
                        }
                   }
                   return(ret)}))
         return(varList)}


