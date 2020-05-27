####
#############################################################################
#' Prepare SPSS syntax to transfer variable and value labels into an SPSS data set
#'
#' Usually, variable and value labels are lost if ASCII data is imported into SPSS.
#' Variable and value labels normally must be reestablished using SPSS syntax file. The function
#' prepares an SPSS syntax snippet from the variable and value labels stored as attributes in R.
#'
#'@param dat R data frame. Variable and value labels must be stored in attributes as provided, for example, by the function \code{\link{convertLabel}}.
#'@param file Character string with the name of syntax file which should be created.
#'@param keep Logical: Create value labels even if the value does not occur in the data? For example, if
#' \code{keep = TRUE}, \code{createSpssSyntaxSnippet} will write labels also for males, even if the data only contains females.
#'
#'@return No return, the SPSS syntax file is written to disk.
#'
#'@author Sebastian Weirich
#'
#'@examples
#'file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
#'dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
#'dat  <- convertLabel(dat)
#'createSpssSyntaxSnippet( dat = dat, file = file.path(tempdir(), "labels.txt"), keep = FALSE)
#'@export
createSpssSyntaxSnippet <- function ( dat , file, keep = TRUE ) {
  sink(file=file)
  ### Schritt 1: Variablenlabels
  cat("  VARIABLE LABELS\n")
  for ( i in 1:ncol(dat)) {
    varLab <- attr(dat[,i],"varLabel")
    if ( length(varLab)>0) { cat(paste0("    ",colnames(dat)[i], " \"",varLab,"\" \n"))}
  }
  cat(".\n \n")
  ### Schritt 2: Wertelabels
  for ( i in 1:ncol(dat)) {
    valLab <- attr(dat[,i],"valLabel")
    if ( length(valLab)>0) {
      if ( keep == FALSE ) {
        weg <- setdiff ( valLab, dat[,i])
        if ( length(weg)>0) { valLab <- valLab[-match(weg, valLab)] }
      }
      cat(paste(paste0("add value labels ", colnames(dat)[i]), valLab, paste0("'",names(valLab),"'"), sep=" ", collapse = ".\n"))
      cat(".\n")
    }
  }
  ### Schritt 3: nach dem letzten Wertelabel muss Schraegstrich durch Punkt ersetzt werden
  cat("EXECUTE.\n")
  sink()
}
