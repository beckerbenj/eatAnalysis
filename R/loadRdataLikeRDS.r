####
#############################################################################
#' Reload Saved rda or Rdata Datasets and assign them to a new object
#'
#' Function allows to load rda and Rdata objects like rds objects.
#' Hence, the function mimics the \code{\link[base]{readRDS}} functionality for rda and Rdata objects.
#'
#'@param file the name of the rda or Rdata file
#'
#'@return If the Rdata file contains only one R object, the object is returned. If the file contains
#'several objects, a list of these objects is returned.
#'
#'@export
loadRdataLikeRDS <- function(file) {
    char <- load(file)
    if ( length(char) == 1) {
         getC <- get(char)
    }  else  {
         message(paste0("   '",file,"' contains several objects: '", paste(char, collapse = "', '"), "'. Objects are stored as a list with length ", length(char)))
         getC <- lapply(char, FUN = function (y ) {get(y)})                     ### lapply(char, get) funktioniert komischerweise nicht (immer)
         names(getC) <- char
    }
    return(getC)}
