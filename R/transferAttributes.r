#' Transfer attributes from a origin data.frame to a target data.frame.
#'
#' Function allows to copy the attributes of (a subset of) common variables
#' from an origin data.frame to a target data.frame.
#'
#'@param origin the data.frame from which the attributes should be copied.
#'@param target the target data.frame to which the attributes should be copied.
#'@param whichVars Optional: character vector of variables which attributes should
#'be copied. If \code{NULL}, all common variables will be used.
#'@param whichAttrs Optional: character vector of attributes which should be copied.
#'If \code{NULL}, all attributes from origin data.frame variables will be used.
#'
#'@return The target data.frame with additional attributes
#'
#'@examples
#'mtcars2 <- mtcars
#'attr(mtcars[,"cyl"], "varLabel") <- "Number of cylinders"
#'mtcars2 <- transferAttributes(mtcars, mtcars2, whichVars = "cyl", whichAttrs="varLabel")
#'
#'@export
### wenn vars gleich NULL; werden alle Variablen genommen
transferAttributes <- function(origin, target, whichVars=NULL , whichAttrs = NULL){
       target <- makeDataFrame(target)
    ### wenn whichVars = NULL, dann alle gemeinsamen variablen nehmen
       if(is.null(whichVars)) {
          whichVars <- intersect(colnames(origin), colnames(target))
       } else {
          whichVars <- intersect(whichVars, intersect(colnames(origin), colnames(target)))
       }
       if (length(whichVars)==0) {
          message("No match between elements in 'whichVars' and common colnames of 'origin' and 'target'.")
          return(target)
       }
    ### ueber Variablen schleifen
       for ( i in whichVars) {
          from <- attributes(origin[,i])
          if(!is.null(whichVars)) {
             from <- from[which(names(from) %in% whichAttrs)]
          }
          if ( length(from) ==0) {
             message(paste0("Variable '",i,"': No common attributes between desired attributes specified in 'whichAttrs' and available attributes in 'attributes(origin)'."))
          } else {
             comm <- intersect(names(attributes(target[,i])), names(from))
             if (length(comm)>0) {  attributes(target[,i]) <- attributes(target[,i])[-match(comm, names(attributes(target[,i])))] }
             attributes(target[,i]) <- c(attributes(target[,i]), from)
          }
       }
       return(target)}
          