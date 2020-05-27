
### mergen mit Attributen, das kann 'merge()' nicht:
### http://stackoverflow.com/questions/20306853/maintain-attributes-of-data-frame-columns-after-merge
####
#############################################################################
#' Merge Two Data Frames and maintain variable attributes.
#'
#' This is a wrapper for the \code{merge} function from the \code{base}
#' package. \code{merge} does not maintain variable attributes. \code{mergeAttr} might
#' be useful if variable attributes should be maintained.
#'
#'@param x first data frame to be merged.
#'@param y second data frame to be merged.
#'@param by specifications of the columns used for merging
#'@param by.x specifications of the columns used for merging
#'@param by.y specifications of the columns used for merging
#'@param all logical; \code{all = L} is shorthand for \code{all.x = L} and \code{all.y = L},
#'where \code{L} is either \code{TRUE} or \code{FALSE}.
#'@param all.x logical; if \code{TRUE}, then extra rows will be added to the output, one for each
#'row in \code{x} that has no matching row in \code{y}. These rows will have \code{NAs}
#'in those columns that are usually filled with values from \code{y}. The default
#'is \code{FALSE}, so that only rows with data from both \code{x} and \code{y} are
#'included in the output.
#'@param all.y logical; analogous to \code{all.x}.
#'@param sort logical. Should the result be sorted on the \code{by} columns?
#'@param suffixes a character vector of length 2 specifying the suffixes to be used for making unique
#'the names of columns in the result which not used for merging (appearing in \code{by} etc).
#'@param setAttr Logical: restore the variable attributes? If FALSE, the behavior of \code{mergeAttr} equals
#'the behavior of \code{merge}.
#'@param onlyVarValLabs Logical: If TRUE, only the variable and value labels will be restored. If FALSE, all
#'variable attributes will be restored.
#'@param homoClass Logical: Beginning with R version 3.5, \code{merge} may give an error if the class of the
#'by-variables differs in both data.frames. If TRUE, class of by-variable(s) will be homogenized
#'before merging.
#'
#'@return A \code{data.frame}. See the help page of \code{merge} for further details. .
#'
#'@author Sebastian Weirich
#'
#'@examples
#'### data frame 1, variable 'y' with variable.label 'test participation'
#'df1 <- data.frame ( id = 1:3, sex = factor ( c("male", "male", "female")), y = c(TRUE,FALSE,FALSE))
#'attr(df1[,"y"], "variable.label") <- "test participation"
#'
#'### data frame 2 without labels
#'df2 <- data.frame ( id = c(2,4), status = factor ( c("married", "single")), z = c(TRUE,FALSE))
#'
#'### lost label after merging
#'df3 <- merge(df1, df2, all = TRUE)
#'attr(df3[,"y"], "variable.label")
#'
#'### maintain label
#'df4 <- mergeAttr(df1, df2, all = TRUE, onlyVarValLabs = FALSE)
#'attr(df4[,"y"], "variable.label")
#'@export
mergeAttr <- function ( x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x",".y"), setAttr = TRUE, onlyVarValLabs = TRUE, homoClass = TRUE) {
     ### erstmal von allen by-variablen die Klassen homogenisieren, falls gewuenscht
             byvars<- data.frame ( x=by.x, y=by.y, clx = sapply(x[,by.x,drop=FALSE], class), cly = sapply(y[,by.y,drop=FALSE], class), stringsAsFactors = FALSE)
             for ( i in 1:nrow(byvars) ) {
                   if ( length(unique(unlist(byvars[i,c("clx", "cly")]))) > 1 ) {
                        if ( isTRUE(homoClass)) {
                            cat(paste0("   Merging variable pair '", paste(unlist(byvars[i,c("x", "y")]), collapse = "'<==>'"), "' has different classes: '", paste(unlist(byvars[i,c("clx", "cly")]), collapse = "'<==>'"),"'. Classes will be homogenized to 'character'.\n   Use 'homoClass = FALSE' to depreciate this behavior.\n"))
                            if ( byvars[i,"clx"] != "character" ) { x[, byvars[i,"x"]] <- as.character(x[, byvars[i,"x"]]) }
                            if ( byvars[i,"cly"] != "character" ) { y[, byvars[i,"y"]] <- as.character(y[, byvars[i,"y"]]) }
                        }  else  {
                            cat(paste0("   Merging variable pair '", paste(unlist(byvars[i,c("x", "y")]), collapse = "'<==>'"), "' has different classes: '", paste(unlist(byvars[i,c("clx", "cly")]), collapse = "'<==>'"),"'.\n   Use 'homoClass = TRUE' to homogenize classes.\n"))
                        }
                   }
             }
     ### jetzt mergen und DANACH die Attribute rekonstruieren
             datM  <- merge ( x=x, y=y, by.x=by.x, by.y=by.y, all=all, all.x=all.x, all.y=all.y, sort=sort, suffixes =suffixes)
             if ( setAttr == TRUE ) {
                   dats<- list(x=x, y=y)
                   for ( d in names(dats)) {
                         for ( v in colnames(dats[[d]])) {
                               vsuf <- paste0(v, suffixes[2])
                               if ( vsuf %in% colnames(datM) ) {
                                    if ( onlyVarValLabs == FALSE ) {
                                         attributes(datM[,vsuf]) <- attributes(dats[[d]][,v])
                                    }  else  {
                                         attr(datM[,vsuf], "varLabel") <- attr(dats[[d]][,v], "varLabel")
                                         attr(datM[,vsuf], "valLabel") <- attr(dats[[d]][,v], "valLabel")
                                    }
                               }  else  {
                                    if ( v %in% colnames(datM) ) {
                                         if ( onlyVarValLabs == FALSE ) {
                                              attributes(datM[,v]) <- attributes(dats[[d]][,v])
                                         }  else  {
                                              attr(datM[,v], "varLabel") <- attr(dats[[d]][,v], "varLabel")
                                              attr(datM[,v], "valLabel") <- attr(dats[[d]][,v], "valLabel")
                                         }
                                    }
                               }
                         }
                   }
             }
             return(datM)}
