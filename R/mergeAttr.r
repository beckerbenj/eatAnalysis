
### mergen mit Attributen, das kann 'merge()' nicht:
### http://stackoverflow.com/questions/20306853/maintain-attributes-of-data-frame-columns-after-merge
####
#############################################################################
#' Merge Two Data Frames and maintain variable attributes.
#'
#' This is a wrapper for the \code{\link[base]{merge}} function. \code{merge}
#' does not maintain variable attributes. \code{mergeAttr} might be useful if variable
#' attributes should be maintained. For example, if SPSS data are imported via
#' \code{\link[foreign]{read.spss}}, variable and value labels are stored
#' as attributes which get lost if data are merged subsequently. Moreover, function gives
#' additional messages if (combination of) by-variables are not unique in at least one data.frame,
#' or if by-variables have different classes, or if some units of the by-variables are missing in
#' one of the data sets. Users are free to specify which kind of messages are desirable.
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
#'@param onlyVarValLabs If TRUE, only the variable and value labels as captured by \code{read.spss} and
#'stored by \code{\link{convertLabel}} will be restored. If FALSE, all variable attributes will be restored.
#'@param homoClass Logical: Beginning with R version 3.5, \code{merge} may give an error if the class of the
#'by-variables differs in both data.frames. If TRUE, class of by-variable(s) will be homogenized
#'before merging.
#'@param unitName Optional: Set the name for the unit variable to get more informative messages. This is mainly
#'relevant if \code{mergeAttr} is called from other functions.
#'@param xName Optional: Set the name for the x data.frame to get more informative messages. This is mainly
#'relevant if \code{mergeAttr} is called from other functions.
#'@param yName Optional: Set the name for the y data.frame to get more informative messages. This is mainly
#'relevant if \code{mergeAttr} is called from other functions.


#'@param verbose Optional: Choose whether messages concerning missing levels in by-variables should be printed
#'on console (\code{"match"}), or messages concerning uniqueness of by-variables (\code{"unique"}),
#'or messages concerning different classes of by-variables (\code{"class"}), or messages concerning
#'appropriate class (\code{data.frame}) of \code{x} and \code{y} (\code{"dataframe"}). Multiple choices
#'are possible, e.g. \code{verbose = c("match", "class")}. If \code{verbose = TRUE}, all
#'messages are printed, if \code{verbose = FALSE}, no messages are printed at all. The default
#'is equivalent to \code{verbose = TRUE}.
#'
#'@return A \code{data.frame}. See the help page of \code{merge} for further details. .
#'
#'@author Sebastian Weirich
#'
#'@examples
#'### data frame 1, variable 'y' with variable.label 'test participation'
#'df1 <- data.frame ( id = 1:3, sex = factor ( c("male", "male", "female")),
#'       happy = c("low", "low", "medium"))
#'attr(df1[,"happy"], "variable.label") <- "happieness in the workplace"
#'
#'### data frame 2 without labels
#'df2 <- data.frame ( id = as.factor(c(2,2,4)), status = factor ( c("married", "married", "single")),
#'       convicted = c(FALSE, FALSE, TRUE))
#'
#'### lost label after merging
#'df3 <- merge(df1, df2, all = TRUE)
#'attr(df3[,"happy"], "variable.label")
#'
#'### maintain label
#'df4 <- mergeAttr(df1, df2, all = TRUE, onlyVarValLabs = FALSE)
#'attr(df4[,"happy"], "variable.label")
#'
#'### adapt messages
#'df5 <- mergeAttr(df1, df2, all = TRUE, onlyVarValLabs = FALSE, unitName = "student",
#'       xName = "student questionnaire", yName = "school questionnaire",
#'       verbose = c("match", "unique"))
#'@export
### mergen mit Attributen, das kann 'merge()' nicht:
### http://stackoverflow.com/questions/20306853/maintain-attributes-of-data-frame-columns-after-merge
mergeAttr <- function ( x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x",".y"), setAttr = TRUE, onlyVarValLabs = TRUE, homoClass = TRUE, unitName = "unit", xName = "x", yName = "y", verbose = c("match", "unique", "class", "dataframe")) {
     ### verbose setzen
             verb  <- setVerbose(verbose, choices = c("match", "unique", "class", "dataframe"))
     ### das muessen data.frames sein
             if(length(class(x))>1 || !"data.frame" %in% class(x)) {
                if ("dataframe" %in% verb) {message(paste0("'",xName,"' must be a data.frame. Convert '",xName,"' to data.frame by applying `data.frame(x)`.") )}
                x <- data.frame(x, stringsAsFactors = FALSE)
             }
             if(length(class(y))>1 || !"data.frame" %in% class(y)) {
                if ("dataframe" %in% verb) {message(paste0("'",yName,"' must be a data.frame. Convert '",yName,"' to data.frame by applying `data.frame(y)`."))}
                y <- data.frame(y, stringsAsFactors = FALSE)
             }
             byvars<- data.frame ( x=by.x, y=by.y, clx = sapply(x[,by.x,drop=FALSE], class), cly = sapply(y[,by.y,drop=FALSE], class), stringsAsFactors = FALSE)
     ### pruefen, ob die level der by-variablen in dem anderen datensatz enthalten sind
             levs  <- apply(X=byvars, MARGIN = 1, FUN = function (v) {
                      nix <- setdiff(unique(y[,v[["y"]]]), unique(x[,v[["x"]]]))
                      if ("match" %in% verb) {if(length(nix)>0) {message(paste0(length(nix), " ",unitName,"(s) of merging variable '",v[["y"]],"' from data set '",yName,"' not included in data set '",xName,"'."))}}
                      niy <- setdiff(unique(x[,v[["x"]]]), unique(y[,v[["y"]]]))
                      if ("match" %in% verb) {if(length(niy)>0) {message(paste0(length(niy), " ",unitName,"(s) of merging variable '",v[["x"]],"' from data set '",xName,"' not included in data set '",yName,"'."))}} })
     ### pruefen, ob die level der by-variablen unique sind
             if ( nrow(byvars)>1) {
                   xby <- unlist(plyr::alply(x, .margins = 1, .fun = function (z) {paste(as.vector(unlist(eatTools::set.col.type(z[,byvars[,"x"]], col.type = list("character" = byvars[,"x"])))),collapse="_")}))
                   yby <- unlist(plyr::alply(y, .margins = 1, .fun = function (z) {paste(as.vector(unlist(eatTools::set.col.type(z[,byvars[,"y"]], col.type = list("character" = byvars[,"y"])))),collapse="_")}))
             }  else  {
                   xby <- x[,byvars[1,"x"]]
                   yby <- y[,byvars[1,"y"]]
             }
             if ("unique" %in% verb) {if ( length(xby) != length(unique(xby))) { message(paste0("Merging levels are not unique in data set '",xName,"'."))}}
             if ("unique" %in% verb) {if ( length(yby) != length(unique(yby))) { message(paste0("Merging levels are not unique in data set '",yName,"'."))}}
     ### von allen by-variablen die Klassen homogenisieren, falls gewuenscht
             for ( i in 1:nrow(byvars) ) {
                   if ( length(unique(unlist(byvars[i,c("clx", "cly")]))) > 1 ) {
                        if ( isTRUE(homoClass)) {
                            if ("class" %in% verb) {message(paste0("   Merging variable pair '", paste(unlist(byvars[i,c("x", "y")]), collapse = "'<==>'"), "' has different classes: '", paste(unlist(byvars[i,c("clx", "cly")]), collapse = "'<==>'"),"'. Classes will be homogenized to 'character'.\n   Use 'homoClass = FALSE' to suppress this behavior."))}
                            if ( byvars[i,"clx"] != "character" ) { x[, byvars[i,"x"]] <- as.character(x[, byvars[i,"x"]]) }
                            if ( byvars[i,"cly"] != "character" ) { y[, byvars[i,"y"]] <- as.character(y[, byvars[i,"y"]]) }
                        }  else  {
                            if ("class" %in% verb) {message(paste0("   Merging variable pair '", paste(unlist(byvars[i,c("x", "y")]), collapse = "'<==>'"), "' has different classes: '", paste(unlist(byvars[i,c("clx", "cly")]), collapse = "'<==>'"),"'.\n   Use 'homoClass = TRUE' to homogenize classes."))}
                        }
                   }
             }
     ### jetzt mergen und DANACH die Attribute rekonstruieren
             datM  <- merge ( x=x, y=y, by.x=by.x, by.y=by.y, all=all, all.x=all.x, all.y=all.y, sort=sort, suffixes =suffixes)
             if ( isTRUE(setAttr) ) {
                   dats<- list(x=x, y=y)
                   for ( d in names(dats)) {
                         for ( v in colnames(dats[[d]])) {
                               vsuf <- paste0(v, suffixes[2])
                               if ( vsuf %in% colnames(datM) ) {
                                    if ( onlyVarValLabs == FALSE ) {
                                         if(!is.null(attributes(dats[[d]][,v]))) {attributes(datM[,vsuf]) <- attributes(dats[[d]][,v])}
                                    }  else  {
                                         if(!is.null(attr(dats[[d]][,v], "varLabel"))) {attr(datM[,vsuf], "varLabel") <- attr(dats[[d]][,v], "varLabel")}
                                         if(!is.null(attr(dats[[d]][,v], "valLabel"))) {attr(datM[,vsuf], "valLabel") <- attr(dats[[d]][,v], "valLabel")}
                                    }
                               }  else  {
                                    if ( v %in% colnames(datM) ) {
                                         if ( onlyVarValLabs == FALSE ) {
                                              if(!is.null(attributes(dats[[d]][,v]))) {attributes(datM[,v]) <- attributes(dats[[d]][,v])}
                                         }  else  {
                                              if(!is.null(attr(dats[[d]][,v], "varLabel"))) {attr(datM[,v], "varLabel") <- attr(dats[[d]][,v], "varLabel")}
                                              if(!is.null(attr(dats[[d]][,v], "valLabel"))) {attr(datM[,v], "valLabel") <- attr(dats[[d]][,v], "valLabel")}
                                         }
                                    }
                               }
                         }
                   }
             }
             return(datM)}

### hilfsfunktion fuer mergeAttr()
setVerbose <- function(verbose, choices){
    if(is.logical(verbose)) {
       stopifnot(length(verbose) == 1)
       if ( isTRUE(verbose)) {
            verbose <- choices
       } else {
            verbose <- ""
       }
    }  else  {
       verbose <- match.arg(verbose, choices = choices, several.ok = TRUE)
    }
    return(verbose)}
