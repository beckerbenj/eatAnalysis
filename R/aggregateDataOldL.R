####
#############################################################################
#' Aggregate variables to items and/or scales in long format data.frames.
#'
#' This is the old version of the \code{aggregateData} function from the \code{eatPrep}
#' package. In contrast to \code{\link{aggregateDataOld}}, \code{aggregateDataOldL} accepts and returns
#' long format data, i.e. one line per variable/person ID combination. The function is currently deprecated
#' and was only kept in the package to keep older scripts executable. The function might be beneficial if
#' aggregation information from the IQB database is not available.
#'
#' The function use a rather simple aggregation rule: all variables which share a common ``stem''
#' are considered to belong together. The ``stem'' is everything except the last sign. By convention,
#' the last sign may be a letter in ascending order, for example \code{"I1a", "I1b", "I1c"}. However,
#' \code{"I12", "I1_", "I1j"} is also possible although less convenient. The item \code{"I1"} consists
#' of three variables and is considered to be correct if all variables are correct. See examples for
#' further details. Note: if \code{inputList} is specified, aggregation rules are executed as specified
#' in the ZKD input list.
#'
#'@param datLong A data frame in the long format with at least three columns: person identifier, varable identifier and response.
#'@param idCol Name or number of the person identifier column in the long format data
#'@param varCol Name or number of the variable identifier column in the long format data
#'@param valueCol Name or number of the variable response column in the long format data
#'@param varExclude Optional: Numeric vector of variables which are to be excluded from aggregation in any case
#'@param itemColName Name of the item identifier column in the newly created aggregated data.frame. Name must not occur in the current data.frame.
#'@param unexpected.pattern.as.na Logical: TRUE, if non-valid patterns should be aggregated to NA.
#'@param printCases Logical: Specifies whether exhaustive aggregation information should be printed on console.
#'@param printPattern Logical: Print the unexpected patterns to console?
#'@param inputList Optional: Input list to differentiate between variables (sub items) and items. If \code{NULL},
#'all variables in the data frame which share the same ID except for the last sign are considered
#'to belong to the same item.
#'
#'@return A data.frame in the long format containing sum and aggregated values, and maintaining all additional
#'columns from the original data.frame if its values do not vary between variables which belong to the same item.
#'The sum variable is named \code{"valueSum"}, the aggregated variable is names \code{"valueAgg"}. The number
#'of aggregated variables (i.e. the maximum possible sum score per item) is captured in the variable
#'\code{"valueMax"}.
#'
#'@examples
#' ### create artificial data
#' dat <- data.frame ( id = paste0("P", 11:50),
#'        matrix(data = sample(x=0:1, size = 400, replace = TRUE),nrow=40, ncol = 10))
#' ### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
#' colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b", "I4a", "I5a", "I6a", "I6b")
#' ### reshape data to the long format
#' datL<- reshape2::melt(dat, id.vars = "id")
#' agg <- aggregateDataOldL(datL,idCol="id", varCol="variable", valueCol="value")
#'
#'@export
aggregateDataOldL<- function (datLong, idCol, varCol, valueCol, varExclude = NULL, itemColName = "item", unexpected.pattern.as.na = TRUE, printCases = FALSE, printPattern = FALSE, inputList = NULL ) {
        datLong     <- eatTools::makeDataFrame(datLong)
        allVars     <- list(idCol = idCol, varCol = varCol, valueCol=valueCol)
        all.Names   <- lapply(allVars, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = datLong, variable=ii)})
        if(length(all.Names) != length(unique(all.Names)) ) {stop("'idCol', 'varCol', and 'valueCol' overlap.\n")}
        forbidden   <- c("valueSum", "valueAgg", "partialCredit", "valueMax", "numOfVars", "aggregationRule", itemColName)
        if ( sum(forbidden %in% colnames(datLong))>0) { stop(paste0("Columns in data.frame must not be named '", paste(forbidden[which(forbidden %in% colnames(datLong))], collapse= "', '"), "'"))}
        if(!inherits(datLong[,all.Names[["valueCol"]]], c("numeric", "integer"))) {stop ( "'valueCol' must be numeric or integer.")}
        foo         <- table(nchar(as.character(datLong[,all.Names[["varCol"]]])))# Haben alle Variablennamen die gleiche Anzahl Zeichen?
        if(length(foo)>1) {message("Variable names with mixed numbers of characters.")}
        if (is.null(inputList) ) {                                              ### es gibt keine Inputliste: Aggregierung nach Standardverfahren
            datLong[,itemColName] <- substr(as.character(datLong[,all.Names[["varCol"]]]),1,nchar(as.character(datLong[,all.Names[["varCol"]]]))-1)
        }  else  {                                                              ### untere Zeile: no aggregation rule
            nag    <- setdiff ( unique(as.character(datLong[,all.Names[["varCol"]]])), inputList[["subunits"]][,"subunit"])
            if ( length( nag) > 0 ) {
                warning(paste0("Following ",length(nag), " from ",length(unique(as.character(datLong[,all.Names[["varCol"]]])))," variables(s) without aggregation rule in ZKD input list:\n   '",paste(nag, collapse = "', '"), "'.\nThese varables(s) won't be aggregated to items."))
            }                                                                   ### Wenn es zwar eine zkd-Inputliste gibt, aber fuer manche Items keine Aggregierungsvorschrift,
            items  <- unique(inputList[["subunits"]][which(inputList[["subunits"]][,"subunit"] %in% unique(as.character(datLong[,all.Names[["varCol"]]]))),c("subunit", "unit")])
            if ( nrow(items) != length(unique(items[,1])) ) {stop("'subunit' column of <inputList>$subunits is not unique.")}
            colnames(items) <- c(all.Names[["varCol"]],itemColName)             ### werden die Items nicht nach Standardaggregierung aggregiert, sondern gar nicht.
            datLong<- eatTools::mergeAttr(datLong, items, by = all.Names[["varCol"]], all.x = TRUE, all.y = FALSE, setAttr=FALSE)
            datLong[,"aggregationRule"] <- car::recode(datLong[,"item"], "NA=FALSE; else=TRUE")
            if(length(which(is.na(datLong[,itemColName])))>0) {datLong[which(is.na(datLong[,itemColName])),itemColName] <- datLong[which(is.na(datLong[,itemColName])),all.Names[["varCol"]]]}
        }                                                                       ### Es werden dann einfach die Variablen uebernommen. Das mergen setzt NAs in der Itemspalte, wenn es keine Aggregierungsvorschrift gibt. Die NAs werden dann mit dem Wert der Variablenspalte aufgefuellt
     ### Die Variablen, die NICHT aggregiert werden sollen, schreibt man einfach 1:1 in die itemspalte, dann landen sie unten bei noAgg
        if(!is.null(varExclude)) {
            if(!inherits(varExclude, "character")) {stop("'varExclude' must be a character vector.")}
            if(length(varExclude) != length(unique(varExclude))) {message("'varExclude' include duplicated elements. Use only unique elements.")}
            comm <- intersect(unique(varExclude), unique(datLong[,all.Names[["varCol"]]]))
            if ( length(comm) != length(unique(varExclude))) {message(paste0("Elements '", paste(setdiff(unique(varExclude),unique(datLong[,all.Names[["varCol"]]])), collapse="', '"), "' do not occur in data.frame."))}
            if ( length(comm)>0) {
                 datLong[eatTools::whereAre(comm, datLong[,all.Names[["varCol"]]], verbose=FALSE),itemColName] <- datLong[eatTools::whereAre(comm, datLong[,all.Names[["varCol"]]], verbose=FALSE),all.Names[["varCol"]]]
            }
        }
     ### jetzt checken: welche muessen aggregiert werden, welche koennen einfach so uebernommen werden?
        toAgg       <- names(which(rowSums(table(datLong[,c(itemColName,all.Names[["varCol"]])]) != 0)>1))
        noAgg       <- setdiff(unique(as.character(datLong[,itemColName])), toAgg)
        message(paste0("Overall: ",length(unique(as.character(datLong[,all.Names[["varCol"]]]))), " variables, ", length(unique(as.character(datLong[,itemColName]))), " items. Aggregate ", length(unique(datLong[which(datLong[,itemColName] %in% toAgg), all.Names[["varCol"]]])), " variables to ", length(toAgg) , " items."))
        if (printCases) { print(table(eatTools::facToChar(datLong[which(datLong[,itemColName] %in% toAgg),c(all.Names[["varCol"]], itemColName)]))) }
     ### nicht zu aggregierende Daten sammeln
        datNoAgg    <- datLong[which(datLong[,itemColName] %in% noAgg),]
        datNoAgg    <- data.frame ( datNoAgg, valueSum = datNoAgg[,all.Names[["valueCol"]]], valueAgg = datNoAgg[,all.Names[["valueCol"]]], partialCredit=FALSE, valueMax = NA, stringsAsFactors = FALSE)
     ### zu aggregierende Daten aggregieren: Achtung! die unexpected missing pattern findet man im Langformat weniger gut, weil es ja (ggf.) keine NAs in den Zeilen gibt!
     ### so ein unexpected pattern wuerde man nur daran erkennen, dass fuer eine person 4 variablen (= item responses) pro item existieren, fuer eine andere aber nur 3
     ### wenn der Datensatz allerdings so gestaltet ist, dass es im langformat NAs gibt, dann koennen diese pattern auch so auftreten.
     ### es gibt also zweierlei moeglichkeiten, wie diese pattern im Datensatz auftreten koennen ... schwierig ...
        datAgg      <- doAggLong(datLong=datLong, all.Names=all.Names,itemColName=itemColName,printPattern=printPattern, toAgg=toAgg, printCases=printCases, unexpected.pattern.as.na=unexpected.pattern.as.na, inputList=inputList)
        datAgg      <- eatTools::rbind_common(datNoAgg, do.call(eatTools::rbind_common, datAgg))
        return(datAgg)}

doAggLong <- function(datLong, all.Names,itemColName,printPattern, toAgg, printCases, unexpected.pattern.as.na, inputList){
        datLong <- eatTools::facToChar(datLong[which(datLong[,itemColName] %in% toAgg),])
        datAgg  <- by(data = datLong, INDICES = datLong[,itemColName], FUN = function (i ) {
                   frm<- paste0(all.Names[["idCol"]], " ~ ", all.Names[["varCol"]])
                   iw <- reshape2::dcast(i, stats::as.formula(frm), value.var = all.Names[["valueCol"]])
                   foo<- checkUnexpectedPattern(iw[,-1, drop=FALSE], item.i = i[1,itemColName], printCases=printCases, printPattern=printPattern)
                   i[,"numOfVars"] <- ncol(iw) - 1
                   agg<- do.call(eatTools::rbind_common, by(i, INDICES = i[,all.Names[["idCol"]]], FUN = function (j) {
                         colsWeg <- unique(c(names(which(sapply(j, FUN = function (x) {length(unique(x)) != 1 }))), all.Names[["varCol"]], all.Names[["valueCol"]]))
                         if(nrow(j) < j[1,"numOfVars"]) {vec <- c(j[,all.Names[["valueCol"]]], rep(NA, times =j[1,"numOfVars"]-nrow(j)))} else{vec <- j[,all.Names[["valueCol"]]]}
                         summe   <- ifelse( all(is.na(vec)), NA, sum(vec, na.rm=!unexpected.pattern.as.na))
                         if (is.null(inputList)) {
                             agg     <- ifelse(summe == length(vec),1,0)
                         }  else  {
                             sl  <- inputList[["unitRecodings"]][which(inputList[["unitRecodings"]][,"unit"] == unique(j[,itemColName])),]
                             if ( nrow(sl) == 0 ) {stop(paste0("Cannot find aggregation rule in 'unitRecodings' sheet of the input list for item '",unique(j[,itemColName]),"'."))}
                             recstr <- paste("'",sl[,"value"] , "' = '" , sl[,"valueRecode"],"'",sep="", collapse="; ")
                             agg <- car::recode ( summe, recstr)
                         }
                         ret     <- data.frame ( j[1,-match(colsWeg, colnames(j))], valueSum = summe, valueAgg = agg, partialCredit = TRUE, valueMax = j[1,"numOfVars"], stringsAsFactors = FALSE)
                         return(ret)}))
                   return(agg)})
        return(datAgg)}

### Funktion hat keine Rueckgabe, checkt nur
checkUnexpectedPattern <- function(sub.dat, item.i, printCases, printPattern){
        isNA         <- table(rowSums(is.na(sub.dat)))
        isNA.names   <- as.numeric(names(isNA))
        unexpected   <- setdiff(isNA.names, c(0,ncol(sub.dat)))
    # if ( substr(colnames(sub.dat)[1], 1, 8) == "M3621603") {browser()}
        if( length( unexpected ) > 0  )   {
          cases      <- sum(as.numeric(isNA[as.character(unexpected)]))
          message(paste0("Caution! Found unexpected missing pattern in variables for item ",item.i, " in ",cases," cases.") )
          whichUnexp <- which( rowSums(is.na(sub.dat)) %in% unexpected)
          if (printCases)   {message(paste0("   Cases in question: ", paste(whichUnexp, collapse=", ")))}
          if (printPattern) {
              patt <- apply(sub.dat[whichUnexp,], MARGIN = 1, FUN = function ( zeile ) { paste(zeile, collapse = "_")})
              print(table(patt))
          }}}

