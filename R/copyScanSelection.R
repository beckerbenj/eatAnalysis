####
#############################################################################
#' Copy booklet scan selection to desired directory
#'
#' Functions allows to copy the scans of a selection of variables,
#' sort by variable codes, into a desired directory.
#'
#'@param vars character string of variables whose scans should be copied.
#'@param dat wide format data which contain these variables. Names in \code{vars} must match column names of \code{dat}.
#'@param id column number or column name of the person identifier variable in the wide format data.
#'@param sourceDir The directory which contains the scans.
#'@param targetDir The target directory for the copied scans.
#'@param codebook either a character string with the folder of the corresponding excel file,
#'or an already imported data.frame of the IQB codebook.
#'@param startRow Optional: If codebook is provided as character string referring to an excel file,
#'\code{startRow} indicates the first line in the file which should be read in.
#'@param sheet character string containing the name of the excel sheet containing the codebook.
#'Only necessary if \code{codebook} was provided as character string.
#'@param varColumn character string of the variable identifier column name in the codebook.
#'@param bookletColumnPrefix Character string of the booklet identifier prefix in the codebook. The codebook
#'usually contains several booklet columns which should begin with a common identifier.
#'@param separators Character strings which separates booklet identifier and person identifier in the
#'filenames of the scans.
#'@param suffix Suffix of the filenames of the scans.
#'
#'@return No return, the files will be written on disk.
#'
#'@author Sebastian Weirich
#'
#'@examples
#' \dontrun{
#'# source directory
#'path <- "s:/Vera3-Scans/Deutsch/V3_Pilot_2015/Depot_100"
#'# target directory
#'target <- "N:/archiv/test"
#'# codebook folder
#'codebook <- "p:/R/Material/V3-2016_Codebook_Zoowaerter.xlsx"
#'# variable list
#'library(readxl)
#'vars <- read_excel("p:/R/Material/KA3_Variablennamen_Zoowaerter.xlsx",
#'                   sheet = "Tabelle1")
#'vars <- substr(unique(unlist(vars)),1,7)
#'# load data and reshape to the wide format
#'load("r:/VERA3/Deutsch/V3_DEU_2016/1_Pilotierung_2015/13_Auswertung und Itemselektion/02_Itemebene.rda")
#'dat <- reshape2::dcast(datAggL, ID~item, value.var = "value")
#'# select and copy scans
#'cop  <- copyScanSelection(vars=vars, dat=dat, id="ID", sourceDir=path,
#'                          targetDir=target, codebook=codebook, startRow = 1)
#'}
#'
#'@export
### foo <- copyScanSelection(codebook, vars=unlist(variablen), dat=dat, id=1, sourceDir = "s:/Vera3-Scans/Deutsch/V3_Pilot_2015/Depot_100", targetDir = "N:/archiv/test")
### prefixPattern = "TH[:digit:]{2}-[:digit:]{2}_[:digit:]{10}"
copyScanSelection <- function ( vars, dat, id, sourceDir, targetDir, codebook, startRow = 4, sheet = "Codebook", varColumn = "Variable", bookletColumnPrefix = "TH", separators = c("-", "_"), suffix = ".TIF") {
  if(length(id) != 1 ) {stop("Argument 'id' must be of length 1.\n",sep="")}
  if(class(vars) != "character") {stop("Argument 'vars' must be of class 'character'.\n",sep="")}
  if(length(vars) != length(unique(vars))) {stop("'vars' is not unique.\n")}
  allV <- list(ID = id, variablen=vars )
  allN <- lapply(allV, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = dat, variable=ii)})
  if (is.character(codebook)) {codebook <- data.frame ( readxl::read_excel(codebook, sheet = "Codebook", skip = startRow-1, col_types = "text"), stringsAsFactors = FALSE) }
  liste<- do.call("rbind", lapply (allN[["variablen"]], FUN = function (va) {
    codes <- setdiff(names(table(dat[,va])), c("mbd", "mnr", "mci", "mnr", "mir", "mbi", "9", "97", "98", "99", "7","8"))
    sepCod<- do.call("rbind", lapply(codes, FUN = function ( co ) {
      id <- dat[which(dat[,va] == co),allN[["ID"]] ]                  ### alle IDs raussuchen, die diesen code haben
      rw <- grep(va, codebook[,varColumn])                            ### in welchen testheften (Zeilen) kommt die variable "va" vor?
      if(length(rw) == 0) {stop(paste0("Cannot find variable '",va,"' in the codebook.\n"))}
      if ( length(setdiff(unique(codebook[rw,"Variable"]), "")) > 1) {
        cat(paste0("Item '",va,"' seems to be aggregated from '",paste(setdiff(unique(codebook[rw,"Variable"]), ""), collapse= "', '"),"'. \nScan selection is skipped as aggregated scores do not match variable raw scores.\n"))
        return(NULL)}
      th <- codebook[rw[1], grep(paste0("^", bookletColumnPrefix), colnames(codebook), value=TRUE)]
      if(all(is.na(th))) {
        cat(paste0("Variable '",va,"': codebook does not contain any valid information about booklet occurrences. All '",bookletColumnPrefix,"'-columns seem to be empty. Skip '",va,"'.\n"))
        return(NULL)}
      th <- th[which(!is.na(th))]
      str<- paste0(names(th), separators[1], unlist(th), separators[2])
      str<- expand.grid(str, id)
      str<- paste0(str[,1], str[,2])
      str<- paste0(str, suffix)
      dfr<- data.frame ( variable = va, code = co, scans = str, stringsAsFactors = FALSE)
      return(dfr)}))
    return(sepCod)}))                                                   ### jetzt aus 'liste' alle scans loeschen, die es gar nicht gibt
  scans<- list.files(path = sourceDir, pattern = paste0(suffix, "$"), recursive = TRUE)
  if ( length(scans)==0) {stop(paste0("Cannot found any scan files in source directory '",sourceDir,"'.\n"))}
  scan2<- eatTools::halveString(string = scans, pattern="/", first=FALSE)
  if (length(intersect(liste[,"scans"], scan2[,2])) == 0) {warning("keine Scans im Verzeichnis gefunden.\n")}
  weg  <- setdiff(liste[,"scans"], scan2[,2])
  if ( length(weg)>0) { liste <- liste[-eatTools::whereAre(weg, liste[,"scans"], quiet = TRUE),]}
  b    <- match(liste[,"scans"],scan2[,2])
  liste[,"quelle"] <- file.path(sourceDir, scans[b])
  if ( dir.exists(targetDir) == FALSE ) {dir.create(targetDir)}
  foo  <- lapply(unique(liste[,"variable"]), FUN = function(y) {
    dir.create(file.path(targetDir, y))                                 ### Verzeichnis fuer jede einzelne Variable erzeugen, und darunter: Unterverzeichnis fuer jeden einzelnen Code jeder einzelnen Variable erzeugen
    lapply(unique(liste[which(liste[,"variable"] == y),"code"]), FUN = function(co) { dir.create(file.path(targetDir, y, co))}) })
  foo2 <- file.copy(from = liste[,"quelle"], to = file.path(targetDir, liste[,"variable"], liste[,"code"], liste[,"scans"]))
  return(foo2)}
