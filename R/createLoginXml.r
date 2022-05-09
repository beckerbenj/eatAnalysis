#' Creates xml login files from data.frame for computer-based assessment
#'
#' In computer-based assessment, logins for each student have to be created beforehand.
#' If logins and password are captured in a common data.frame, function creates several xml
#' files, one for each class or test group.
#'
#'@param dat data.frame with login information. See examples.
#'@param login Variable name or column number of the login column in \code{dat}. Login variable must not contain any missing values.
#'@param password Variable name or column number of the password column in \code{dat}. Passwords must not contain any missing values.
#'@param label Variable name or column number of the label column in \code{dat}. Missing values are not allowed.
#'@param group Optional: if several classes or test groups are stored in \code{dat}, \code{group} specifies variable name or column
#'number of the group variable in \code{dat}. If specified, missing values are not allowed.
#'@param class Variable name or column number of the class column in \code{dat}. Missing values are not allowed.
#'@param dir target directory for the xml files
#'@param prefix prefix for file name of the xml files
#'@param sep separator which separates prefix and remaining xml file name
#'@param booklet single string (length 1) for the desired booklet entry in the xml files. See example.
#'@param mode single string (length 1) for the desired mode entry in the xml files. See example.
#'@param login.mode single string (length 1) for the desired login mode entry in the xml files. See example.
#'@param cslb class size lower boundary: if classes have less than \code{cslb} students, a warning is given.
#'
#'@return No return, the files will be written on disk.
#'
#'@examples
#'file <- system.file("extdata", "logins.xlsx", package = "eatAnalysis")
#'dat  <- readxl::read_excel(file, sheet = "Tabelle1")
#'createLoginXml (dat=dat, login = "Name", password="Passwort", label = "Label",group = "groupID",
#'    dir = tempdir(), prefix = "logins", sep="_",booklet = "V8DeuTBAPilot2022TH15Faultier")
#'
#'@export
createLoginXml <- function(dat, login, password, label, group = NULL, class = "Klasse", dir, prefix = "test", sep="_", booklet, mode="run-hot-return", login.mode = "monitor-group", cslb = 5) {
     ### 'dat' muss data.frame sein
          dat   <- eatTools::makeDataFrame(dat)
          allVar<- list(login=login, password=password, label=label, group=group, class=class)
          allNam<- lapply(allVar, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = dat, variable=ii, warnIfMissing = TRUE)})
     ### verzeichnis checken
          dir   <- eatTools::crop(dir,"/")                                      ### wenn das Verzeichnis aber nicht existiert, wird es jetzt erzeugt
          if(dir.exists(dir) == FALSE) {
              cat(paste("Specified folder '",dir,"' does not exist. Create folder ... \n",sep=""))
              dir.create(dir, recursive = TRUE)
          }
     ### fuer jede gruppe separat. wenn es keine gruppen gibt, werden sie kuenstlich erzeugt, um mit by drueberschleifen zu koennen
          if (is.null(group)) {dat[,"grp"] <- 1; allNam[["group"]] <- "grp"}
     ### unerlaubte Zeichen aus labels loeschen
          dat[,allNam[["label"]]] <- gsub("_|\\.|-|/", "", as.character(dat[,allNam[["label"]]]) )
          xml   <- by(data = dat, INDICES = dat[,allNam[["group"]]], FUN = function (subdat){
                   if (nrow(subdat)>0) {
                       tlid<- checkXmlClass(subdat, an=allNam, cslb=cslb)
                       pre <- c("<?xml version=\"1.0\"?>", "<Testtakers>", "<Metadata>", "</Metadata>", paste("<Group id=\"",subdat[1,allNam[["group"]]],"\" label=\"",subdat[1,allNam[["label"]]],"\">", sep=""))
                       main<- paste0("<Login name=\"",subdat[which(subdat[,allNam[["class"]]] != tlid),allNam[["login"]]],"\" mode=\"",mode,"\" pw=\"",subdat[which(subdat[,allNam[["class"]]] != tlid),allNam[["password"]]],"\"> \n <Booklet>",booklet,"</Booklet> \n </Login>")
                       post<- c(paste0("<Login mode=\"",login.mode,"\" name=\"",subdat[which(subdat[,allNam[["class"]]] == tlid),allNam[["login"]]],"\" pw=\"",subdat[which(subdat[,allNam[["class"]]] == tlid),allNam[["password"]]],"\">"),"</Login>",	"</Group>","</Testtakers>")
                       all <- c(pre, main, post)
                       nam <- paste0(paste(prefix,unique(subdat[,allNam[["label"]]]), sep=sep), ".xml")
                       write(all, file = file.path(dir, nam), sep="\n")}  }) }

### Hilfsfunktion fuer createLoginXml()
checkXmlClass <- function ( d, an, cslb) {
     ### Anzahl Kinder in Klasse
          if(nrow(d) < 2) {stop(paste0("Group '",unique(d[,an[["group"]]]), "' with only ",nrow(d) , " unit(s)."))}
          if(nrow(d) <= cslb) {warning(paste0("Group '",unique(d[,an[["group"]]]), "' with only ",nrow(d) , " unit(s)."))}
     ### zwei units pro Klasse erlaubt
          if(!length(unique(d[,an[["class"]]])) == 2) {stop(paste0("Group '",unique(d[,an[["group"]]]),"'. Expect 2 distinct entries in '",an[["group"]],"' columns, found ",length(unique(d[,an[["class"]]])),": '",paste(unique(d[,an[["class"]]]), collapse="', '"),"'."))}
     ### eine unit darf genau einmal vorkommen
          if(!min(table(d[,an[["class"]]])) == 1) {cat(paste0("Error: One of the two occurrences in column '",an[["class"]],"' of group '",unique(d[,an[["group"]]]),"' must have frequency 1. Frequencies found:")); print(table(d[,an[["class"]]])); stop()}
          ret <- names(sort(table(d[,an[["class"]]])))[1]
          return(ret)}
