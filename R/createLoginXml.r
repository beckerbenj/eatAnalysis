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
createLoginXml <- function(dat, login, password, label, group = NULL, class = "Klasse", dir, prefix = "test", sep="_", booklet, mode="run-hot-return", login.mode = "monitor-group") {
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
          if (is.null(group)) {dat[,"grp"] <- 1; allVar[["group"]] <- "grp"}
     ### unerlaubte Zeichen aus labels loeschen
          dat[,allVar[["label"]]] <- gsub("_|\\.|-|/", "", as.character(dat[,allVar[["label"]]]) )
          xml   <- by(data = dat, INDICES = dat[,allVar[["group"]]], FUN = function (subdat){
                   stopifnot(length(unique(subdat[,allVar[["class"]]])) == 2)
                   stopifnot(min(table(subdat[,allVar[["class"]]])) == 1)
                   stopifnot(length(unique(subdat[,allVar[["label"]]])) == 1)
                   tlid<- names(sort(table(subdat[,allVar[["class"]]])))[1]
                   pre <- c("<?xml version=\"1.0\"?>", "<Testtakers>", "<Metadata>", "</Metadata>", paste("<Group id=\"",subdat[1,allVar[["group"]]],"\" label=\"",subdat[1,allVar[["label"]]],"\">", sep=""))
                   main<- paste0("<Login name=\"",subdat[which(subdat[,allVar[["class"]]] != tlid),allVar[["login"]]],"\" mode=\"",mode,"\" pw=\"",subdat[which(subdat[,allVar[["class"]]] != tlid),allVar[["password"]]],"\"> \n <Booklet>",booklet,"</Booklet> \n </Login>")
                   post<- c(paste0("<Login mode=\"",login.mode,"\" name=\"",subdat[which(subdat[,allVar[["class"]]] == tlid),allVar[["login"]]],"\" pw=\"",subdat[which(subdat[,allVar[["class"]]] == tlid),allVar[["password"]]],"\">"),"</Login>",	"</Group>","</Testtakers>")
                   all <- c(pre, main, post)
                   nam <- paste0(paste(prefix,unique(subdat[,allVar[["label"]]]), sep=sep), ".xml")
                   write(all, file = file.path(dir, nam), sep="\n")  }) }