####
#############################################################################
#' Unzip files into desired directory
#'
#' This is a workaround function for \code{unzip} which provides
#' unexpected errors especially with data bases. \code{unzip.wa} calls the
#' \code{unzip.exe} via a temporary batch file.
#'
#' Function uses the unzip.exe which is freely available at
#' \url{http://stahlworks.com/dev/index.php?tool=zipunzip#zipexamp}
#'
#'@param zipfile Folder of the zip file which should be uncompressed.
#'@param targetdir The folder in which the content should be written.
#'@param unzip.exe The folder of the unzip.exe program.
#'
#'@return A character string (snippet) useful for formula generation.
#'
#'
#'@examples
#' \dontrun{
#'### gives error
#'zip::unzip(zipfile = "q:/BT2016/BT/05_Anschreiben/03_Anschreiben_IQB_an_Eltern_Lehrer_Schulleiter/00_Archiv/20151110_finale_Unterlagen_Genehmigungsverfahren/LV_2016_Unterlagen_BB.zip", exdir=tempdir())
### works fine
#'unzip.wa(zipfile = "q:/BT2016/BT/05_Anschreiben/03_Anschreiben_IQB_an_Eltern_Lehrer_Schulleiter/00_Archiv/20151110_finale_Unterlagen_Genehmigungsverfahren/LV_2016_Unterlagen_BB.zip")
#'}
#'
#'@export
unzip.wa <- function ( zipfile, targetdir = tempdir(), unzip.exe = "c:/Program Files/tools/unzip.exe") {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(targetdir)
  system(paste0("\"",normalizePath(unzip.exe),"\" \"",normalizePath(zipfile),"\""), show.output.on.console = TRUE)}

