####
#############################################################################
#' Update an \code{.xlsx} file.
#'
#' Update the content of an existing \code{.xlsx} file adding and removing rows in accordance to an identifier column. The content
#' of the columns of variables not in \code{idCol} is dropped.
#'
#' This function is useful if, for example, a recode table or decision table is written to \code{.xlsx} but the underlying data structure changes.
#' In such cases it might be useful to update the \code{.xlsx} table, this means removing no longer existing variables and adding rows for
#' new variables. Use cases might be tables created by \code{eatGADS::getChangeMeta()} or \code{eatFDZ::reverse_check_docu()}.
#'
#'
#'@param newDat The new \code{data.frame}.
#'@param filePath The path to the \code{.xlsx} file..
#'@param sheetName The name of the sheet in the \code{.xlsx} file.
#'@param idCol Name of the unique identifier column.
#'
#'@examples
#' # tbd
#'
#'@return A list or a \code{data.frame}.
#'
#'@export
update_xlsx <- function(newDat, filePath, sheetName, idCol) {
  stopifnot(is.character(filePath) && length(filePath) == 1)
  stopifnot(grepl(".xlsx$", filePath))

  oldDat <- openxlsx::readWorkbook(filePath, sheet = sheetName)

  out <- merge(oldDat, newDat[, idCol, drop = FALSE], by = idCol,
                      all.x = FALSE, all.y = TRUE, sort = FALSE)
  out
}
