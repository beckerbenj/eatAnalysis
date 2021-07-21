####
#############################################################################
#' Read an \code{.xlsx} file.
#'
#' Read an \code{.xlsx} file with multiple sheets.
#'
#' If the \code{.xlsx} contains multiple sheets, the output is a list with all sheets as
#' \code{data.frame} entries in the list.
#'
#'@param filePath The path to the output file.
#'
#'@examples
#' f <- tempfile(fileext = ".xlsx")
#' write_xlsx(list(cars = mtcars, flowers = iris), filePath = f)
#' allDat <- read_xlsx(f)
#'
#'@return A list or a \code{data.frame}.
#'
#'@export
read_xlsx <- function(filePath) {
  stopifnot(is.character(filePath) && length(filePath) == 1)
  stopifnot(grepl(".xlsx$", filePath))

  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  out <- lapply(sheet_names, function(sheet_name) {
    x <- openxlsx::readWorkbook(filePath, sheet = sheet_name)
  })
  if(length(out) == 1) out <- out[[1]]

  out
}


#### Save to xlsx
#############################################################################
#' Save to \code{.xlsx}.
#'
#' Create an \code{.xlsx} file with multiple sheets.
#'
#' An existing file is overwritten.
#'
#'@param df_list A named list with \code{data.frames} to be written to \code{.xlsx}. The names of the list become the sheet names.
#'@param filePath The path to the output file.
#'@param col.names Logical: Should column names of the \code{data.frames} be written to file?
#'@param row.names Logical: Should row names of the \code{data.frames} be written to file?
#'
#'@examples
#' f <- tempfile(fileext = ".xlsx")
#' write_xlsx(mtcars, filePath = f)
#'
#'@export
write_xlsx <- function(df_list, filePath, row.names = FALSE, col.names = TRUE) {
  stopifnot(is.character(filePath) && length(filePath) == 1)
  stopifnot(is.list(df_list))
  stopifnot(!is.null(names(df_list)))
  stopifnot(grepl(".xlsx$", filePath))

  if(file.exists(filePath)) file.remove(filePath)

  if(is.data.frame(df_list)) df_list <- list(df_list)

  df_list <- lapply(df_list, function(x) x) # workaround problems with by
  openxlsx::write.xlsx(df_list, file = filePath,
                       sheetName = names(df_list), colNames = col.names, rowNames = row.names)
}
