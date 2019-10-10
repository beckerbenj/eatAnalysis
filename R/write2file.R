#### Save to xlsx
#############################################################################
#' Save to xlsx with multiple sheets.
#'
#' Create an xlsx file with multiple sheets.
#'
#' An existing file is overwritten
#'
#'@param df_list A named list with data.frames to be written to xlsx. The names of the list become the sheet names.
#'@param filePath The path to the output file.
#'@param col.names A logical value indicating whether the column names of x are to be written along with x to the file.
#'@param row.names A logical value indicating whether the row names of x are to be written along with x to the file.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
write_xlsx <- function(df_list, filePath, row.names = TRUE, col.names = TRUE) {
  stopifnot(is.character(filePath) && length(filePath) == 1)
  stopifnot(is.list(df_list))
  stopifnot(!is.null(names(df_list)))
  stopifnot(grepl(".xlsx$", filePath))

  if(file.exists(filePath)) file.remove(filePath)

  df_list <- lapply(df_list, function(x) x) # workaround problems with by
  openxlsx::write.xlsx(df_list, file = filePath,
                       sheetName = names(df_list), col.names = col.names, row.names = row.names)
}
