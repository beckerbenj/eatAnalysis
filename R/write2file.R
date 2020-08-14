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

  if(is.data.frame(df_list)) df_list <- list(df_list)

  df_list <- lapply(df_list, function(x) x) # workaround problems with by
  openxlsx::write.xlsx(df_list, file = filePath,
                       sheetName = names(df_list), col.names = col.names, row.names = row.names)
}
