
#############################################################################
#' Create latex table code.
#'
#' Create latex code for a table from a \code{data.frame}.
#'
#' Prints the resulting latex code to console
#'
#'@param df A \code{data.frame}.
#'@param caption A single character, containing the table caption.
#'@param san Should all characters be sanitized.
#'@param label A single character, containing the table label (only for referencing in the \code{.tex} document.
#'@param note A single character, containing the table notes.
#'@param note_width A single numeric, containing the table note width.
#'
#'@return Returns \code{NULL}.
#'
#'@examples
#'df2tex_xtable(mtcars)
#'
#'@export
df2tex_xtable <- function(df, caption = "", san = TRUE, label = NULL,
                          note = NULL, note_width = 14) {
  rescFactor <- ifelse(ncol(df) > 5, 1 - (ncol(df) - 5) * 0.1, 1)
  xTab <- xtable::xtable(df, caption = caption, label = label)
  if(identical(san, TRUE)) san <- function(str) gsub("\\", "\\", str, fixed = TRUE)

  if(is.null(note)) {
    print(xTab, caption.placement = "top", include.rownames = FALSE,
          sanitize.text.function = san)
    # scalebox = as.character(rescFactor), sanitize.text.function = san)
    return(invisible(NULL))
  }

  comm <- paste0("\\hline \n \\multicolumn{", ncol(df), "}{p{", note_width, "cm}}",
                 "{\\small{\\textit{Note}: ", note, "}} \n")
  print(xTab, caption.placement = "top", include.rownames = FALSE,
        sanitize.text.function = san,
        hline.after=c(-1, 0), ## Remove hline after Note
        add.to.row = list(pos = list(nrow(df)), command = comm))
  return(invisible(NULL))
}

