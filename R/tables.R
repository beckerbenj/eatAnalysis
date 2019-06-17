#### Prop table helper
#############################################################################
#' Create a standard relative frequency table.
#'
#' Create a simple relative frequency table with percentages.
#'
#' Function is meant to be able to easily \code{rbind} various tables.
#'
#'@param vec A vector.
#'@param useNA How should missing values be treated in the table? Possible values: \code{no}, \code{ifany}, \code{always}.
#'@param round_perc With how many decimals should the percantages be given?
#'
#'@return Returns a data.frame with 1 row.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
prop_table <- function(vec, useNA = "no", round_perc = 1) {
  stopifnot(is.vector(vec) || is.factor(vec))
  tab <- round(100 * prop.table(table(vec, useNA = useNA)), digits = round_perc)

  out_df <- as.data.frame(as.list(tab))
  names(out_df) <- names(tab)
  out_df
}



#### Prop table by and all
#############################################################################
#' Create a standard relative frequency table.
#'
#' Create a simple relative frequency table with percentages.
#'
#' Function is meant to be able to easily \code{rbind} various tables.
#'
#'@param vec A vector.
#'@param useNA How should missing values be treated in the table? Possible values: \code{no}, \code{ifany}, \code{always}.
#'@param round_perc With how many decimals should the percantages be given?
#'
#'@return Returns a data.frame with 1 row.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
prop_table_by_and_all <- function(df, av, by_var, vec, useNA = "no", round_perc = 1) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(av) && length(av) == 1)
  stopifnot(is.character(by_var) && length(by_var) == 1)

  gesamt <- prop_table(df[, av], useNA = useNA)
  out_by <- do.call(rbind, by(df, INDICES = df[, by_var], function(df) prop_table(df[, av], useNA = useNA)))
  out <- rbind(out_by, gesamt)
  rownames(out)[nrow(out)] <- "Gesamt"
  out
}
