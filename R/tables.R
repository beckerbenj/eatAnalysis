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
#'@param round_perc With how many decimals should the percentages be given?
#'
#'@return Returns a data.frame with 1 row.
#'\describe{
#' \item{A}{The name of the object the analysis results are assigned to.}
#' \item{b}{The lmer-function called}
#'}
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
#'@param df A \code{data.frame}.
#'@param dep The name of the dependent variable in the \code{data.frame}.
#'@param by_var The name of the group variable in the \code{data.frame}.
#'@param useNA How should missing values be treated in the table? Possible values: \code{no}, \code{ifany}, \code{always}.
#'@param round_perc With how many decimals should the percentages be given?
#'
#'@return Returns a data.frame with 1 row.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
prop_table_by_and_all <- function(df, dep, by_var, useNA = "no", round_perc = 1) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(dep) && length(dep) == 1)
  stopifnot(is.character(by_var) && length(by_var) == 1)

  gesamt <- prop_table(df[, dep], useNA = useNA)
  split_tables <- by(df, INDICES = df[, by_var], function(df) prop_table(df[, dep], useNA = useNA))
  out_by <- do.call(plyr::rbind.fill, split_tables)
  rownames(out_by) <- names(split_tables)
  out <- rbind(out_by, gesamt)
  rownames(out)[nrow(out)] <- "Total"
  out
}



#### Simple frequency table for saving as excel
#############################################################################
#' Create a standard frequency table.
#'
#'  Create a standard frequency table that can be written to excel.
#'
#' Standard \code{table} output is very ugly when written to excel. This function creates an easy to write to excel object.
#'
#'@param x A vector.
#'@param x_label Label for the x variable?
#'@param useNA How should missing values be treated in the table? Possible values: \code{no}, \code{ifany}, \code{always}.
#'
#'@return Returns a data.frame with 2 columns, the levels of x and its frequencies.
#'
#'@examples
#'pretty_table(iris$Species, "Flower Species")
#'
#'@export
pretty_table <- function(x, x_label, useNA = "ifany") {
  stopifnot(is.character(x_label) && length(x_label) == 1)
  stopifnot(is.character(useNA) && length(useNA) == 1)

  tab <- table(x, useNA = useNA)
  out <- as.data.frame(tab)
  names(out) <- c(x_label, "Frequency")
  out
}
