#### Prop table helper
#############################################################################
#' Create a standard relative frequency table.
#'
#' Create a simple relative frequency table with percentages, optionally while using sampling weights.
#'
#'@param vec A vector.
#'@param weights A numeric vector of non-negative weights.
#'@param na.rm Set to \code{FALSE} to suppress checking for \code{NAs}. If \code{TRUE}, \code{NAs} are removed from \code{x} as well as from \code{weights} prior to variance estimation.
#'@param round_perc With how many decimals should the percentages be given?
#'
#'@return Returns a data.frame with 1 row.
#'
#'@examples
#'# without weights
#'prop_table(mtcars$cyl)
#'
#'# with weights
#' prop_table(mtcars$cyl, weights = rep(c(0.5, 1), 16))
#'
#'@export
prop_table <- function(vec, weights = NULL, na.rm = FALSE, round_perc = 1) {
  stopifnot(is.vector(vec) || is.factor(vec))

  if(is.null(weights)) weights <- rep(1, length(vec))

  #tab <- round(100 * prop.table(table(vec, useNA = useNA)), digits = round_perc)
  freqs <- eatTools::wtdTable(x = vec, weights = weights, na.rm = TRUE)
  relFreqs <- freqs/sum(freqs, na.rm = TRUE) * 100
  relFreqs <- round(relFreqs, digits = round_perc)

  out_df <- as.data.frame(as.list(relFreqs))
  names(out_df) <- names(freqs)
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
#'@param weights The name of the weights variable in the \code{data.frame}.
#'@param na.rm Set to \code{FALSE} to suppress checking for \code{NAs}. If \code{TRUE}, \code{NAs} are removed from \code{x} as well as from \code{weights} prior to variance estimation.
#'@param round_perc With how many decimals should the percentages be given?
#'
#'@return Returns a data.frame with 1 row.
#'
#'@examples
#'# without weights
#'prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear")
#'
#'# with weights
#'mtcars2 <- mtcars
#'mtcars2[, "weights"] <- rep(c(0.5, 1), 16)
#'prop_table_by_and_all(df = mtcars2, dep = "cyl", by_var = "gear", weights = "weights")
#'
#'@export
prop_table_by_and_all <- function(df, dep, by_var, weights = NULL, na.rm = TRUE, round_perc = 1) {
  if(!is.data.frame(df)) stop("'df' needs to be a data.frame.")
  if(!is.character(dep) || length(dep) != 1) stop("'dep' needs to be a character of length 1.")
  if(!is.character(by_var) || length(by_var) != 1) stop("'by_var' needs to be a character of length 1.")
  if(!is.null(weights) && (!is.character(weights) || length(weights) != 1)) stop("'weights' needs to be a character of length 1.")
  not_in_df <- setdiff(c(dep, by_var, weights), names(df))
  if(length(not_in_df) > 0) stop("The following variables are not in 'df': ", paste(not_in_df, collapse = ", "))


  if(is.null(weights)){
    df[, "artificial_weights"] <- rep(1, nrow(df))
    weights <- "artificial_weights"
  }

  gesamt <- prop_table(df[, dep], weights = df[, weights], na.rm = na.rm)
  split_tables <- by(df, INDICES = df[, by_var], function(subdf) {
    prop_table(subdf[, dep], na.rm = na.rm, weights = subdf[, weights])
  })
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
