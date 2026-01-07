# Aggregate variables to items and/or scales.

This is the old version of the `aggregateData` function from the
`eatPrep` package. The function is currently deprecated and was only
kept in the package to keep older scripts executable. For the same
reason, inappropriate argument names of `aggregateDataOld` have not been
modified. The function might be beneficial if aggregation information
from the IQB database is not available.

## Usage

``` r
aggregateDataOld(
  all.daten,
  spalten,
  unexpected.pattern.as.na = TRUE,
  printCases = FALSE,
  printPattern = FALSE,
  inputList = NULL
)
```

## Arguments

- all.daten:

  A data frame in the wide format with at least two (dichotomous)
  variable columns.

- spalten:

  Column names or numbers with variables to aggregate.

- unexpected.pattern.as.na:

  Logical: `TRUE`, if non-valid patterns should be aggregated to `NA`.

- printCases:

  Logical: Specifies whether exhaustive aggregation information should
  be printed on console.

- printPattern:

  Logical: Print the unexpected patterns to console?

- inputList:

  Optional: Input list to differentiate between variables (sub items)
  and items. If `NULL`, all variables in the data frame which share the
  same ID except for the last sign are considered to belong to the same
  item.

## Value

A list. First element is a data frame with sum scores. Second element is
a data frame with aggregated scores. Third element is a data frame with
information how many variables are summarized for each item.

## Details

The function use a rather simple aggregation rule: all variables which
share a common “stem” are considered to belong together. The “stem” is
everything except the last sign. By convention, the last sign may be a
letter in ascending order, for example `"I1a", "I1b", "I1c"`. However,
`"I12", "I1_", "I1j"` is also possible although less convenient. The
item `"I1"` consists of three variables and is considered to be correct
if all variables are correct. See examples for further details. Note: if
`inputList` is specified, aggregation rules are executed as specified in
the ZKD input list.

## Examples

``` r
### create artificial data
dat <- data.frame(id = paste0("P", 11:50),
                  matrix(data = sample(x=0:1, size = 400, replace = TRUE),
                  nrow=40, ncol = 10))
### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b",
                       "I4a", "I5a", "I6a", "I6b")
agg <- aggregateDataOld(dat, -1)
#> Aggregate 10 variable(s) to 6 item(s).
```
