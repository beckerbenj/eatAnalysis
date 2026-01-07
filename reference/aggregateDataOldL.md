# Aggregate variables to items and/or scales in long format data.frames.

This is the old version of the `aggregateData` function from the
`eatPrep` package. In contrast to
[`aggregateDataOld`](https://beckerbenj.github.io/eatAnalysis/reference/aggregateDataOld.md),
`aggregateDataOldL` accepts and returns long format data, i.e. one line
per variable/person ID combination. The function is currently deprecated
and was only kept in the package to keep older scripts executable. The
function might be beneficial if aggregation information from the IQB
database is not available.

## Usage

``` r
aggregateDataOldL(
  datLong,
  idCol,
  varCol,
  valueCol,
  varExclude = NULL,
  itemColName = "item",
  unexpected.pattern.as.na = TRUE,
  printCases = FALSE,
  printPattern = FALSE,
  inputList = NULL
)
```

## Arguments

- datLong:

  A data frame in the long format with at least three columns: person
  identifier, varable identifier and response.

- idCol:

  Name or number of the person identifier column in the long format data

- varCol:

  Name or number of the variable identifier column in the long format
  data

- valueCol:

  Name or number of the variable response column in the long format data

- varExclude:

  Optional: Numeric vector of variables which are to be excluded from
  aggregation in any case

- itemColName:

  Name of the item identifier column in the newly created aggregated
  data.frame. Name must not occur in the current data.frame.

- unexpected.pattern.as.na:

  Logical: TRUE, if non-valid patterns should be aggregated to NA.

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

A data.frame in the long format containing sum and aggregated values,
and maintaining all additional columns from the original data.frame if
its values do not vary between variables which belong to the same item.
The sum variable is named `"valueSum"`, the aggregated variable is names
`"valueAgg"`. The number of aggregated variables (i.e. the maximum
possible sum score per item) is captured in the variable `"valueMax"`.

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
dat <- data.frame ( id = paste0("P", 11:50),
       matrix(data = sample(x=0:1, size = 400, replace = TRUE),nrow=40, ncol = 10))
### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b", "I4a", "I5a", "I6a", "I6b")
### reshape data to the long format
datL<- reshape2::melt(dat, id.vars = "id")
agg <- aggregateDataOldL(datL,idCol="id", varCol="variable", valueCol="value")
#> Overall: 10 variables, 6 items. Aggregate 7 variables to 3 items.
#> Warning: the ‘isNested’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainter to do so.
#> This warning is displayed once per session.
```
