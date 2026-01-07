# Prepare SPSS syntax to transfer variable and value labels into an SPSS data set

Usually, variable and value labels are lost if ASCII data is imported
into SPSS. Variable and value labels normally must be reestablished
using SPSS syntax file. The function prepares an SPSS syntax snippet
from the variable and value labels stored as attributes in R.

## Usage

``` r
createSpssSyntaxSnippet(dat, file, keep = TRUE)
```

## Arguments

- dat:

  R data frame. Variable and value labels must be stored in attributes
  as provided, for example, by the function
  [`convertLabel`](https://beckerbenj.github.io/eatAnalysis/reference/convertLabel.md).

- file:

  Character string with the name of syntax file which should be created.

- keep:

  Logical: Create value labels even if the value does not occur in the
  data? For example, if `keep = TRUE`, `createSpssSyntaxSnippet` will
  write labels also for males, even if the data only contains females.

## Value

No return, the SPSS syntax file is written to disk.

## Author

Sebastian Weirich

## Examples

``` r
file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
dat  <- convertLabel(dat)
createSpssSyntaxSnippet( dat = dat, file = file.path(tempdir(), "labels.txt"), keep = FALSE)
```
