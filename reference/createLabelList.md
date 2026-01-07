# Creates a data.frame with variable and value labels from variable attributes

By default,
[`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html) from the
foreign package uses variable.labels as attributes of the whole
`data.frame`, value.labels as attribute of each specific variable in the
`data.frame`.
[`convertLabel`](https://beckerbenj.github.io/eatAnalysis/reference/convertLabel.md)
provides variable and value labels as variable attributes.
`createLabelList` creates a data.frame with variable and value labels.

## Usage

``` r
createLabelList(dfr, additionalAttributes = NULL)
```

## Arguments

- dfr:

  A data.frame with variable and value labels stored as attributes using
  the convention of
  [`convertLabel`](https://beckerbenj.github.io/eatAnalysis/reference/convertLabel.md).

- additionalAttributes:

  Optional: Vector of names of additional attributes which should be
  collected in the returned data frame.

## Value

A `data.frame`.

## Author

Sebastian Weirich

## Examples

``` r
file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
dat  <- convertLabel(dat)
atts <- createLabelList(dat)
```
