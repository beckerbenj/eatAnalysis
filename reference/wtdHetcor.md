# Weighted heterogeneous correlation matrix.

Computes a weighted heterogeneous correlation matrix, consisting of
Pearson product-moment correlations between numeric variables,
polyserial correlations between numeric and ordinal variables, and
polychoric correlations between ordinal variables.

## Usage

``` r
wtdHetcor(
  dataFrame,
  vars = NULL,
  weights = NULL,
  out = c("wide", "long", "both"),
  triangular = FALSE
)
```

## Arguments

- dataFrame:

  a data.frame containing all variables

- vars:

  character or numeric vector indicating the variables for which a
  correlation table should be computed. If `NULL`, all variables in the
  data.frame will be used.

- weights:

  character or numeric vector indicating the column in `dataFrame` which
  contains numeric non-negative weights. If `NULL`, equally weighted
  cases are assumed, i.e. all weights are defaulted to 1.

- out:

  Specifies the output format. `"wide"` gives a classical correlation
  matrix, `"long"` gives a long format table which includes the type of
  correlation.

- triangular:

  Logical: should the wide-format matrix be arranged in triangular
  shape?

## Value

a correlation table or a list

## Details

Variables in the data.frame should be accordingly classified as numeric
or factor variables. Function resembles the `hetcor` function from the
`polycor` package, but allows for incorporating weights. For this
purpose, the function makes use of the
[`weightedCorr`](https://american-institutes-for-research.github.io/wCorr/reference/weightedCorr.html)
function from the `wCorr` package.

## Examples

``` r
data(mtcars)
# create arbitrary weights
mtcars[,"weight"] <- abs(rnorm(nrow(mtcars), 10,5))
# choose variables
vars <- c("mpg", "cyl", "hp")
# inappropriate classes: variables which are inherently ordinal, have the 'wrong'
# class 'numeric'.
sapply(mtcars[,vars], class)
#>       mpg       cyl        hp 
#> "numeric" "numeric" "numeric" 
mtcars[,"cyl"] <- as.factor(mtcars[,"cyl"])
wtdHetcor(mtcars, vars = vars, out = "long")
#>   Var1 Var2  class1  class2     method nPairs        cor
#> 1  cyl   hp  factor numeric Polyserial     32  0.9587446
#> 2  cyl  mpg  factor numeric Polyserial     32 -0.9695361
#> 3   hp  mpg numeric numeric    Pearson     32 -0.7761684
wtdHetcor(mtcars, vars = vars, weights = "weight", out = "long")
#>   Var1 Var2  class1  class2     method nPairs        cor
#> 1  cyl   hp  factor numeric Polyserial     32  0.9742075
#> 2  cyl  mpg  factor numeric Polyserial     32 -0.9525372
#> 3   hp  mpg numeric numeric    Pearson     32 -0.7957354
```
