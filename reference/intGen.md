# Generate interaction terms for `lm` formulas

Specify two-way or three-way interactions from a set of independent
variables. The function might be useful, if the user has a regression
model with more than two predictors and want to specify all main effects
plus all two-way interaction effects but no higher-order interaction
terms.

## Usage

``` r
intGen(vars, upto = 3)
```

## Arguments

- vars:

  Character vector of variables

- upto:

  The maximum way of interactions which should be created.

## Value

A character string (snippet) useful for formula generation.

## Author

Sebastian Weirich

## Examples

``` r
data(mtcars)
# relevant variables
vars <- c("cyl", "hp", "wt")
# cyl*hp*wt would specify two-way as well as three-way interactions
# generate formula snippet for two-way interactionn
frml <- intGen(vars = vars, upto = 2)
frml1<- as.formula(paste0("mpg ~ ", paste(vars, collapse=" + "), " + ",
        paste(frml, collapse=" + ")))
mod  <- lm(frml1, data = mtcars)
```
