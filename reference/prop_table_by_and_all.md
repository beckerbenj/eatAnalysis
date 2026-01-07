# Create a standard relative frequency table.

Create a simple relative frequency table with percentages.

## Usage

``` r
prop_table_by_and_all(
  df,
  dep,
  by_var,
  weights = NULL,
  na.rm = TRUE,
  round_perc = 1
)
```

## Arguments

- df:

  A `data.frame`.

- dep:

  The name of the dependent variable in the `data.frame`.

- by_var:

  The name of the group variable in the `data.frame`.

- weights:

  The name of the weights variable in the `data.frame`.

- na.rm:

  Set to `FALSE` to suppress checking for `NAs`. If `TRUE`, `NAs` are
  removed from `x` as well as from `weights` prior to variance
  estimation.

- round_perc:

  With how many decimals should the percentages be given?

## Value

Returns a data.frame with 1 row.

## Details

Function is meant to be able to easily `rbind` various tables.

## Examples

``` r
# without weights
prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear")
#>          4    6    8
#> 3      6.7 13.3 80.0
#> 4     66.7 33.3   NA
#> 5     40.0 20.0 40.0
#> Total 34.4 21.9 43.8

# with weights
mtcars2 <- mtcars
mtcars2[, "weights"] <- rep(c(0.5, 1), 16)
prop_table_by_and_all(df = mtcars2, dep = "cyl", by_var = "gear", weights = "weights")
#>          4    6    8
#> 3      4.5 18.2 77.3
#> 4     68.4 31.6   NA
#> 5     42.9 28.6 28.6
#> Total 35.4 25.0 39.6
```
