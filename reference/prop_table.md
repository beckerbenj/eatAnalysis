# Create a standard relative frequency table.

Create a simple relative frequency table with percentages, optionally
while using sampling weights.

## Usage

``` r
prop_table(vec, weights = NULL, na.rm = FALSE, round_perc = 1)
```

## Arguments

- vec:

  A vector.

- weights:

  A numeric vector of non-negative weights.

- na.rm:

  Set to `FALSE` to suppress checking for `NAs`. If `TRUE`, `NAs` are
  removed from `x` as well as from `weights` prior to variance
  estimation.

- round_perc:

  With how many decimals should the percentages be given?

## Value

Returns a data.frame with 1 row.

## Examples

``` r
# without weights
prop_table(mtcars$cyl)
#>      4    6    8
#> 1 34.4 21.9 43.8

# with weights
prop_table(mtcars$cyl, weights = rep(c(0.5, 1), 16))
#>      4  6    8
#> 1 35.4 25 39.6
```
