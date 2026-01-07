# Create a standard frequency table.

Create a standard frequency table that can be written to excel.

## Usage

``` r
pretty_table(x, x_label, useNA = "ifany")
```

## Arguments

- x:

  A vector.

- x_label:

  Label for the x variable?

- useNA:

  How should missing values be treated in the table? Possible values:
  `no`, `ifany`, `always`.

## Value

Returns a data.frame with 2 columns, the levels of x and its
frequencies.

## Details

Standard `table` output is very ugly when written to excel. This
function creates an easy to write to excel object.

## Examples

``` r
pretty_table(iris$Species, "Flower Species")
#>   Flower Species Frequency
#> 1         setosa        50
#> 2     versicolor        50
#> 3      virginica        50
```
