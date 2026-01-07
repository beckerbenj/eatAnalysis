# Transfer attributes from a origin data.frame to a target data.frame.

Function allows to copy the attributes of (a subset of) common variables
from an origin data.frame to a target data.frame.

## Usage

``` r
transferAttributes(origin, target, whichVars = NULL, whichAttrs = NULL)
```

## Arguments

- origin:

  the data.frame from which the attributes should be copied.

- target:

  the target data.frame to which the attributes should be copied.

- whichVars:

  Optional: character vector of variables which attributes should be
  copied. If `NULL`, all common variables will be used.

- whichAttrs:

  Optional: character vector of attributes which should be copied. If
  `NULL`, all attributes from origin data.frame variables will be used.

## Value

The target data.frame with additional attributes

## Examples

``` r
mtcars2 <- mtcars
attr(mtcars[,"cyl"], "varLabel") <- "Number of cylinders"
mtcars2 <- transferAttributes(mtcars, mtcars2, whichVars = "cyl", whichAttrs="varLabel")
```
