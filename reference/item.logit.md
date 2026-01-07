# Simulates 1PL or 2PL responses

Simulates responses following a 1PL or 2PL model given item parameters
assuming normally distributed person parameters.

## Usage

``` r
item.logit(z, slope = 1, thr)
```

## Arguments

- z:

  A vector of item difficulties.

- slope:

  A vector of item discrimination parameters. Default is 1 and equals a
  1PL model.

- thr:

  A vector of item threshold parameters.

## Value

A list with two matrices. First matrix includes the responses, seconds
matrix includes the correct response probabilities.

## Author

Ulf Kroehne

## Examples

``` r
z <- rnorm(30, 0, 1)
d <- item.logit(z = z, thr = 0)
```
