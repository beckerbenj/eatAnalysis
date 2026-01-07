# Reload Saved rda or Rdata Datasets and assign them to a new object

Function allows to load rda and Rdata objects like rds objects. Hence,
the function mimics the [`readRDS`](https://rdrr.io/r/base/readRDS.html)
functionality for rda and Rdata objects.

## Usage

``` r
loadRdataLikeRDS(file)
```

## Arguments

- file:

  the name of the rda or Rdata file

## Value

If the Rdata file contains only one R object, the object is returned. If
the file contains several objects, a list of these objects is returned.
