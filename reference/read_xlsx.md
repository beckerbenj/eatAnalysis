# Read an `.xlsx` file.

Read an `.xlsx` file with multiple sheets.

## Usage

``` r
read_xlsx(filePath)
```

## Arguments

- filePath:

  The path to the output file.

## Value

A list or a `data.frame`.

## Details

If the `.xlsx` contains multiple sheets, the output is a list with all
sheets as `data.frame` entries in the list.

## Examples

``` r
f <- tempfile(fileext = ".xlsx")
write_xlsx(list(cars = mtcars, flowers = iris), filePath = f)
allDat <- read_xlsx(f)
```
