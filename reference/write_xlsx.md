# Save to `.xlsx`.

Create an `.xlsx` file with multiple sheets.

## Usage

``` r
write_xlsx(df_list, filePath, row.names = FALSE, col.names = TRUE)
```

## Arguments

- df_list:

  A named list with `data.frames` to be written to `.xlsx`. The names of
  the list become the sheet names.

- filePath:

  The path to the output file.

- row.names:

  Logical: Should row names of the `data.frames` be written to file?

- col.names:

  Logical: Should column names of the `data.frames` be written to file?

## Details

An existing file is overwritten.

## Examples

``` r
f <- tempfile(fileext = ".xlsx")
write_xlsx(mtcars, filePath = f)
```
