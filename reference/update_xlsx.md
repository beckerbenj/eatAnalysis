# Update an `.xlsx` file.

Update the content of an existing `.xlsx` file adding and removing rows
in accordance to an identifier column. The content of the columns of
variables not in `idCol` is dropped.

## Usage

``` r
update_xlsx(newDat, filePath, sheetName, idCol)
```

## Arguments

- newDat:

  The new `data.frame`.

- filePath:

  The path to the `.xlsx` file..

- sheetName:

  The name of the sheet in the `.xlsx` file.

- idCol:

  Name of the unique identifier column.

## Value

A list or a `data.frame`.

## Details

This function is useful if, for example, a recode table or decision
table is written to `.xlsx` but the underlying data structure changes.
In such cases it might be useful to update the `.xlsx` table, this means
removing no longer existing variables and adding rows for new variables.
Use cases might be tables created by `eatGADS::getChangeMeta()` or
`eatFDZ::reverse_check_docu()`.

## Examples

``` r
# tbd
```
