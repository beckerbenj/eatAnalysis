# xlsx-based Data viewer on a matrix-like R object

Invokes a xlsx-implemented viewer for a more handsome look of
matrix-like R objects especially under Linux.

## Usage

``` r
xview(
  dfr,
  name = NULL,
  large = FALSE,
  font = "Carlito",
  font_size = 11,
  headerBackground = "#7dcea0",
  stripColor = "#fadbd8",
  digits = 3,
  row.names = FALSE
)
```

## Arguments

- dfr:

  an R object which can be coerced to a data frame with non-zero numbers
  of rows and columns.

- name:

  Optional: name of the temporarily saved file (without file extension,
  see examples). May be useful if several files are opened in Excel to
  differentiate between them.

- large:

  Logical: If the data set is large, the procedure becomes very
  time-consuming. With `large = TRUE` a faster procedure (based on csv
  instead of Excel) is used that may be suitable for large datasets.
  However, the result does not look quite as nice as in Excel.

- font:

  character of the font which should be used. Font must be installed in
  the operating system

- font_size:

  Numeric value of font size for displaying the results

- headerBackground:

  hex code for background color of the column header, see
  https://htmlcolorcodes.com/

- stripColor:

  hex code for striped color of the rows, see
  https://htmlcolorcodes.com/

- digits:

  numerical value of decimal places to round to. If `NULL`, the values
  are not rounded.

- row.names:

  logical value indicating whether the row names of `dfr` are to be
  written along with `dfr`. Only works for csv files, i.e., if
  `large = TRUE`.

## Details

The R-based data viewer under Linux is not very clear and descriptive. A
better display can be achieved by temporarily saving the data object as
a formatted Excel file and then opening it with Excel or Libre Office.
For this purpose, functions of the R package openxlsx2 are used
internally. Please note that the procedure can be very slow for larger
data sets. In this case, use the option `large = TRUE` which generates
only a csv file instead of a formatted Excel file, which is considerably
faster for large data sets. It is recommended to use `large = TRUE` for
data sets with more than 1000 lines.

## Examples

``` r
if (FALSE) { # \dontrun{
xview(mtcars)
xview(mtcars, name = "mtcars")
} # }
```
