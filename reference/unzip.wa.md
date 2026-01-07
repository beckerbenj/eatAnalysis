# Unzip files into desired directory

This is a workaround function for `unzip` which provides unexpected
errors especially with data bases. `unzip.wa` calls the `unzip.exe` via
a temporary batch file.

## Usage

``` r
unzip.wa(
  zipfile,
  targetdir = tempdir(),
  unzip.exe = "c:/Program Files/tools/unzip.exe"
)
```

## Arguments

- zipfile:

  Folder of the zip file which should be uncompressed.

- targetdir:

  The folder in which the content should be written.

- unzip.exe:

  The folder of the unzip.exe program.

## Value

A character string (snippet) useful for formula generation.

## Details

Function uses the unzip.exe which is freely available at
<http://stahlworks.com/dev/index.php?tool=zipunzip#zipexamp>

## Examples

``` r
if (FALSE) { # \dontrun{
### gives error
zip::unzip(zipfile = "q:/BT2016/BT/05_Anschreiben/03_Anschreiben_IQB_an_Eltern_Lehrer_Schulleiter/00_Archiv/20151110_finale_Unterlagen_Genehmigungsverfahren/LV_2016_Unterlagen_BB.zip", exdir=tempdir())
unzip.wa(zipfile = "q:/BT2016/BT/05_Anschreiben/03_Anschreiben_IQB_an_Eltern_Lehrer_Schulleiter/00_Archiv/20151110_finale_Unterlagen_Genehmigungsverfahren/LV_2016_Unterlagen_BB.zip")
} # }
```
