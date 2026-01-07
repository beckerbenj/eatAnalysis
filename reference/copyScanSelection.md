# Copy booklet scan selection to desired directory

Functions allows to copy the scans of a subset of variables, sort by
variable codes, into a desired directory. This might be useful, if, for
example, all scans where variable `"D122a"` was scored with 1 (correct)
should be collected in a common folder.

## Usage

``` r
copyScanSelection(
  vars,
  dat,
  id,
  sourceDir,
  targetDir,
  codebook,
  startRow = 4,
  sheet = "Codebook",
  varColumn = "Variable",
  bookletColumnPrefix = "TH",
  exclude = c("mbd", "mnr", "mci", "mnr", "mir", "mbi", "9", "97", "98", "99", "7", "8"),
  separators = c("-", "_"),
  suffix = ".TIF"
)
```

## Arguments

- vars:

  character string of variables whose scans should be copied. If
  missing, all available variables will be used.

- dat:

  wide format data which contain these variables. Names in `vars` must
  match column names of `dat`.

- id:

  column number or column name of the person identifier variable in the
  wide format data.

- sourceDir:

  The directory which contains the scans.

- targetDir:

  The target directory for the copied scans.

- codebook:

  either a character string with the folder of the corresponding excel
  file, or an already imported data.frame of the IQB codebook.

- startRow:

  Optional: If codebook is provided as character string referring to an
  excel file, `startRow` indicates the first line in the file which
  should be read in.

- sheet:

  character string containing the name of the excel sheet containing the
  codebook. Only necessary if `codebook` was provided as character
  string.

- varColumn:

  character string of the variable identifier column name in the
  codebook.

- bookletColumnPrefix:

  Character string of the booklet identifier prefix in the codebook. The
  codebook usually contains several booklet columns which should begin
  with a common identifier.

- exclude:

  Character string of codes which should be ignored for selection. If
  all codes should be used, type `exclude = ""` or `exclude = NULL`.

- separators:

  Two character strings must be given: First string separates booklet
  identifier and page identifier in the filenames of the scans. Second
  string separates page identifier from person identifier in the
  filenames of the scans.

- suffix:

  Suffix of the filenames of the scans. If the scan files have multiple
  suffixes, you can use `suffix = ".TIF|.tif"`, fo example.

## Value

No return, the files will be written on disk.

## Author

Sebastian Weirich

## Examples

``` r
if (FALSE) { # \dontrun{
# source directory
path <- "s:/Vera3-Scans/Deutsch/V3_Pilot_2015/Depot_100"
# target directory
target <- "N:/archiv/test"
# codebook folder
codebook <- "p:/R/Material/V3-2016_Codebook_Zoowaerter.xlsx"
# variable list
vars <- readxl::read_excel("p:/R/Material/KA3_Variablennamen_Zoowaerter.xlsx",
                  sheet = "Tabelle1")
vars <- substr(unique(unlist(vars)),1,7)
# load data and reshape to the wide format
load("r:/VERA3/Deutsch/V3_DEU_2016/1_Pilotierung_2015/13_Auswertung und Itemselektion/02_Itemebene.rda")
dat <- reshape2::dcast(datAggL, ID~item, value.var = "value")
# select and copy scans
cop  <- copyScanSelection(vars=vars, dat=dat, id="ID", sourceDir=path,
                         targetDir=target, codebook=codebook, startRow = 1)
} # }
```
