# Creates xml login files from data.frame for computer-based assessment

In computer-based assessment, logins for each student have to be created
beforehand. If logins and password are captured in a common data.frame,
function creates several xml files, one for each class or test group.

## Usage

``` r
createLoginXml(
  dat,
  login,
  password,
  label,
  group = NULL,
  class = "Klasse",
  dir,
  prefix = "test",
  sep = "_",
  booklet,
  mode = "run-hot-return",
  login.mode = "monitor-group",
  cslb = 5
)
```

## Arguments

- dat:

  data.frame with login information. See examples.

- login:

  Variable name or column number of the login column in `dat`. Login
  variable must not contain any missing values.

- password:

  Variable name or column number of the password column in `dat`.
  Passwords must not contain any missing values.

- label:

  Variable name or column number of the label column in `dat`. Missing
  values are not allowed.

- group:

  Optional: if several classes or test groups are stored in `dat`,
  `group` specifies variable name or column number of the group variable
  in `dat`. If specified, missing values are not allowed.

- class:

  Variable name or column number of the class column in `dat`. Missing
  values are not allowed.

- dir:

  target directory for the xml files

- prefix:

  prefix for file name of the xml files

- sep:

  separator which separates prefix and remaining xml file name

- booklet:

  single string (length 1) for the desired booklet entry in the xml
  files. See example.

- mode:

  single string (length 1) for the desired mode entry in the xml files.
  See example.

- login.mode:

  single string (length 1) for the desired login mode entry in the xml
  files. See example.

- cslb:

  class size lower boundary: if classes have less than `cslb` students,
  a warning is given.

## Value

No return, the files will be written on disk.

## Examples

``` r
file <- system.file("extdata", "logins.xlsx", package = "eatAnalysis")
dat  <- readxl::read_excel(file, sheet = "Tabelle1")
createLoginXml (dat=dat, login = "Name", password="Passwort", label = "Label",group = "groupID",
   dir = tempdir(), prefix = "logins", sep="_",booklet = "V8DeuTBAPilot2022TH15Faultier")
#> Convert 'dat' of class 'tbl_df', 'tbl', 'data.frame' to a data.frame.
#> Specified folder 'tmp/RtmpA4ZkLI' does not exist. Create folder ... 
```
