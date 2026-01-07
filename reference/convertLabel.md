# Transform SPSS variable and value labels.

By default,
[`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html) from the
`foreign` package uses variable labels as attributes of the whole
`data.frame`, value labels as attribute of each specific variable in the
`data.frame`. `convertLabel` provides variable and value labels as
variable attributes.

## Usage

``` r
convertLabel(spssList, stringsAsFactors = TRUE, useZkdConvention = TRUE)
```

## Arguments

- spssList:

  An object created by
  [`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html).
  Important: Using
  [`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html),
  `to.data.frame` has to be `FALSE`.

- stringsAsFactors:

  Transform character variables into factors?

- useZkdConvention:

  Logical: If `TRUE`. variable labels are named `'varLabel'`, value
  labels are named `'valLabel'`.

## Value

A `data.frame`.

## Author

Sebastian Weirich

## Examples

``` r
file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
dat  <- convertLabel(dat)
str(dat)
#> 'data.frame':    279 obs. of  20 variables:
#>  $ TRAINING: num  1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "value.labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Experimentalgruppe" "Kontrollgruppe"
#>   ..- attr(*, "varLabel")= chr "Zugeh\xf6rigkeit zur Trainings- oder Experimentalgruppe"
#>   ..- attr(*, "valLabel")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Experimentalgruppe" "Kontrollgruppe"
#>  $ WST11   : num  10 9 17 15 15 14 14 16 12 10 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Teil 1 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ WST21   : num  11 9 17 12 14 17 12 17 14 10 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Teil 2 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ WST1    : num  21 18 34 27 29 31 26 33 26 20 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Gesamttest (Addition von WST11 und WST21); Pr\xe4test"
#>  $ CFT11   : num  11 16 24 24 26 17 17 17 18 13 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Teil 1 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ CFT21   : num  9 15 23 21 23 13 16 19 13 12 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Teil 2 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ CFT1    : num  20 31 47 45 49 30 33 36 31 25 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Gesamttest (Addition von CFT11 und CFT21); Pr\xe4test"
#>  $ CPM11   : num  9 8 16 15 13 13 9 14 12 7 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Teil 1 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ CPM21   : num  8 5 15 15 14 10 9 8 12 7 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Teil 2 (bei Zufallsaufteilung des Tests); Pr\xe4test"
#>  $ CPM1    : num  17 13 31 30 27 23 18 22 24 14 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Gesamttest (Addition von CPM11 und CPM21); Pr\xe4test"
#>  $ WST12   : num  10 10 17 17 16 16 14 16 12 10 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Teil 1 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ WST22   : num  11 13 17 17 12 17 13 16 12 11 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Teil 2 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ WST2    : num  21 23 34 34 28 33 27 32 24 21 ...
#>   ..- attr(*, "varLabel")= chr "Wortschatztest; Gesamttest (Addition von WST12 und WST22); Posttest 1"
#>  $ CFT12   : num  20 24 33 31 33 22 23 30 28 24 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Teil 1 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ CFT22   : num  21 24 29 33 29 20 23 28 27 22 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Teil 2 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ CFT2    : num  41 48 62 64 62 42 46 58 55 46 ...
#>   ..- attr(*, "varLabel")= chr "Grundintellingenztest CFT 1 Skala 1; Gesamttest (Addition von CFT12 und CFT22); Posttest 1"
#>  $ CPM12   : num  14 12 21 22 21 17 15 19 16 13 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Teil 1 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ CPM22   : num  14 8 20 20 18 15 15 16 17 13 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Teil 2 (bei Zufallsaufteilung des Tests); Posttest 1"
#>  $ CPM2    : num  28 20 41 42 39 32 30 35 33 26 ...
#>   ..- attr(*, "varLabel")= chr "Raven-Matrizen-Test; Gesamttest; Posttest 1"
#>  $ RCI_WST : num  0 1.659 0 2.323 -0.332 ...
#>   ..- attr(*, "varLabel")= chr ""
```
