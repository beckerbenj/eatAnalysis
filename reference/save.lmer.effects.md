# Save lme4-output to disc

The output of any lme4 analysis is saved in a memory saving manner.

## Usage

``` r
save.lmer.effects(
  lmerObj,
  lmerObjRestrict = NULL,
  fileName,
  scipen = 6,
  standardized = FALSE,
  digits = 3,
  quick = FALSE
)
```

## Arguments

- lmerObj:

  An `lme4` object to be saved.

- lmerObjRestrict:

  Optional: a second (restricted) lme4 analysis output. Needs to be
  nested to the first one. Intended if model comparison should be saved
  likewise.

- fileName:

  Name of the file for the output (without file extension).

- scipen:

  Maximum number of decimal places before exponential notation is used.

- standardized:

  Logical: Additionally print standardized coefficients to the output?

- digits:

  Number of digits for printing standardized results

- quick:

  Logical: Sometimes, default method for computing \\R^2\\ by calling
  `r.squaredGLMM` is very time-consuming. Using `quick = TRUE` forces
  the function to choose an alternative method which, however, is not
  available when `lmerTest` is attached. If `lmerTest` is attached, no
  \\R^2\\ is computed if `quick = TRUE`.

## Value

No output is returned to console. Two files are created in the desired
directory.

## Author

Sebastian Weirich

## Examples

``` r
if (FALSE) { # \dontrun{
library(lme4)
### Example from the help page of lmer().
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
save.lmer.effects(fm1, fileName = tempfile())

### with model comparison
### specify a "null model" nested in the first one: no fixed effects, only intercept
fm0 <- lmer(Reaction ~ (Days | Subject), sleepstudy)
save.lmer.effects(lmerObj = fm1, lmerObjRestrict = fm0, fileName = tempfile())
} # }
```
