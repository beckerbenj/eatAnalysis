# Extract results

Extract results from an object created by
[`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) or
[`glmer`](https://rdrr.io/pkg/lme4/man/glmer.html) from the `lme4`
package.

## Usage

``` r
get.lmer.effects(
  lmerObj,
  bootMerObj = NULL,
  conf = 0.95,
  saveData = FALSE,
  quick = FALSE
)
```

## Arguments

- lmerObj:

  An object of class `merMod` or `glmerMod`, as created by
  [`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`glmer`](https://rdrr.io/pkg/lme4/man/glmer.html) from the `lme4`
  package.

- bootMerObj:

  Optional: An object of S3 class `boot`, as created by `bootMer`.
  Applies if standard error and/or confidence intervals from a bootstrap
  should be augmented to the `lme4` results object.

- conf:

  Applies if confidence intervals from a bootstrap should be augmented
  to the `lme4` results object. Define the upper bound of the confidence
  interval.

- saveData:

  Logical: Should the data frame be attached to the output as an
  attribute?

- quick:

  Logical: Sometimes, default method for computing \\R^2\\ by calling
  [`r.squaredGLMM`](https://rdrr.io/pkg/MuMIn/man/r.squaredGLMM.html) is
  very time-consuming. Using `quick = TRUE` forces the function to
  choose an alternative method which, however, is not available when
  `lmerTest` is attached. If `lmerTest` is attached, no \\R^2\\ is
  computed if `quick = TRUE`.

## Value

A data frame with at least 10 columns comprising the results of the GLMM
analysis.

- model:

  The name of the object the analysis results are assigned to.

- source:

  The lmer-function called

- var1:

  First variable name

- var2:

  Second variable name

- type:

  Type of variable and/or derived parameter

- group:

  The group a model parameter belongs to

- par:

  Name of the model parameter

- derived.par:

  Second name of the model parameter

- var2:

  Second variable name

- value:

  Corresponding numerical value

## Details

In principle, `get.lmer.effects` collects only output already contained
in the lme4-output. Additionally, the marginal and conditional r-squared
from Nakagawa and Schielzeth (2013) is provided. The parameters are
labeled `R2_m` and `R2_c` in the `par`-column.

## Author

Sebastian Weirich

## Examples

``` r
if (FALSE) { # \dontrun{
library ( lme4 )
### First example: GLMM analysis
fmVA <- glmer( r2 ~ Anger + Gender + btype + situ + (1|id) + (1|item),
               family = binomial, data = VerbAgg)
results    <- get.lmer.effects ( fmVA )

### second example: obtain standard errors and confidence intervals from the model estimated
### in the first example via bootstrap (using only 5 bootstrap samples for illustration)
### We use the 'bootMer' function fom the lme4 package
fmVAB<- bootMer(x = fmVA, FUN = get.lmer.effects.forBootMer, nsim = 5)
resultsBoot<- get.lmer.effects ( lmerObj = fmVA, bootMerObj = fmVAB, conf = .95, saveData = FALSE)
} # }
```
