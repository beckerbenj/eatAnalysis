% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.lmer.effects.R
\name{save.lmer.effects}
\alias{save.lmer.effects}
\title{Save lme4-output to disc}
\usage{
save.lmer.effects(lmerObj, lmerObjRestrict = NULL, fileName, scipen = 6)
}
\arguments{
\item{lmerObj}{An \code{lme4} object to be saved.}

\item{lmerObjRestrict}{Optional: a second (restricted) lme4 analysis output. Needs to be nested to the first one. Intended if model comparison should be saved likewise.}

\item{fileName}{Name of the file for the output (without file extension).}

\item{scipen}{Maximum number of decimal places before exponential notation is used.}
}
\value{
No output is returned to console. Two files are created in the desired directory.
}
\description{
The output of any lme4 analysis is saved in a memory saving manner.
}
\examples{
\dontrun{
library(lme4)
### Example from the help page of lmer().
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
save.lmer.effects(fm1, fileName = tempfile())

### with model comparison
### specify a "null model" nested in the first one: no fixed effects, only intercept
fm0 <- lmer(Reaction ~ (Days | Subject), sleepstudy)
save.lmer.effects(lmerObj = fm1, lmerObjRestrict = fm0, fileName = tempfile())
}
}
\author{
Sebastian Weirich
}
