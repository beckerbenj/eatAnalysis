
context("Create result tables")

# library(lme4)
# fmVA <- lme4::glmer( r2 ~ Anger + Gender + btype + situ + (1|id) + (1|item), family = binomial, data = VerbAgg)
# library(eatTools)
# results    <- eatTools::get.lmer.effects ( fmVA )
# saveRDS(results, "C:/Benjamin_Becker/02_Repositories/packages/eatAnalysis/tests/testthat/old_eatTools_out.RDS")
# saveRDS(fmVA, "C:/Benjamin_Becker/02_Repositories/packages/eatAnalysis/tests/testthat/lme4_out.RDS")

#tools_res <- readRDS("tests/testthat/old_eatTools_out.RDS")
tools_res <- readRDS("old_eatTools_out.RDS")
#fmVA <- readRDS("tests/testthat/lme4_out.RDS")
fmVA <- readRDS("lme4_out.RDS")

test_that("get.lmer.effects ", {
  out <- get.lmer.effects(fmVA)
  expect_equal(tools_res, out)
})

test_that("save.lmer.effects ", {
  out_file <- tempfile()
  save.lmer.effects(fmVA, file = out_file)
  out_file_in <- paste0(out_file, ".rda")
  load(out_file_in)

  expect_equal(tools_res[, -1], ret[, -1])
})
