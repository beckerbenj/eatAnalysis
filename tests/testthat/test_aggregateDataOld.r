context("Old version of the aggregation function implemented in eatPrep")

###
dat <- data.frame ( id = paste0("P", 11:50),
                    matrix(data = sample(x=0:1, size = 400, replace = TRUE),
                    nrow=40, ncol = 10))
### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b",
                       "I4a", "I5a", "I6a", "I6b")

#load("tests/testthat/aggregated.rda")
load("aggregated.rda")
test_that("aggregateDataOld", {
  out <- capture_output(agg <- aggregateDataOld(dat, -1))

  testthat::expect_equal(class(agg), "list")
  testthat::expect_equal(length(agg), 3)
  testthat::expect_equal(names(agg), c("sum", "agg",  "pc.list"))
  testthat::expect_equal(agg$sum, agg_vorlage$sum)
  testthat::expect_equal(agg$agg, agg_vorlage$agg)
  testthat::expect_equal(agg$pc.list, agg_vorlage$pc.list)
})

