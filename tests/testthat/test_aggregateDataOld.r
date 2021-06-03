context("Old version of the aggregation function implemented in eatPrep")

###
dat <- data.frame ( id = paste0("P", 11:50),
                    matrix(data = sample(x=0:1, size = 400, replace = TRUE),
                    nrow=40, ncol = 10))
### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b",
                       "I4a", "I5a", "I6a", "I6b")
agg <- aggregateDataOld(dat, -1)

load("aggregated.rda")
test_that("aggregateDataOld", {
  testthat::expect_equal(class(agg), "list")
  testthat::expect_equal(length(agg), 3)
  testthat::expect_equal(names(agg), c("sum", "agg",  "pc.list"))
  testthat::expect_equal(agg, agg_vorlage)
})

