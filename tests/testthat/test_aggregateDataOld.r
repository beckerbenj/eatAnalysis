
###
set.seed(4)
dat <- data.frame ( id = paste0("P", 1:10),
                    matrix(data = sample(x=0:1, size = 100, replace = TRUE),
                    nrow=10, ncol = 10))
### aggregate Item 0+1+2 and 4+5 and 8+9: define sequential letter
colnames(dat)[-1] <- c("I1a", "I1b", "I1c", "I2a", "I3a", "I3b",
                       "I4a", "I5a", "I6a", "I6b")

test_that("aggregateDataOld", {
  out <- capture_messages(agg <- aggregateDataOld(dat, -1))

  expect_equal(out, "Aggregate 10 variable(s) to 6 item(s).\n")

  expect_equal(class(agg), "list")
  expect_equal(length(agg), 3)
  expect_equal(names(agg), c("sum", "agg",  "pc.list"))
  expect_equal(agg$pc.list[, 1], c("I1", "I3", "I6"))
  expect_equal(agg$pc.list[, 2], c("1, 2, 3", "0, 1, 2", "0, 1, 2"))
  expect_equal(agg$pc.list[, "max"], c(3, 2, 2))

  expect_equal(as.numeric(agg$sum[1, -1]), c(1, 0, 2, 0, 1, 2))
  expect_equal(as.numeric(agg$sum[6, -1]), c(3, 1, 1, 1, 1, 2))
  expect_equal(as.numeric(agg$sum[10, -1]), c(2, 0, 2, 1, 1, 1))

  expect_equal(as.numeric(agg$agg[1, -1]), c(0, 0, 1, 0, 1, 1))
  expect_equal(as.numeric(agg$agg[6, -1]), c(1, 1, 0, 1, 1, 1))
  expect_equal(as.numeric(agg$agg[4, -1]), c(0, 0, 1, 0, 1, 0))
})

