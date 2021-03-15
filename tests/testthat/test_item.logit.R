test_that("item.logit", {
  z <- rnorm(100, 0, 2)
  d <- item.logit(z = z, slope = 4, thr = 0)
  expect_true(all(d$x %in% 0:1))
  expect_true(mean(d$x) > 0.4)
  expect_true(mean(d$x) < 0.6)
})

