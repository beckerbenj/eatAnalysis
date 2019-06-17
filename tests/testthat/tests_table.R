
context("Create result tables")


test_that("Prop table helper ", {
  out <- prop_table(mtcars$cyl)
  expect_identical(names(out), c("4", "6", "8"))
  expect_identical(as.numeric(out[1, ]), c(34.4, 21.9, 43.8))
})
