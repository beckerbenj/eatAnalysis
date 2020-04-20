
context("Create result tables")


test_that("Prop table helper ", {
  out <- prop_table(mtcars$cyl)
  expect_identical(names(out), c("4", "6", "8"))
  expect_identical(as.numeric(out[1, ]), c(34.4, 21.9, 43.8))
})


test_that("Prop table by and all ", {
  out <- prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear")
  expect_identical(names(out), c("4", "6", "8"))
  expect_equal(dim(out), c(4, 3))
  expect_identical(as.numeric(out[1, ]), c(6.7, 13.3, 80))
})
