
test_that("Pretty table", {
  out <- pretty_table(mtcars$cyl, x_label = "var")
  expect_equal(names(out), c("var", "Frequency"))
  expect_equal(out[, 2], c(11, 7, 14))
  expect_equal(out[, 1], factor(c(4, 6, 8)))
})


test_that("Prop table", {
  out <- prop_table(mtcars$cyl)
  expect_equal(names(out), c("4", "6", "8"))
  expect_equal(as.numeric(out[1, ]), c(34.4, 21.9, 43.8))
})


test_that("Prop table by and all", {
  out <- prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear")
  expect_equal(names(out), c("4", "6", "8"))
  expect_equal(dim(out), c(4, 3))
  expect_equal(as.numeric(out[1, ]), c(6.7, 13.3, 80))
})
