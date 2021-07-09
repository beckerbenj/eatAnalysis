
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

test_that("Prop table with weights", {
  out <- prop_table(mtcars$cyl, weights = rep(c(0.5, 1), 16))
  expect_equal(names(out), c("4", "6", "8"))
  expect_equal(as.numeric(out[1, ]), c(35.4, 25, 39.6))
  # Hmisc::wtd.table(mtcars$cyl, weights = rep(c(0.5, 1), 16))
  #8.5 /(8.5 + 6.0 +9.5)
})

test_that("Prop table by and all errors", {
  expect_error(prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = 1:2),
               "'by_var' needs to be a character of length 1.")
  expect_error(prop_table_by_and_all(df = mtcars, dep = 1:2, by_var = 1),
               "'dep' needs to be a character of length 1.")
  expect_error(prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear", weights = 1),
               "'weights' needs to be a character of length 1.")
  expect_error(prop_table_by_and_all(df = mtcars, dep = "lala", by_var = "gear", weights = "test"),
               "The following variables are not in 'df': lala, test")

})

test_that("Prop table by and all", {
  out <- prop_table_by_and_all(df = mtcars, dep = "cyl", by_var = "gear")
  expect_equal(names(out), c("4", "6", "8"))
  expect_equal(dim(out), c(4, 3))
  expect_equal(as.numeric(out[1, ]), c(6.7, 13.3, 80))
  expect_equal(rownames(out), c("3", "4", "5", "Total"))
})

test_that("Prop table by and all with weights", {
  mtcars2 <- mtcars
  mtcars2[, "weights"] <- rep(c(0.5, 1), 16)
  out <- prop_table_by_and_all(df = mtcars2, dep = "cyl", by_var = "gear", weights = "weights")
  expect_equal(names(out), c("4", "6", "8"))
  expect_equal(dim(out), c(4, 3))
  expect_equal(as.numeric(out[1, ]), c(4.5, 18.2, 77.3))
  expect_equal(as.numeric(out[4, ]), c(35.4, 25.0, 39.6))
  expect_equal(rownames(out), c("3", "4", "5", "Total"))
})
