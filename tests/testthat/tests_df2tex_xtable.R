
test_that("df2tex_xtable", {
  out <- capture_output(null <- df2tex_xtable(mtcars))
  expect_equal(length(out), 1)
  expect_true(grepl("\\hline", out))
  expect_true(grepl("mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb", out))
  expect_true(grepl("\\begin{table}[ht]", out, fixed = TRUE))
  expect_true(grepl("\\begin{tabular}{rrrrrrrrrrr}", out, fixed = TRUE))
  expect_true(grepl("\\centering
\\caption{}", out, fixed = TRUE))
})

