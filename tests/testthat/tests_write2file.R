
context("Write tables and graphs to files")

old_wd <- getwd()
# file.remove("write_xlsx_helper.xlsx")


test_that("Save xlsx with multiple sheets ", {
  df_list <- list(mtcars1 = mtcars, iris1 = iris, mtcars2 = mtcars)
  write_xlsx(df_list = df_list, filePath = "write_xlsx_helper.xlsx", row.names = FALSE)

  mtcars_test <- xlsx::read.xlsx("write_xlsx_helper.xlsx", sheetName = "mtcars1", colClasses = "numeric")
  iris_test <- xlsx::read.xlsx("write_xlsx_helper.xlsx", sheetName = "iris1", colClasses = "numeric")
  rownames(mtcars) <- rownames(iris) <- NULL
  expect_identical(mtcars, mtcars_test)
  expect_identical(iris, iris_test)
})
