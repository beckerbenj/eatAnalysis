
context("Write tables and graphs to files")

old_wd <- getwd()
# file.remove("write_xlsx_helper.xlsx")


test_that("Save xlsx with multiple sheets ", {
  df_list <- list(mtcars1 = mtcars, iris1 = iris, mtcars2 = mtcars)
  write_xlsx(df_list = df_list, filePath = "write_xlsx_helper.xlsx", row.names = FALSE)

  mtcars_test <- openxlsx::read.xlsx("write_xlsx_helper.xlsx", sheet = "mtcars1")
  iris_test <- openxlsx::read.xlsx("write_xlsx_helper.xlsx", sheet = "iris1")
  rownames(mtcars) <- rownames(iris) <- NULL
  expect_identical(mtcars, mtcars_test)
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)
  expect_identical(iris2, iris_test)
})
