
# old_wd <- getwd()
# file.remove("write_xlsx_helper.xlsx")

test_that("Save and read xlsx with one data.frame ", {
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(df_list = mtcars, filePath = f, row.names = FALSE)

  mtcars_test <- read_xlsx(f)
  mtcars_test2 <- mtcars
  rownames(mtcars_test2) <- NULL
  expect_identical(mtcars_test2, mtcars_test)
})

test_that("Save and read xlsx with multiple sheets ", {
  f <- tempfile(fileext = ".xlsx")
  df_list <- list(mtcars1 = mtcars, iris1 = iris, mtcars2 = mtcars)
  write_xlsx(df_list = df_list, filePath = f, row.names = FALSE)

  mtcars_test <- openxlsx::read.xlsx(f, sheet = "mtcars1")
  iris_test <- openxlsx::read.xlsx(f, sheet = "iris1")
  test <- read_xlsx(f)

  rownames(mtcars) <- rownames(iris) <- NULL
  expect_identical(mtcars, test$mtcars1)
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)
  expect_identical(iris2, test$iris1)
})
