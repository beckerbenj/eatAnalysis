
# old_wd <- getwd()
# file.remove("write_xlsx_helper.xlsx")

test_that("update xlsx", {
  newDat <- data.frame(varName = c("var1", "var2", "var4"), decision = NA,
                       stringsAsFactors = FALSE)

  out <- update_xlsx(newDat = newDat, filePath = "helper_update_xlsx.xlsx", sheetName = "decision", idCol = "varName")

  expect_equal(out$varName, newDat$varName)
  expect_equal(out$decision, c("x", "x", NA))
})
