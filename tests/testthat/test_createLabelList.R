test_that("createLabelList", {
  file <- system.file("extdata", "Klauer.sav", package = "eatAnalysis")
  dat  <- foreign::read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
  dat  <- convertLabel(dat)
  atts <- createLabelList(dat)
  expect_true(class(atts) == "data.frame")
  expect_true(all(dim(atts) == c(21,5)))
  expect_true(all(colnames(atts) == c("varName", "class", "varLabel", "value", "valLabel")))
# checken dass doppelte aufteten von varName nur dort vorkommt, wo es value labels gibt
  na   <- atts[which(is.na(atts[,"value"])),]
  expect_equal(length(unique(na[,"varName"])), nrow(na))
})

