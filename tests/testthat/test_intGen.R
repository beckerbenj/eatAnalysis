
test_that("intGen", {
  vars   <- paste0("Var_", letters[1:5])
  frml   <- intGen(vars = vars, upto = 2)
  target <- "Var_a : Var_b + Var_a : Var_c + Var_a : Var_d + Var_a : Var_e + Var_b : Var_c + Var_b : Var_d + Var_b : Var_e + Var_c : Var_d + Var_c : Var_e + Var_d : Var_e"
  testthat::expect_equal(frml, target)
})

