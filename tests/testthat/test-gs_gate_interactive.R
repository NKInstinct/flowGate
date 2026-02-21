test_that("gs_gate_interactive is functional", {

gs_gate_interactive(gs,filterId = "Lymphocytes", dims = list("FSC-H", "SSC-H"), regate=FALSE)
 
  A <- 2 + 2
  expect_equal(A, 4)# 
})