# Tests for the unexported packages from ggcyto that I need to use - should show
# an early red flag if these get changed at all

test_that("ggcyto:::fortify.rectangleGate is behaving expectedly", {
    rect <- flowCore::rectangleGate(filterId = "TestRect", 
                                    list("FSC-H" = c(200, 600), 
                                         "SSC-H" = c(0, 400)))
    fortRect <- ggcyto:::fortify.rectangleGate(rect)
    
    expect_s3_class(fortRect, "data.frame")
    expect_length(fortRect, 2)
    expect_length(fortRect$`FSC-H`, 5)
    expect_equal(fortRect[[3,2]], 400)
    expect_equal(colnames(fortRect), c("FSC-H", "SSC-H"))
})

test_that("ggcyto:::fortify.polygonGate is behaving expectedly", {
    bound <- matrix(c(300, 300, 600, 600, 50, 300, 300, 50), ncol = 2, nrow = 4)
    colnames(bound) <- c("FSC-H", "SSC-H")
    poly <- flowCore::polygonGate(filterId = "nonDebris", .gate = bound)
    fortPoly <- ggcyto:::fortify.polygonGate(poly)
    
    expect_s3_class(fortPoly, "data.frame")
    expect_length(fortPoly, 2)
    expect_length(fortPoly$`FSC-H`, 5)
    expect_equal(fortPoly[[3,2]], 300)
    expect_equal(colnames(fortPoly), c("FSC-H", "SSC-H"))
})