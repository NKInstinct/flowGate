context("Gate Handlers")

test_that("Brush gates return correct outputs", {

  brush <- data.frame(xmin = 0, xmax = 4, ymin = 0, ymax = 1)

  expect_equal(coordBrush(brush, "rectangleGate", FALSE, NULL, NULL), list("X" = c(0, 4),
                                                        "Y" = c(0, 1)))
  expect_equal(length(coordBrush(brush, "rectangleGate", FALSE, NULL, NULL)), 2)
  expect_equal(coordBrush(brush, "spanGate", FALSE, NULL, NULL), list("X" = c(0, 4)))
  expect_equal(length(coordBrush(brush, "spanGate", FALSE, NULL, NULL)), 1)
})
#> Test passed 🎉

test_that("Click gates return correct outputs", {
  click <- data.frame(x = c(0, 5, 5, 0),
                      y = c(0, 0, 5, 5))

  expect_equal(coordClick(click[1,], "polygonGate", FALSE, NULL, NULL), data.frame("x" = 0,
                                                                "y" = 0))
  expect_equal(coordClick(click, "polygonGate", FALSE, NULL, NULL), data.frame("x" = c(0, 5, 5, 0),
                                                            "y" = c(0, 0, 5, 5)))
  expect_equal(coordClick(click[1,], "quadGate", FALSE, NULL, NULL), list("X" = 0,
                                                       "Y" = 0))
})
#> Test passed 🌈
