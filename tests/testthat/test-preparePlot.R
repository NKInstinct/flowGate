test_that("Data is retrieved correctly", {
  sample <- 1
  sample.gs <- gs[[sample]]
  expect_length(sample.gs, 1)
  expect_s4_class(sample.gs, "GatingHierarchy")
})

#> Test passed 🎉

test_that("1D plots are working", {
  dims <- list("FSC-H")
  subset <- "root"
  gg <- ggcyto::ggcyto(sample.gs, aes(!!dims[[1]]), subset = subset) +
            geom_density() + scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) + theme_flowGate
  
  expect_true(inherits(gg, "ggplot"))
  vdiffr::expect_doppelganger("1D_FSC-H", gg)

  coords <- list(250, 750)
  gg2 <- gg + coord_cartesian(xlim = c(coords[[1]], coords[[2]]))
  vdiffr::expect_doppelganger("1D_FSC-H_coords", gg2)
})

#> Test passed 🎉

test_that("2D plots are working", {
  dims <- list("FSC-H")
  subset <- "root"
  gg <- ggcyto::ggcyto(sample.gs, aes(!!dims[[1]]), subset = subset) +
            geom_density() + scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) + theme_flowGate
  
  expect_true(inherits(gg, "ggplot"))
  vdiffr::expect_doppelganger("1D_FSC-H", gg)

  coords <- list(250, 750)
  gg2 <- gg + coord_cartesian(xlim = c(coords[[1]], coords[[2]]))
  vdiffr::expect_doppelganger("1D_FSC-H_coords", gg2)
})

#> Test passed 🎉
