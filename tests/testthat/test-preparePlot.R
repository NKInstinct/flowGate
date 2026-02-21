test_that("Data is retrieved correctly", {
  expect_length(sample.gs, 1)
  expect_s4_class(sample.gs, "GatingHierarchy")
})

#> Test passed 🎉

test_that("1D plots are working", {
  dims <- list("FSC-H")
  subset <- "root"
  gg1D <- ggcyto::ggcyto(sample.gs, aes(!!dims[[1]]), subset = subset) +
            geom_density() + scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) + theme_flowGate
  
  expect_true(inherits(gg1D, "ggplot"))
  vdiffr::expect_doppelganger("1D_FSC-H", gg1D)

  coords <- list(250, 750)
  gg1D_coord <- gg1D + coord_cartesian(xlim = c(coords[[1]], coords[[2]]))
  vdiffr::expect_doppelganger("1D_FSC-H_coords", gg1D_coord)

  if ("ggcyto_GatingSet" %in% class(gg1D)){
    class(gg1D) <- class(gg1D)[class(gg1D) != "ggcyto_GatingSet"]
  }
  
  gg1D_ggplot <- as.ggplot(gg1D)
  expect_false(inherits(gg1D_ggplot, "ggcyto"))
  vdiffr::expect_doppelganger("1D_FSC-H_coords_gg", gg1D_ggplot)
})

#> Test passed 🎉

test_that("2D plots are working", {
  dims <- list("FSC-H", "SSC-H")
  bins <- 120
  subset <- "root"

  gg2d <- ggcyto::ggcyto(
    sample.gs, aes(!!dims[[1]], !!dims[[2]]), subset = subset) +
    geom_hex(bins = bins) + scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + theme_flowGate

  expect_true(inherits(gg2d, "ggplot"))
  vdiffr::expect_doppelganger("2D_FSC-H", gg2d)

  coords <- list(250, 750, 0, 500)

  gg2d_coord <- gg2d + coord_cartesian(xlim = c(coords[[1]], coords[[2]]),
    ylim = c(coords[[3]], coords[[4]]))

  vdiffr::expect_doppelganger("2D_FSC-H_coords", gg2d_coord)

    if ("ggcyto_GatingSet" %in% class(gg2d)){
    class(gg2d) <- class(gg2d)[class(gg2d) != "ggcyto_GatingSet"]
  }
  
  gg2d_ggplot <- as.ggplot(gg2d)
  expect_false(inherits(gg2d_ggplot, "ggcyto"))
  vdiffr::expect_doppelganger("2D_FSC-H_coords_gg", gg2d_ggplot)
})

#> Test passed 🎉

#test_that("as.ggplot is working", {
#  gg <- ggcyto::as.ggplot(gg)
#
#  vdiffr::expect_doppelganger("2D_FSC-H_coords", gg2)
#})


