test_that("gs_gate_interactive adds gates to the GatingSet", {

# Base Plot

sample <- 1
dims <- list("FSC-H", "SSC-H")
subset <- "root"
overlayGates <- NULL

input <- list(
  bins = 100,
  useCoords = FALSE,
  XMin=-1000,
  XMax=300000,
  Ymin=-1000,
  YMax=300000,
  gateType="polygonGate",
  useBiex = TRUE,
  xMaxVal = 250000,
  xWidth = -10,
  xPos = 4,
  xNeg = 0,
  yMaxVal = 250000,
  yWidth = -10,
  yPos = 4,
  yNeg = 0
)

# ----- Simulate reactiveValues -----
vals <- list(
  gateCoords = data.frame(x = numeric(), y = numeric())
)

# ----- Simulate Biex handling -----
biexTabSelected <- if (input$useBiex) "biexPanel" else "blankPanel"

# ----- Simulate transX and transY reactives -----
transX <- flowjo_biexp(
  maxValue = input$xMaxVal, 
  pos = input$xPos, 
  neg = input$xNeg, 
  widthBasis = input$xWidth, 
  inverse = TRUE
)

transY <- flowjo_biexp(
  maxValue = input$yMaxVal, 
  pos = input$yPos, 
  neg = input$yNeg, 
  widthBasis = input$yWidth, 
  inverse = TRUE
)
  
FPlot <- preparePlot(gs, sample, dims, subset, input$bins, input$useCoords, 
            c(input$XMin, input$XMax, input$YMin, input$YMax), overlayGates, 
            input$gateType, vals$gateCoords, input$useBiex, input$xMaxVal, 
            input$xWidth, input$xPos, input$xNeg, input$yMaxVal, input$yWidth,
            input$yPos, input$yNeg)

expect_true(inherits(FPlot, "ggplot"))
vdiffr::expect_doppelganger("Shiny_Biexp", FPlot)

# span Plot
  
filterId <- "Test2"
GateType <- "spanGate"
coords <- list(X=c(250, 500))
applyGateClose(gs, subset, coords, GateType, filterId, FPlot, 
                input$useBiex, input$bins, input$xMaxVal, input$xWidth, 
               input$xPos, input$xNeg, input$yMaxVal, input$yWidth, input$yPos, 
              input$yNeg)
  
spanPlot <- ggcyto(gs[[1]], subset="root", aes(x = "FSC-H", y = "SSC-H"))  + 
  geom_hex(bins=100) + geom_gate("Test2")
  
expect_true("Test2" %in% gs_get_pop_paths(gs, path=1))
  
vdiffr::expect_doppelganger("spanPlot", spanPlot)
 
# polygon Plot  
  
filterId <- "Test3"
GateType <- "polygonGate"
coords <- data.frame(X=c(50, 100, 150, 200), Y=c(100, 150, 150, 100))
applyGateClose(gs, subset, coords, GateType, filterId, FPlot, 
                input$useBiex, input$bins, input$xMaxVal, input$xWidth, 
               input$xPos, input$xNeg, input$yMaxVal, input$yWidth, input$yPos, 
              input$yNeg)
  
polygonPlot <- ggcyto(gs[[1]], subset="root", aes(x = "FSC-H", y = "SSC-H"))  + 
  geom_hex(bins=100) + geom_gate("Test3")
  
expect_true("Test3" %in% gs_get_pop_paths(gs, path=1))
  
vdiffr::expect_doppelganger("polygonPlot", polygonPlot)
  
# rectanglePlot  
  
filterId <- "Test4"
GateType <- "rectangleGate"
coords <- list(X=c(250, 500), Y=c(250, 500))
applyGateClose(gs, subset, coords, GateType, filterId, FPlot, 
                input$useBiex, input$bins, input$xMaxVal, input$xWidth, 
               input$xPos, input$xNeg, input$yMaxVal, input$yWidth, input$yPos, 
              input$yNeg)

rectanglePlot <- ggcyto(gs[[1]], subset="root", aes(x = "FSC-H", y = "SSC-H"))  + 
  geom_hex(bins=100) + geom_gate("Test4")
  
expect_true("Test4" %in% gs_get_pop_paths(gs, path=1))
  
vdiffr::expect_doppelganger("rectanglePlot", rectanglePlot)
  
# Quadrant Plot 
  
dims <- list("FL1-H", "FL2-H")
  
FPlot <- preparePlot(gs, sample, dims, subset, input$bins, input$useCoords, 
            c(input$XMin, input$XMax, input$YMin, input$YMax), overlayGates, 
            input$gateType, vals$gateCoords, input$useBiex, input$xMaxVal, 
            input$xWidth, input$xPos, input$xNeg, input$yMaxVal, input$yWidth,
            input$yPos, input$yNeg) 
  
filterId <- "Test1"
GateType <- "quadGate"
coords <- list(X=c(5000), Y=c(5000))
applyGateClose(gs, subset, coords, GateType, filterId, FPlot, 
                input$useBiex, input$bins, input$xMaxVal, input$xWidth, 
               input$xPos, input$xNeg, input$yMaxVal, input$yWidth, input$yPos, 
              input$yNeg)
  
quadPlot <- ggcyto(gs[[1]], subset="root", aes(x = "FL1-H", y = "FL2-H"))  + 
  geom_hex(bins=100) + geom_gate("CD15 FITC-CD45 PE+")
  
expect_true("CD15 FITC-CD45 PE+" %in% gs_get_pop_paths(gs, path=1))
  
vdiffr::expect_doppelganger("quadPlot", rectanglePlot)
  
})