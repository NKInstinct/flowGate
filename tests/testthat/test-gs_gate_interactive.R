test_that("gs_gate_interactive is functional", {

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
  
#gs_gate_interactive(gs,filterId = "Lymphocytes6", dims = list("FSC-H", "SSC-H"), regate=FALSE)

#filterId <- "Test2"
#GateType <- "spanGate"
#coords <- list(X=c(250, 500))
  
  
#applyGateClose(gs, subset, coords, GateType, filterId, FPlot, 
#                input$useBiex, input$bins, input$xMaxVal, input$xWidth, 
#                input$xPos, input$xNeg, input$yMaxVal, input$yWidth, input$yPos, 
#               input$yNeg)

})