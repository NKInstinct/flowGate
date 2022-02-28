uiTransform <- shiny::fluidPage(
    shiny::titlePanel("Update a Gate"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::actionButton("transReset", "Reset"),
            shiny::actionButton("transDone", "Done"),
            shiny::radioButtons("transScaleToggle", "Scale Type:",
                                c("Uniform" = "uniform", 
                                  "Independent" = "independent")),
            shiny::numericInput("transScaleX", "Scale Factor (Uniform/X)", 1),
            shiny::numericInput("transScaleY", "Scale Factor (Y)", 1),
            shiny::sliderInput("transRotate", "Rotation (deg)", value = 0,
                               min = -360, max = 360),
            shiny::numericInput("transShiftX", "Shift Gate (X)", 0),
            shiny::numericInput("transShiftY", "Shift Gate (Y)", 0),
            shiny::sliderInput("transBins", "Bins", min = 2, max = 2048, value = 256),
            shiny::checkboxInput("transUseBiex", "Use FlowJo Biex?"),
            shiny::checkboxInput("transUseCoords", "Enable Manual Coords?"),
            shiny::numericInput("transXMin", "X Minimum", -1000),
            shiny::numericInput("transXMax", "X Maximum", 20000),
            shiny::numericInput("transYMin", "Y Minimum", -1000),
            shiny::numericInput("transYMax", "Y Maximum", 20000)),
        # Main panel for displaying outputs ----------------------------------
        shiny::mainPanel(
            shiny::plotOutput("transPlot",
                              height="auto"))))