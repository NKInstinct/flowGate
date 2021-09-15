#Setup the UI----------------------------------
ui <- shiny::fluidPage(
  shiny::titlePanel("Draw your gate"),
  # Sidebar layout with input and output definitions ----
  shiny::sidebarLayout(
    # Sidebar panel for inputs ----
    shiny::sidebarPanel(
      # clear button - only actually needed for the click ones, not
      # the brush ones
      shiny::actionButton("reset", "Reset"),
      # done button
      shiny::actionButton("done", "Done"),
      shiny::sliderInput("bins", "Bins",
                         min = 2, max = 2048, value = 256),

      shiny::radioButtons("gateType", "Gate Type:",
                          c("Rectangle" = "rectangleGate",
                            "Polygon" = "polygonGate",
                            "Span" = "spanGate",
                            "Quadrant" = "quadGate"),
                          selected = "rectangleGate"),
      shiny::checkboxInput("useBiex", "Use FlowJo Biex?"),
      shiny::tabsetPanel(id = "biexTab", type = "hidden",
                         shiny::tabPanel("blankPanel", " "),
                         shiny::tabPanel("biexPanel",
      shiny::sliderInput("xMaxVal", "X Max Value",
                         min = -1000, max = 250000, value = 50000),
      shiny::sliderInput("xWidth", "X Width Basis",
                         min = -1000, max = -1, value = -1),
      shiny::sliderInput("xNeg", "X Extra Negative Decades",
                         min = 0, max = 1, value = 0),
      shiny::sliderInput("xPos", "X Positive Decades",
                         min = 2, max = 7, value = 4),
      shiny::sliderInput("yMaxVal", "Y Max Value",
                         min = -1000, max = 250000, value = 50000),
      shiny::sliderInput("yWidth", "Y Width Basis",
                         min = -1000, max = -1, value = -1),
      shiny::sliderInput("yNeg", "Y Extra Negative Decades",
                         min = 0, max = 1, value = 0),
      shiny::sliderInput("yPos", "Y Positive Decades",
                         min = 2, max = 7, value = 4)
      ))
    ),
    # Main panel for displaying outputs ----------------------------------
    shiny::mainPanel(
      shiny::textOutput("filterId"),
      shiny::textOutput("subset"),
      shiny::plotOutput("plot1",
                        height="auto",
                        # width="500px",
                        click = "plot1_click",
                        brush = shiny::brushOpts(id = "plot1_brush")),
    )
  )
)
