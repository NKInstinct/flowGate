#Setup the UI----------------------------------
ui <- shiny::fluidPage(
  shiny::titlePanel("Draw your gate"),
  # Sidebar layout with input and output definitions ----
  shiny::sidebarLayout(
    # Sidebar panel for inputs ----
    shiny::sidebarPanel(
      # clear button - only actually needed for the click ones, not
      # the brush ones
      shiny::actionButton("reset", "Clear"),
      # done button
      shiny::actionButton("done", "Done"),

      shiny::radioButtons("gateType", "Gate Type:",
                          c("Rectangle" = "rectangleGate",
                            "Polygon" = "polygonGate",
                            "Span" = "spanGate",
                            "Quadrant" = "quadGate"),
                          selected = "rectangleGate"),
    ),
    # Main panel for displaying outputs ----------------------------------
    shiny::mainPanel(
      shiny::textOutput("filterId"),
      shiny::textOutput("subset"),
      shiny::plotOutput("plot1",
                        height="auto",
                        # width="500px",
                        click = "plot1_click",
                        brush = shiny::brushOpts(id = "plot1_brush"))
    )
  )
)
