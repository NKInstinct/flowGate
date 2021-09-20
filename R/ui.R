sliderInput_MaxVal <- function(id, tag){
    shiny::sliderInput(id, tag, min = -1000, max = 300000, value = 250000)
}

sliderInput_Width <- function(id, tag){
    shiny::sliderInput(id, tag, min = -1000, max = -1, value = -100)
}

sliderInput_Neg <- function(id, tag){
    shiny::sliderInput(id, tag, min = 0, max = 1, value = 0)
}

sliderInput_Pos <- function(id, tag){
    shiny::sliderInput(id, tag, min = 2, max = 7, value = 4, step = 0.1)
}

ui <- shiny::fluidPage(
    shiny::titlePanel("Draw your gate"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
        shiny::actionButton("reset", "Reset"),
        shiny::actionButton("done", "Done"),
        shiny::sliderInput("bins", "Bins", min = 2, max = 2048, value = 256),
        shiny::radioButtons("gateType", "Gate Type:", c(
            "Rectangle" = "rectangleGate", "Polygon" = "polygonGate",
            "Span" = "spanGate", "Quadrant" = "quadGate"),
            selected = "rectangleGate"),
        shiny::checkboxInput("useBiex", "Use FlowJo Biex?"),
        shiny::tabsetPanel(id = "biexTab", type = "hidden",
                            shiny::tabPanel("blankPanel", " "),
                            shiny::tabPanel(
                            "biexPanel",
                            sliderInput_MaxVal("xMaxVal", "X Max Value"),
                            sliderInput_Width("xWidth", "X Width Basis"),
                            sliderInput_Neg("xNeg", "X Extra Negative Decades"),
                            sliderInput_Pos("xPos", "X Positive Decades"),
                            sliderInput_MaxVal("yMaxVal", "Y Max Value"),
                            sliderInput_Width("yWidth", "Y Width Basis"),
                            sliderInput_Neg("yNeg", "Y Extra Negative Decades"),
                            sliderInput_Pos("yPos", "Y Positive Decades")))),
    # Main panel for displaying outputs ----------------------------------
    shiny::mainPanel(
        shiny::textOutput("filterId"),
        shiny::textOutput("subset"),
        shiny::plotOutput("plot1",
                        height="auto",
                        click = "plot1_click",
                        brush = shiny::brushOpts(id = "plot1_brush")))))