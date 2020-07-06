# Better interactive select cells
# Shamelessly stolen from the Trappnel lab's Monocle 3 "Select cells" and adapted
# for flow cytometry

#' Choose cells interactively to gate a gate set
#'
#' @param gs gate-set object to gate on
#' @param filterId Name of the new gate
#' @param sample which gating hierarchy in the gate-set do you want to preview?
#' @param dims a list of length 2 giving the abscissa and ordonate
#' @param subset name of parent gate for drawing
#' @param bins The level of granularity needed for the gate
#' @param coords passes to coord_cartesian.
#' @param regate Boolean - should the gate first be deleted from the gs to allow it to be redrawn?
#' @param overlayGates Should I draw other gates on this plot, to help with adding multiples?
#'
#' @return the gs with a new gate drawn and recalculated on all gh within the set.
#' @export
#'
GateInteractive <- function(gs,
                            filterId,
                            sample = 1,
                            dims = list("FSC-A", "SSC-A"),
                            subset = "root",
                            bins = 256,
                            coords = NULL, #might change this back, we'll see
                            regate = FALSE,
                            overlayGates = NULL){

  #Delete gate if regating-----------------------
  if(regate == TRUE){
    gs_pop_remove(gs, filterId)
  }

  #Select only the one sample to plot------------
  sample.gs <- gs[[sample]]


  #generate the plot using the input params------
  #Add an assert.that tag if they try to pass 3+ dims - can't handle 3d graphs yet :P
  if(length(dims) > 2){
    warning("GateInteractive can only handle one or two dims.
            The first two dims will be used, the others discarded.")
  }

  if(length(dims) == 1){
    gg <- ggcyto(sample.gs, aes(!!dims[[1]]), subset = subset) +
      geom_density() +
      #theme_pubr() + would like to do this without requiring ggpubr as well, just update theme() below
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            title = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank())
    if(!is.null(coords)){
      gg <- gg + coord_cartesian(xlim = coords[[1]])
    }
  } else {
    gg <- ggcyto(sample.gs, aes(!!dims[[1]], !!dims[[2]]), subset = subset) +
      geom_hex(bins = bins) +
      #theme_pubr() +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            title = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank())

    if(!is.null(coords)){
      gg <- gg + coord_cartesian(xlim = coords[[1]], ylim = coords[[2]])
    }
  }

  if(!is.null(overlayGates)){
    gg <- gg + geom_gate(overlayGates)
  }

  gg %<>% as.ggplot()

  #Setup the UI----------------------------------
  ui <- shiny::fluidPage(
    shiny::titlePanel("Draw your gate"),

    # Sidebar layout with input and output definitions ----
    shiny::sidebarLayout(

      # Sidebar panel for inputs ----
      shiny::sidebarPanel(
        # clear button - only actually needed for the click ones, not the brush ones
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

      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::plotOutput("plot1",
                          height="auto",
                          # width="500px",
                          click = "plot1_click",
                          brush = shiny::brushOpts(id = "plot1_brush"))
      )
    )
  )

  #Make server functions----------------------

  server <- function(input, output, session) {

    vals <- shiny::reactiveValues(
      plot = gg,
      coords = data.frame("x" = numeric(), "y" = numeric())
    )

    output$plot1 <- shiny::renderPlot({
      vals$plot
    },
    height = function() {
      session$clientData$output_plot1_width
    }
    )

    #Brush Gates
    shiny::observeEvent(input$plot1_brush, {
      # req(input$gateType)
      # reactive({
      if(input$gateType == "rectangleGate"){
        #Rectangle Gate
        vals$coords <- list("X" = c(input$plot1_brush$xmin,
                                    input$plot1_brush$xmax),
                            "Y" = c(input$plot1_brush$ymin,
                                    input$plot1_brush$ymax))
      } else if(input$gateType == "spanGate"){
        #Span Gate
        vals$coords <- list("X" = c(input$plot1_brush$xmin,
                                    input$plot1_brush$xmax))
      }
      # })

    })

    #Click Gates
    shiny::observeEvent(input$plot1_click, {
      # req(input$gateType)
      # reactive({
      if(input$gateType == "polygonGate"){
        #Polygon Gate
        res <- data.frame("x" = input$plot1_click$x,
                          "y" = input$plot1_click$y)
        vals$coords <- bind_rows(vals$coords, res)
      } else if(input$gateType == "quadGate"){
        #Quad Gate
        vals$coords <- list("X" = input$plot1_click$x,
                            "Y" = input$plot1_click$y)
      }
      # })
    })

    # Reset all points
    shiny::observeEvent(input$reset, {
      vals$coords <- data.frame("x" = numeric(), "y" = numeric())
    })

    #Apply gate and close
    shiny::observeEvent(input$done, {
      # reactive({
      #Kinda weird gating strat but here it goes:
      #Of the four options, only the polygon gate is stored as a dataframe, so
      #first check if the coordinates are a data.frame. If so, this should be
      #treated as a polygon (this way if someone draws a rectangular polygon, it
      #will still be treated as a polygon and not as a rectangle. Very important
      #for slanty rectangles)
      if(is.data.frame(vals$coords)){
        #Get the channel name instead of the short name
        names(vals$coords) <- c(names(gg[[1]])[[3]], names(gg[[1]])[[4]])
        vals$coords %<>%
          as.matrix %>%
          flowCore::polygonGate(filterId = filterId)

        #If it isn't a dataframe, check if it's a length-1 list. If so, it has to
        #be a span (only single-dimension gate)
      } else if(length(vals$coords) == 1){
        names(vals$coords) <- c(names(gg[[1]])[[3]])
        vals$coords %<>%
          flowCore::rectangleGate(filterId = filterId)

        #If it isn't a span, check the length of the first element of the list. If
        #it's only length-1, that means this is a quad gate (which gets defined by
        #a single, two-dimensional point)
      }  else if(length(vals$coords[[1]]) == 1){
        names(vals$coords) <- c(names(gg[[1]])[[3]], names(gg[[1]])[[4]])
        vals$coords %<>%
          flowCore::quadGate(filterId = filterId)

        #At this point, it should be a rectangle, but just in case we're going to
        #check that the first element is length-2 and if it isn't, throw an error
        #because the thing that got passed doesn't fit any gating scheme we know.
      } else if(length(vals$coords[[1]]) == 2){
        names(vals$coords) <- c(names(gg[[1]])[[3]], names(gg[[1]])[[4]])
        vals$coords %<>%
          flowCore::rectangleGate(filterId = filterId)
      } else {
        stop("What did you draw? I'm sure it was pretty but it wasn't anything I can recognize as a gate")
      }


      gs_pop_add(gs, vals$coords, parent = subset)
      recompute(gs)


      shiny::stopApp(vals$coords)
    })

  }

  #Actually run the app------------------------------------
  sel <- shiny::runApp(shiny::shinyApp(ui, server))
  # plot(sel[2])

  return(sel)
}
