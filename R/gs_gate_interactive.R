# Better interactive select cells
# Shamelessly stolen from the Trappnel lab's Monocle 3 "Select cells" and
# adapted
# for flow cytometry

#' Interactive Manual Gating
#'
#' \code{gs_gate_interactive} opens a new graphical window where you can draw
#' rectangle, polygon, 1-D span, or 2-D quadrant gates that will be applied to
#' an entire GateSet (see the flowWorkspace package for complete information
#' about GateSets).
#'
#' @param gs The GateSet that will be gated on.
#' @param filterId String that gives the name of the new gate. Must be unique
#'   (can specify parent gates to aid in this).
#' @param sample Numeric specifying which of the GatingHierarchy objects (i.e.
#'   which FCS file/flow sample) that make up the GateSet do you want to use to
#'   draw the gate? Note that the gate you draw will be applied to all
#'   GatingHierarchy objects in the GateSet. Defaults to the first
#'   GatingHierarchy object in the GateSet.
#' @param dims A list of strings, length-1 or length-2, that specifies the x-
#'   and y- parameters that you will be gating on. Giving a length-1 list will
#'   result in a histogram, while a length-2 list will result in a dot-plot.
#'   Giving a length-3 or longer list will result in only the first two
#'   dimensions being used, and will generate a warning to say as much. Defaults
#'   to forward scatter ("FSC-A") and side scatter ("SSC-A").
#' @param subset String that gives the name of the parent gate that you want to
#'   sample from. For example, if you wanted to gate all live cells out of a
#'   previously drawn "lymphocytes" gate, you would specify "lymphocytes" here.
#'   Defaults to "root" (ungated).
#' @param bins Numeric specifying the level of granularity needed for the gate.
#'   Higher numbers result in smaller dots. Defaults to 256, and is ignored for
#'   histograms.
#' @param coords A length-2 list of minimum and maximum coordinates that passes
#'   to coord_cartesian, in the format \code{list(x = c(0, 10), y = c(0, 10))}.
#'   Defaults to \code{NULL} (data-driven coordinate system).
#' @param regate A boolean specifying whether all gates with a name matching
#'   \code{filterId} should first be deleted before being re-drawn. Attempting
#'   to draw a gate with a non-unique \code{filterId} without specifying
#'   \code{regate = TRUE} will result in an error. Defaults to \code{FALSE}
#' @param overlayGates List of strings giving the \code{filterId}s of other
#'   gates to draw on the example plot when gating. Useful for drawing multiple
#'   gates on the same population (for example, after specifying a marker-low
#'   population, you can overlay the marker-low gate to aid in drawing a
#'   marker-high gate). Defaults to \code{NULL} (no overlaid gates).
#'
#' @examples
#'
#' path_to_fcs <- system.file("extdata", package = "flowGate")
#' fs <- read.flowSet(path = path_to_fcs,
#'                    pattern = ".FCS$",
#'                    full.names = TRUE)
#' gs <- GatingSet(fs)
#'
#' if(interactive()) { # only run in interactive sessions
#' gs_gate_interactive(gs,
#'                     filterId = "Lymphocytes")
#' }
#'
#' # returns gs with the same "Lymphocytes" gate on FSC-A and SSC-A applied to
#' # the root node (all events) of each sample in the GateSet.
#'
#' if(interactive()) {
#' gs_gate_interactive(gs,
#'                     filterId = "Live cells",
#'                     dims = "Viability",
#'                     subset = "Lymphocytes")
#' }
#'
#' # returns gs with a "Live cells" gate drawn on all cells included in the
#' # parent "Lymphocytes" gate. This gate would be based on a histogram of a
#' # marker called Viability, using the first GatingHierarchy sample as an
#' # example.
#'
#' if(interactive()){
#' gs_gate_interactive(gs,
#'                     filterId = "Live cells",
#'                     dims = list("Viability", "SSC-A"),
#'                     subset = "Lymphocytes",
#'                     regate = TRUE)
#' }
#'
#' # first deletes the "Live cells" gate drawn above, then adds a new "Live
#' # cells" gate to the set, this time based on a dot plot of Viability by
#' # side-scatter.
#'
#' if(interactive()){
#' gs_gate_interactive(gs,
#'                     filterId = "Dead cells",
#'                     dims = list("Viability", "SSC-A"),
#'                     subset = "Lymphocytes",
#'                     overlayGates = "Live cells")
#' }
#'
#' # returns gs with a "Dead cells" gate drawn on the same example graph that
#' # was used to draw the "Live cells" gate above. Overlays the "Live cells"
#' # gate on top of this graph to aid in drawing the "Dead cells" gate.
#'
#' @return The GateSet object with a new gating applied and named as specified
#'   in filterId. Also recalculates the GateSet.
#'
#' @import flowWorkspace
#' @import ggcyto
#' @import BiocManager
#' @importFrom ggplot2 aes geom_density scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme element_blank coord_cartesian geom_hex geom_path
#'
#'

# helper functions ==================================
theme_flowGate <- theme_gray() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              title = element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank())

# main function =====================================
#'
#' @export
#'
gs_gate_interactive <- function(gs,
                                filterId,
                                sample = 1,
                                dims = list("FSC-A", "SSC-A"),
                                subset = "root",
                                bins = 256,
                                coords = NULL,
                                regate = FALSE,
                                overlayGates = NULL){

    #Delete gate if regating-----------------------
    if(regate == TRUE){
        gs_pop_remove(gs, filterId)
    }

    #Select only the one sample to plot------------
    sample.gs <- gs[[sample]]


    #generate the plot using the input params------
    if(length(dims) > 2){
        warning("gs_gate_interactive can only handle one or two dims.
            The first two dims will be used, the others discarded.")
    }


    if(length(dims) == 1){
        gg <- ggcyto::ggcyto(sample.gs,
                             aes(!!dims[[1]]),
                             subset = subset) +
            geom_density() +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) +
            theme_flowGate
        if(!is.null(coords)){
            gg <- gg + coord_cartesian(xlim = coords[[1]])
        }
    } else {
        gg <- ggcyto::ggcyto(sample.gs,
                             aes(!!dims[[1]],
                                 !!dims[[2]]),
                             subset = subset) +
            geom_hex(bins = bins) +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) +
            theme_flowGate

        if(!is.null(coords)){
            gg <- gg + coord_cartesian(xlim = coords[[1]],
                                       ylim = coords[[2]])
        }
    }

    if(!is.null(overlayGates)){
        gg <- gg + geom_gate(overlayGates)
    }

    gg <- as.ggplot(gg)

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
                vals$coords <- dplyr::bind_rows(vals$coords, res)

                vals$plot <- vals$plot +
                    geom_path(data = vals$coords,
                              aes(x, y, group = 1),
                              inherit.aes = FALSE)

            } else if(input$gateType == "quadGate"){
                #Quad Gate
                vals$coords <- list("X" = input$plot1_click$x,
                                    "Y" = input$plot1_click$y)

                vals$plot <- vals$plot +
                    ggplot2::geom_hline(yintercept = vals$coords$Y) +
                    ggplot2::geom_vline(xintercept = vals$coords$X)

            }
            # })
        })


        # Reset all points
        shiny::observeEvent(input$reset, {
            vals$coords <- data.frame("x" = numeric(), "y" = numeric())
            vals$plot <- gg
        })

        #Apply gate and close
        shiny::observeEvent(input$done, {
            # reactive({
            #Kinda weird gating strat but here it goes: Of the four options,
            #only the polygon gate is stored as a dataframe, so first check if
            #the coordinates are a data.frame. If so, this should be treated as
            #a polygon (this way if someone draws a rectangular polygon, it will
            #still be treated as a polygon and not as a rectangle. Very
            #important for slanty rectangles)
            if(is.data.frame(vals$coords)){
                #Get the channel name instead of the short name
                names(vals$coords) <- c(names(gg[[1]])[[3]],
                                        names(gg[[1]])[[4]])
                vals$coords <- as.matrix(vals$coords)
                vals$coords <- flowCore::polygonGate(vals$coords,
                                                     filterId = filterId)
                #If it isn't a dataframe, check if it's a length-1 list. If so,
                #it has to be a span (only single-dimension gate)
            } else if(length(vals$coords) == 1){
                names(vals$coords) <- c(names(gg[[1]])[[3]])
                vals$coords <- flowCore::rectangleGate(vals$coords,
                                                       filterId = filterId)

                #If it isn't a span, check the length of the first element of
                #the list. If it's only length-1, that means this is a quad gate
                #(which gets defined by a single, two-dimensional point)
            }  else if(length(vals$coords[[1]]) == 1){
                names(vals$coords) <- c(names(gg[[1]])[[3]],
                                        names(gg[[1]])[[4]])
                vals$coords <- flowCore::quadGate(vals$coords,
                                                  filterId = filterId)

                #At this point, it should be a rectangle, but just in case we're
                #going to check that the first element is length-2 and if it
                #isn't, throw an error because the thing that got passed doesn't
                #fit any gating scheme we know.
            } else if(length(vals$coords[[1]]) == 2){
                names(vals$coords) <- c(names(gg[[1]])[[3]],
                                        names(gg[[1]])[[4]])
                vals$coords <- flowCore::rectangleGate(vals$coords,
                                                       filterId = filterId)
            } else {
                stop("What did you draw? I'm sure it was
             pretty but it wasn't anything I can
             recognize as a gate")
            }


            gs_pop_add(gs, vals$coords, parent = subset)
            recompute(gs)


            shiny::stopApp(vals$coords)
        })

    }

    #Actually run the app------------------------------------
    shiny::runApp(shiny::shinyApp(ui, server))
}
