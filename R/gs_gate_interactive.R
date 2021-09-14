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
#'                     filterId = "Lymphocytes",
#'                     dims = list("FSC-H", "SSC-H"))
#' }
#'
#' # returns gs with the same "Lymphocytes" gate on FSC-H and SSC-H applied to
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
#' @importFrom ggplot2 aes_ aes geom_density scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous geom_path geom_hex
#' @importFrom ggplot2 theme element_blank coord_cartesian
#' @importFrom rlang .data
#'
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
    # Delete gate if regating
    if(regate == TRUE){gs_pop_remove(gs, filterId)}

    server <- function(input, output, session) {
        vals <- shiny::reactiveValues(
            plot = preparePlot(gs, sample, dims, subset, shiny::isolate(input$bins),
                               coords, regate, overlayGates),
            coords = data.frame("x" = numeric(), "y" = numeric()),
            gateInfo = list(filterId, subset)
        )
        
        FPlot <- shiny::reactive(if(input$useBiex == TRUE){
            suppressMessages(
            if(length(dims) == 1){
               vals$plot +
                                             ggcyto::scale_x_flowjo_biexp(
                                                 maxValue = input$xMaxVal,
                                                 widthBasis = input$xWidth,
                                                 pos = input$xPos,
                                                 neg = input$xNeg)
            } else if(length(dims) > 1){
                vals$plot +
                                             ggcyto::scale_x_flowjo_biexp(
                                                 maxValue = input$xMaxVal,
                                                 widthBasis = input$xWidth,
                                                 pos = input$xPos,
                                                 neg = input$xNeg) +
                                             ggcyto::scale_y_flowjo_biexp(
                                                 maxValue = input$yMaxVal,
                                                 widthBasis = input$yWidth,
                                                 pos = input$yPos,
                                                 neg = input$yNeg)
            })
        } else{
            vals$plot
        })
        
        output$plot1 <- shiny::renderPlot({
            FPlot()
        },
        height = function() {
            session$clientData$output_plot1_width
        }
        )
        output$filterId <- shiny::renderText({paste("Gate Name: ",
                                                   vals$gateInfo[[1]],
                                                   sep = "")})
        output$subset <- shiny::renderText({paste("subset of: ",
                                                  vals$gateInfo[[2]],
                                                  sep = "")})
        #Brush Gates ---------------------------------------------------
        shiny::observeEvent(input$plot1_brush, {
            if(input$gateType %in% c("rectangleGate", "spanGate")){
                vals$coords <- coordBrush(input$plot1_brush,
                                          input$gateType)
           }
        })
        #Click Gates ---------------------------------------------------
        shiny::observeEvent(input$plot1_click, {
            if(input$gateType == "polygonGate"){
                res <- coordClick(input$plot1_click, input$gateType)
                vals$coords <- dplyr::bind_rows(vals$coords, res)
                if(nrow(vals$coords) > 1){
                    vals$plot <- vals$plot +
                        geom_path(data = vals$coords,
                                  aes(.data$x, .data$y),
                                  inherit.aes = FALSE)
                }
            }else if(input$gateType == "quadGate"){
                vals$coords <- coordClick(input$plot1_click, input$gateType)
                vals$plot <- vals$plot +
                    ggplot2::geom_hline(yintercept = vals$coords$Y) +
                    ggplot2::geom_vline(xintercept = vals$coords$X)
            }
        })
        # Reset all points ----------------------------------------------
        shiny::observeEvent(input$reset, {
            vals$coords <- data.frame("x" = numeric(), "y" = numeric())
            vals$plot <- preparePlot(gs, sample, dims, subset, shiny::isolate(input$bins),
                                    coords, regate, overlayGates)
        })
        
        # Apply gate and close ------------------------------------------
        shiny::observeEvent(input$done, {
            gate <- applyGateClose(vals$coords,
                                   input$gateType,
                                   filterId,
                                   shiny::isolate(FPlot()))
            gs_pop_add(gs, gate, parent = subset)
            recompute(gs)
            output <- list("Gate" = gate,
                           "Bins" = input$bins,
                           "Scaling" = ifelse(input$useBiex,
                                              data.frame(
                                                  "Parameters" = c(
                                                      "Max Value",
                                                      "Width Basis",
                                                      "Positive Decades",
                                                      "Extra Negative Decades"),
                                                  "X" = c(input$xMaxVal,
                                                          input$xWidth,
                                                          input$xPos,
                                                          input$xNeg),
                                                  "Y" = c(input$yMaxVal,
                                                          input$yWidth,
                                                          input$yPos,
                                                          input$yNeg)),
                                              "Unused"))
            shiny::stopApp(output)
        })
    }
    shiny::runApp(shiny::shinyApp(ui, server))
}
