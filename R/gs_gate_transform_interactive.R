#' Interactively adjust a gate from a GatingSet
#' 
#' CAUTION: Experimental Function. Still probably has bugs.
#'
#' Call gs_gate_transform_interactive to open a small Shiny app to allow for
#' manual, interactive adjustments to gates. Currently only supports
#' rectangleGates and polygonGates.
#'
#' @param gs The GatingSet containing the gate you want to adjust
#' @param node String specifying the (unambiguous) name of the node to adjust
#' @param sample Numeric specifying which sample in the GatingSet to use for
#'   example purposes. Note that the adjusted gate will be applied to ALL
#'   samples, not just this one.
#' @param dims List of characters specifying channel names or marker names to
#'   plot on x and y axis. Defaults to list("FSC-A", "SSC-A") mostly just to
#'   make the format clear.
#' @param overlayGates (optional) string or character vector specifying names of
#'   gates to draw on the plot but NOT adjust, for ease of adjusting a gate in
#'   the vicinity of other gates. Leave NULL to not overlay any gates.
#'
#' @return NULL, but silently deletes the old gate, adds the new one, and
#'   recomputes the GatingSet.
#'   
#' @examples 
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
#'                     
#' # Adds a lymphocytes gate to the GatingSet (exactly as in gs_gate_interactive)
#'                     
#' gs_gate_transform_interactive(gs,
#'                               filterId = "Lymphocytes", 
#'                               dims = list("FSC-H", "SSC-H"))
#' }
#' 
#' # Opens a window to adjust the gate manually
#'
#' @import flowWorkspace
#' @import ggcyto
#' @import BiocManager
#' @importFrom ggplot2 aes_ aes geom_density scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous geom_path geom_hex
#' @importFrom ggplot2 theme element_blank coord_cartesian
#' @importFrom rlang .data
#' @importFrom shiny reactive
#' 
#' @export
gs_gate_transform_interactive <- function(
    gs, node, sample = 1, dims = list("FSC-A", "SSC-A"), overlayGates = NULL){
    # Server Function ==========================================================
    transServer <- function(input, output, session) {
        # Prepare main panel plot ----------------------------------------------
        TPlot <- shiny::reactive(prepareTransPlot(
            gs, sample, dims, node, input$transBins, input$transUseCoords, 
            c(input$transXMin, input$transXMax, input$transYMin, 
              input$transYMax), input$transUseBiex, overlayGates, 
            input$transScaleToggle, input$transScaleX, input$transScaleY, 
            input$transRotate, input$transShiftX, input$transShiftY))
        
        output$transPlot <- shiny::renderPlot(TPlot(), height = function() {
            session$clientData$output_transPlot_width})
        # Apply gate and close -------------------------------------------------
        shiny::observeEvent(input$transDone, {
            if(input$transScaleToggle == "uniform"){
                scaleDims <- 1
            }else if(input$transScaleToggle == "independent"){
                scaleDims <- 2
            }
            updateGate(
                gs, node, scaleDims, 
                scale = c(input$transScaleX, input$transScaleY), 
                deg = input$transRotate, dx = input$transShiftX, 
                dy = input$transShiftY
            )
            shiny::stopApp()})}
    shiny::runApp(shiny::shinyApp(uiTransform, transServer))
}

# Helpers ----------------------------------------------------------------------

updateGate <- function(gs, node, scaleDims, scale, deg, dx, dy){
    gate <- flowWorkspace::gh_pop_get_gate(gs[[1]], node)
    if(is(gate, "rectangleGate")){
        deg <- NULL
    }
    if(scaleDims == 1){
        flowCore::transform_gate(gs, y = node, scale = scale[[1]], deg = deg, 
                                 dx = dx, dy = dy)
    }else if(scaleDims == 2){
        flowCore::transform_gate(gs, y = node, scale = scale, deg = deg, 
                                 dx = dx, dy = dy)
    }
    
    flowWorkspace::recompute(gs)
}


prepareTransPlot <- function(gs, sample, dims, node, bins, useCoords, coords, 
                             useBiex, overlayGates, scaleMode, scaleX, scaleY, 
                             rotate, shiftX, shiftY){
    
    sample.gs <- gs[[sample]]
    
    gg <- prepTransPlot(sample.gs, dims, node, bins, useCoords, coords, useBiex)
    
    if(!is.null(overlayGates)){gg <- gg + geom_gate(overlayGates)}
    
    gate <- flowWorkspace::gh_pop_get_gate(sample.gs, node)
    
    if(is(gate, "rectangleGate")){
        if(scaleMode == "uniform"){
            newGate <- flowWorkspace::gh_pop_get_gate(sample.gs, node) |>
                flowCore::transform_gate(scale = scaleX, dx = shiftX, 
                                         dy = shiftY)
        }else if(scaleMode == "individual"){
            newGate <- flowWorkspace::gh_pop_get_gate(sample.gs, node) |>
                flowCore::transform_gate(scale = c(scaleX, scaleY),
                                         dx = shiftX, dy = shiftY)
        }
    }else{
        if(scaleMode == "uniform"){
            newGate <- flowWorkspace::gh_pop_get_gate(sample.gs, node) |>
                flowCore::transform_gate(scale = scaleX, deg = rotate, dx = shiftX,
                                         dy = shiftY)
        }else if(scaleMode == "individual"){
            newGate <- flowWorkspace::gh_pop_get_gate(sample.gs, node) |>
                flowCore::transform_gate(scale = c(scaleX, scaleY), deg = rotate,
                                         dx = shiftX, dy = shiftY)
        }
    }
    
    if(is(newGate, "rectangleGate")){
        newGate <- ggcyto:::fortify.rectangleGate(newGate)
    }else if(is(newGate, "polygonGate")){
        newGate <- ggcyto:::fortify.polygonGate(newGate)
    }
    
    gg <- gg + geom_path(data = newGate, colour = "firebrick")
    
    gg <- ggcyto::as.ggplot(gg)
    
    return(gg)
}

prepTransPlot <- function(sample.gs, dims, node, bins, useCoords, coords, useBiex){
    if(length(dims) > 2){
        warning("The first two dims will be used, the others discarded.")
    }
    
    if(length(dims) == 1){
        gg <- ggcyto::ggcyto(sample.gs, aes(!!dims[[1]])) +
            geom_density() + 
            geom_gate(node, colour = "grey50") +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) + 
            theme_flowGate
        if(useCoords){
            gg <- gg + coord_cartesian(xlim = c(coords[[1]], coords[[2]]))
        }
    } else {
        gg <- ggcyto::ggcyto(sample.gs, aes(!!dims[[1]], !!dims[[2]])) +
            geom_hex(bins = bins) + 
            geom_gate(node, colour = "grey50") +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) + 
            theme_flowGate
        if(useCoords){
            gg <- gg + coord_cartesian(xlim = c(coords[[1]], coords[[2]]),
                                       ylim = c(coords[[3]], coords[[4]]))
        }
    }
    
    if(useBiex){
        suppressMessages(if(length(dims) == 1){
            gg <- gg + ggcyto::scale_x_flowjo_biexp()
        }else{
            gg <- gg + ggcyto::scale_x_flowjo_biexp() +
                ggcyto::scale_y_flowjo_biexp()
        })
    }
    return(gg)
}
