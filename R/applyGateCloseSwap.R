#' Apply gate from gs_gate_interactive
#'
#' @param coords The coordinates of the interactively drawn gate.
#' @param gateType The selected type of gate (from UI).
#' @param filterId The gate name specified by the user.
#' @param gg The plot object from vars$plot.
#' 
#' @importFrom flowCore polygonGate rectangleGate quadGate
#' @importFrom flowWorkspace gs_pop_add recompute
#'
#' @return The original GatingSet with the newly drawn gate applied.
#'
#' @noRd
#'
applyGateCloseSwap <- function(
    gs, subset, coords, gateType, filterId, gg, useBiex, bins, xMax, xWidth, 
    xPos, xNeg, yMax, yWidth, yPos, yNeg, sample){
    
    if(gateType == "polygonGate"){
        names(coords) <- c(names(gg$data)[[3]], names(gg$data)[[4]])
        coords <- as.matrix(coords)
        gate <- flowCore::polygonGate(coords, filterId = filterId)
    } else if(gateType == "spanGate"){
        names(coords) <- c(names(gg$data))[[3]]
        gate <- flowCore::rectangleGate(coords, filterId = filterId)
    } else if(gateType == "quadGate"){
        names(coords) <- c(names(gg$data)[[3]], names(gg$data)[[4]])
        gate <- flowCore::quadGate(coords, filterId = filterId)
    } else if(gateType == "rectangleGate"){
        names(coords) <- c(names(gg$data)[[3]], names(gg$data)[[4]])
        gate <- flowCore::rectangleGate(coords, filterId = filterId)
    }

    # gs_pop_add(gs, gate, parent = subset)
    TheList <- list(gate)
    names(TheList) <- sampleNames(gs[sample])
    gs_pop_set_gate(gs[sample], filterId, TheList)
    recompute(gs[sample])
    
    if(useBiex){
        varsBiex <- list(X = list(
            maxValue = xMax, widthBasis = xWidth, pos = xPos, neg = xNeg),
                         Y = list(
            maxValue = yMax, widthBasis = yWidth, pos = yPos, neg = yNeg))
    } else{
        varsBiex <- "unused"
    }
    output <- list("Gate" = gate, "Bins" = bins, "Scaling" = varsBiex)
    return(output)
}
