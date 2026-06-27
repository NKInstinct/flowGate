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
applyGateClose <- function(
    gs, subset, coords, gateType, filterId, gg, useBiex, bins, xMax, xWidth, 
    xPos, xNeg, yMax, yWidth, yPos, yNeg){
    
    TheNames <- names(gg$data)
    LastTwo  <- tail(TheNames, 2)
    FinalOne  <- tail(TheNames, 1)

    if(gateType == "polygonGate"){
        names(coords) <- LastTwo
        coords <- as.matrix(coords)
        gate <- flowCore::polygonGate(coords, filterId = filterId)
    } else if(gateType == "spanGate"){
        names(coords) <- FinalOne
        gate <- flowCore::rectangleGate(coords, filterId = filterId)
    } else if(gateType == "quadGate"){
        names(coords) <- LastTwo
        gate <- flowCore::quadGate(coords, filterId = filterId)
    } else if(gateType == "rectangleGate"){
        names(coords) <- LastTwo
        gate <- flowCore::rectangleGate(coords, filterId = filterId)
    }

    gs_pop_add(gs=gs, gate=gate, parent = subset)
    recompute(gs)
    
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
