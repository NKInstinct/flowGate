#' Apply gate from gs_gate_interactive
#'
#' @param coords The coordinates of the interactively drawn gate.
#' @param gateType The selected type of gate (from UI).
#' @param filterId The gate name specified by the user.
#' @param gg The plot object from vars$plot.
#'
#' @return The original GatingSet with the newly drawn gate applied.
#'
#' @noRd
#'
applyGateClose <- function(coords, gateType, filterId, gg){
  if(gateType == "polygonGate"){
    names(coords) <- c(names(gg[[1]])[[3]],
                       names(gg[[1]])[[4]])
    coords <- as.matrix(coords)
    gate <- flowCore::polygonGate(coords, filterId = filterId)
  } else if(gateType == "spanGate"){
    names(coords) <- c(names(gg[[1]])[[3]])
    gate <- flowCore::rectangleGate(coords, filterId = filterId)
  } else if(gateType == "quadGate"){
    names(coords) <- c(names(gg[[1]])[[3]],
                       names(gg[[1]])[[4]])
    gate <- flowCore::quadGate(coords, filterId = filterId)
  } else if(gateType == "rectangleGate"){
    names(coords) <- c(names(gg[[1]])[[3]],
                       names(gg[[1]])[[4]])
    gate <- flowCore::rectangleGate(coords, filterId = filterId)
  }
}
