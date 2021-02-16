#' Gate Handlers for gs_gate_interactive
#'
#' These two functions convert the brushes and clicks on the plot generated in
#' gs_gate_interactive into usable dimensions for flowCore's filter definition
#' functions
#'
#' @param brush a reactive from the shiny app referring to brushes on the plot.
#' @param click a reactive from the shiny app referring to clicks on the plot.
#' @param gateType an input from the shiny app showing the type of gate the user
#'   wishes to draw.
#'
#' @return A set of gate coordinates formatted according to the gate type chosen
#'   by the user.
#'
#' @noRd
#'
coordBrush <- function(brush, gateType){
  if(gateType == "rectangleGate"){
    res <- list("X" = c(brush$xmin, brush$xmax),
                   "Y" = c(brush$ymin, brush$ymax))
  } else if(gateType == "spanGate"){
    res <- list("X" = c(brush$xmin, brush$xmax))
  }
  return(res)
}

coordClick <- function(click, gateType){
  if(gateType == "polygonGate"){
    res <- data.frame("x" = click$x,
                      "y" = click$y)
  } else if(gateType == "quadGate"){
    res <- list("X" = click$x,
                "Y" = click$y)
  }
  return(res)
}
