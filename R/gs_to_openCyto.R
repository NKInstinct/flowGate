
#' Converts an existing GatingSet object into an openCyto template,
#' based off gates existing for given sample. Experimental
#' 
#' @param gs A GatingSet object
#' @param sample The sample index to use, default is 1
#' @param outpath The file.path to the storage location. Default NULL will
#' result in creation within the current working directory
#' 
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom stringr str_equal
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom utils write.csv
#' 
#' @return A .csv file containing an openCyto style template for
#' subsequent re-gating attempts
#' 
#' @export
#' 
#' @examples A <- 2 + 2
#' 
gs_to_openCyto <- function(gs, sample=1, outpath=NULL, filename="openCytoTemplate"){
  pops <- gs_get_pop_paths(gs)
  pops <- pops[!str_equal("root", pops)]

  # x <- pops[1]
  CSV <- map(.x=pops, .f=gate_detallitos, gs=gs, sample=sample) |> bind_rows()

  if (is.null(outpath)){outpath <- getwd()}

  TheFilename <- paste0(filename, ".csv")
  StorageLocation <- file.path(outpath, TheFilename)
  write.csv(CSV, StorageLocation, row.names=FALSE)
}

#' Internal for gs_to_openCyto, retrieves details for individual gates
#' 
#' @param x The iterated in gate
#' @param gs The GatingSet object
#' @param sample The sample index being used for the openCyto template values
#' 
#' @importFrom flowWorkspace gs_pop_get_gate gs_pop_get_parent 
#' 
#' @return A data.frame row
#' 
#' @noRd
#'
gate_detallitos <- function(x, gs, sample){

  TheGate <- gs_pop_get_gate(gs[sample], x)[[1]]

  if (is(TheGate, "rectangleGate")){

    # span_gate
    TheMin <- TheGate@min
    TheMax <- TheGate@max
    TheDims <- names(TheGate@min)
    TheMethod <- "span_gate"
    coords <- paste(c(TheMin, TheMax), collapse = ",")

    Data <- data.frame(
      alias       = TheGate@filterId,
      pop         = "+",
      parent      = gs_pop_get_parent(gs[sample], x),
      dims        = TheDims,
      gating_method = TheMethod,
      gating_args = sprintf('min=c(%s),max=c(%s)', TheMin, TheMax),
      collapseDataForGating = "FALSE",
      groupBy     = "NA",
      preprocessing_method = "NA",
      preprocessing_args   = "NA"
    )

  } else if (is(TheGate, "polygonGate")){

    # polygon_gate
    TheVector <- TheGate@boundaries
    TheDims <- colnames(TheVector)
    if(length(TheDims) > 1){TheDims <- paste(TheDims, collapse = ",")}
    coords <- paste(c(TheVector[,1], TheVector[,2]), collapse = ",")

    Data <- data.frame(
      alias       = TheGate@filterId,
      pop         = "+",
      parent      = gs_pop_get_parent(gs[sample], x),
      dims        = TheDims,
      gating_method = "polygon_gate",
      gating_args = sprintf('matrix(c(%s),ncol=2)', coords),
      collapseDataForGating = "FALSE",
      groupBy     = "NA",
      preprocessing_method = "NA",
      preprocessing_args   = "NA"
    )
  }

  return(Data)
}

#' Defines polygon_gate for openCyto
#' 
#' @importFrom flowCore polygonGate
#' 
#' @noRd
.my_polygon <- function(fr, pp_res, channels, boundaries, ...) {
  colnames(boundaries) <- channels
  flowCore::polygonGate(.gate = boundaries)
}

#' Defines span_gate for openCyto
#'
#' @importFrom flowCore rectangleGate
#'
#' @noRd
.my_span <- function(fr, pp_res, channels, min, max, ...) {
  gate_range <- matrix(c(min, max), nrow = 2, dimnames = list(c("min", "max"), channels))
  flowCore::rectangleGate(.gate = gate_range)
}

#' Ensures span_gate and polygon_gate are available to openCyto
#'
#' @importFrom openCyto register_plugins
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  openCyto::register_plugins(fun = .my_polygon, methodName = "polygon_gate")
  openCyto::register_plugins(fun = .my_span,    methodName = "span_gate")
}