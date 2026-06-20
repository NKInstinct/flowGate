#' The purpose of this function is to allow the readjustment of a gate on an
#' individual specimen via the Shiny app, quickly iterating through every
#' specimen present in the GatingSet. 
#' 
#' This allows for fine-tuning of the gate placement for non-representative
#' specimens, or when automated gating methods fail. 
#'
#' @param gs A GatingSet or list of GatingSets.
#' @param gate A tibble-formatted gating strategy (see examples below)
#' @param sample Sample index in the GatingSet, default NULL will iterate through
#' all specimens in the GatingSet.
#' @param ... Other parameters to pass to gs_gate_interactive(). Note that only
#'   constant parameters should be supplied here---anything that varies should
#'   be included in the gating_strategy tibble.
#'
#' @return the GatingSet or list of GatingSets with the gates in gating_strategy
#'   applied as specified.
#'
#' @importFrom tibble tribble
#' @importFrom methods is
#' @importFrom purrr pmap
#'
#' @examples
#'
#' fs <- flowCore::read.flowSet(
#'   path = system.file("extdata", package = "flowGate"), pattern = ".FCS$")
#'
#' gs <- flowWorkspace::GatingSet(fs)
#'
#' # Note - this is a very rudamentary GatingSet for example purposes only. 
#' # Please see the vignette accompanying this package or the flowWorkspace
#' # documentation # for a complete look at creating a GatingSet.
#'
#' gating_strategy <- tibble::tribble(
#' ~filterId,      ~dims,                       ~subset,        ~coords,
#' "Lymphocytes", list("FSC-H", "SSC-H"),       "root",         list(c(0, 3e5), c(0, 3e5)),
#' "CD45 CD15",   list("CD45 PE", "CD15 FITC"), "Lymphocytes",  list(c(0, 3e5), c(0, 2e5)),
#' )
#'
#'
#' if(interactive()){
#' gs_apply_gating_strategy(gs,
#' gating_strategy = gating_strategy,
#' bins = 512) # note that extra args for gs_gate_interactive can be supplied.
#' }
#' @export
#' 
gs_apply_gate_check <- function(gs, gate, sample=NULL, ...){
    if(methods::is(gs, "GatingSet")){

      if(is.null(sample)){
        Samples <- seq_along(gs)
      } else {Samples <- sample}

    purrr::walk(
      .x = Samples,
      .f = function(sample) {
        gs_gate_interactive_adjust(
          gs       = gs,
          gate = gate,
          sample   = sample,
          ...
        )
      }
    )
      
    } else {
        stop("'gs' must be a GatingSet")
    }
}