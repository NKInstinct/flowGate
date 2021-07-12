#' Sequentially apply a manual gating strategy to a GatingSet or list
#'
#' This function allows for a "semi-automatic" approach to interactive manual
#' gating of flow cytometry data. It leverages the purrr package to let you
#' easily define a gating strategy and then apply it sequentially to a
#' GatingSet. This will call gs_gate_interactive() once for each line in your
#' gating strategy, applying it to your GatingSet as soon as you draw each
#' prompted gate.
#'
#' The gating strategy should be a tibble, with each column name corresponding
#' to one parameter from gs_gate_interactive. Any parameters not specified in
#' this tibble will either use their defaults from gs_gate_interactive or can be
#' specified directly in the function call to gs_apply_gating_strategy.
#' Typically, this gating strategy will have a column for 'filterId', 'dims',
#' 'subset', and 'coords', but techinicaly only filterId is required. See
#' examples below for an easy way to construct this strategy using tribble().
#'
#' If you instead supply a list of GatingSets rather than a single one, this
#' function will apply your entire gating strategy to the entire list in one of
#' two ways, as specified by you.
#'
#' In "individual" mode, this function will apply
#' the gating strategy to the first GatingSet in the list, then the second, and
#' so on, prompting you to draw a new gate each time. For example, a list of
#' four GatingSets with a gating strategy containing three gates will ask you to
#' draw twelve gates total, applying all three gates to the first GatingSet
#' before moving on to the second.
#'
#' In "reference" mode, you provide the function a single reference GatingSet
#' within the list (defaults to the first one) to apply your gating strategy
#' manually. It will then copy the strategy to all the other GatingSets in the
#' list automatically. In the above example, this mode would ask you to draw
#' just three gates and then apply them to all four GatingSets.
#'
#' @param gs A GatingSet or list of GatingSets.
#' @param gating_strategy A tibble-formatted gating strategy (see examples below)
#' @param mode A string specifying "individual" or "reference". If gs is a list
#'   of GatingSets, this param controls whether you will apply the gating
#'   strategy to each GatingSet in the list individually ("individual") or
#'   whether you will apply it once to a reference GatingSet ("reference") and
#'   then copy the resulting gates onto each other GatingSet in the list.
#'   Ignored if gs is a single GatingSet.
#' @param reference_sample A numeric specifying the position of the reference
#'   GatingSet in the list of GatingSets. Only used if gs is a list of
#'   GatingSets and mode is "reference".
#' @param ... Other parameters to pass to gs_gate_interactive(). Note that only
#'   constant parameters should be supplied here---anything that varies should
#'   be included in the gating_strategy tibble.
#'
#' @return the GatingSet or list of GatingSets with the gates in gating_strategy
#'   applied as specified.
#'
#' @importFrom tibble tribble
#'
#' @examples
#'
#' fs <- flowCore::read.flowSet(path = system.file("extdata", package = "flowGate"),
#'                              pattern = ".FCS$")
#'
#' gs <- flowWorkspace::GatingSet(fs)
#'
#' # Note - this is a very rudamentary GatingSet for example purposes only. Please
#' # see the vignette accompanying this package or the flowWorkspace
#' # documentation # for a complete look at creating a GatingSet.
#'
#' gating_strategy <- tibble::tribble(
#' ~filterId,      ~dims,                       ~subset,        ~coords,
#' "Lymphocytes", list("FSC-H", "SSC-H"),       "root",         list(c(0, 3e5), c(0, 3e5)),
#' "CD45 CD15",   list("CD45 PE", "CD15 FITC"), "Lymphocytes",  list(c(0, 3e5), c(0, 2e5)),
#' )
#'
#'
#' if(interactive()){ # only run in interactive sessions.
#' gs_apply_gating_strategy(gs,
#' gating_strategy = gating_strategy,
#' bins = 512) # note that extra args for gs_gate_interactive can be supplied.
#' }
#'
#'
#'
#'
#' @export
gs_apply_gating_strategy <- function(gs,
                                     gating_strategy,
                                     mode = "individual",
                                     reference_sample = 1,
                                     ...){
  if(is(gs, "GatingSet")){

    purrr::pwalk(gating_strategy, flowGate::gs_gate_interactive, gs = gs, ...)
  } else if(is.list(gs)){

    if(mode == "individual"){
      purrr::walk(gs,
                  ~purrr::pwalk(gating_strategy,
                                flowGate::gs_gate_interactive,
                                gs = ..1,
                                ...))

    } else if(mode == "reference"){
      gsRef <- gs[[reference_sample]]
      gsNon <- gs[-reference_sample]

      purrr::pwalk(gating_strategy, flowGate::gs_gate_interactive, gs = gsRef, ...)

      for(i in seq_along(gating_strategy$filterId)){
        purrr::walk(gsNon,
                    ~flowWorkspace::gs_pop_add(..1,
                                               gate = flowWorkspace::gh_pop_get_gate(gsRef[[1]],
                                                  gating_strategy$filterId[[i]]),
                                               parent = gating_strategy$subset[[i]]))
      }

      purrr::walk(gs, flowWorkspace::recompute)
    } else{
      stop("'mode' must be 'individual' or 'reference'")
    }
  } else{
    stop("'gs' must be a GatingSet or a list of GatingSets")
  }
  return(gs)

}
