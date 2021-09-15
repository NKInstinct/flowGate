#' Prepare Plot for Interactive Gating
#'
#' A helper function for gs_gate_interactive to prepare the click-able flow
#' plots from ggcyto. All params are pulled from the parent call to
#' gs_gate_interactive.
#'
#' @param gs The GatingSet to use.
#' @param filterId Name of the gate to be drawn.
#' @param sample Which sample within the GatingSet to use for the plot (gates
#'   will be applied to all samples).
#' @param dims List of X and (optionally) Y parameters to plot.
#' @param subset The parent gate from which to draw the current plot.
#' @param bins Passed to geom_hex; how fine resolution the plot should be.
#' @param coords Passed to coord_cartesian; list of length-2 specifying the plot
#'   dimensions.
#' @param regate Boolean; should the gs first be stripped of gates matching the
#'   filterId?
#' @param overlayGates List of filterIds to plot on top of the current plot.
#'
#' @return A ggplot object ready to pass into the shiny app.
#' @noRd
#'
preparePlot <- function(gs,
                        sample,
                        dims,
                        subset,
                        bins,
                        coords,
                        overlayGates,
                        addGateType,
                        addCoords,
                        useBiex,
                        x_max,
                        x_wide,
                        x_pos,
                        x_neg,
                        y_max,
                        y_wide,
                        y_pos,
                        y_neg){
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
  
  if(addGateType == "polygonGate"){
    if(nrow(addCoords) > 1){
      gg <- gg + 
        geom_path(data = addCoords,
                  aes(.data$x, .data$y),
                  inherit.aes = FALSE)
    }
  }else if(addGateType == "quadGate"){
    gg <- gg + 
      ggplot2::geom_vline(xintercept = addCoords$X) +
      ggplot2::geom_hline(yintercept = addCoords$Y)
  }
  
  if(useBiex){
    if(length(dims)==1){
      gg <- gg + 
        ggcyto::scale_x_flowjo_biexp(maxValue = x_max,
                                     widthBasis = x_wide,
                                     pos = x_pos,
                                     neg = x_neg)
    } else{
      gg <- gg + 
        ggcyto::scale_x_flowjo_biexp(maxValue = x_max,
                                     widthBasis = x_wide,
                                     pos = x_pos,
                                     neg = x_neg) +
        ggcyto::scale_y_flowjo_biexp(maxValue = y_max,
                                     widthBasis = y_wide,
                                     pos = y_pos,
                                     neg = y_neg)
    }
  }
  gg <- ggcyto::as.ggplot(gg)
  return(gg)
}


theme_flowGate <- theme_gray() +
  theme(
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    # axis.line = element_blank(),
    # axis.ticks = element_blank(),
    # title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
