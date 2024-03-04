#' Plot credible intervals for a, R, and b
#' 
#' @param res output data frame from \code{\link{ebase}}
#' @param params character vector indicating which parameters to plot, one to any of \code{a}, \code{R}, or \code{b},  (default all)
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details This function plots 95% credible intervals (2.5th to 97.5th percentiles, approximate posterior distributions) for \code{a}, \code{R} and/or \code{b} using the output from \code{\link{ebase}}. Results in the plot are grouped by the \code{ndays} argument that was used in \code{\link{ebase}}.
#' 
#' @examples 
#' # plot credible intervals
#' credible_plot(exres)
credible_plot <- function(res, params = c('a', 'R', 'b')){
  
  toplo <- credible_prep(res, params = params, labels = TRUE)
 
  toplo2 <- na.omit(toplo)
                    
  p <- ggplot2::ggplot(toplo2, ggplot2::aes(x = Date, y = mean, group = grp)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.1)  +
    ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y', labeller = ggplot2::label_parsed, strip.position = 'left') +
    ggplot2::theme_minimal() + 
    ggplot2::coord_cartesian(xlim = range(toplo$Date)) + 
    ggplot2::theme(
      strip.placement = 'outside', 
      strip.text = ggplot2::element_text(size = ggplot2::rel(1))
    ) + 
    ggplot2::labs(
      y = NULL,
      x = NULL
    )
  
  return(p)
  
}
