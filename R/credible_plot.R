#' Plot credible intervals for a, Rt_vol, and b
#' 
#' @param res output data frame from \code{\link{ebase}}
#' @param params character vector indicating which parameters to plot, one to any of \code{a}, \code{Rt_vol}, or \code{b},  (default all)
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details This function plots 95% credible intervals (2.5th to 97.5th percentiles, approximate posterior distributions) for \code{a}, \code{Rt_vol} and/or \code{b} using the output from \code{\link{ebase}}. Results in the plot are grouped by the \code{ndays} argument that was used in \code{\link{ebase}}.
#' 
#' @examples 
#' 
#' library(dplyr)
#' library(lubridate)
#' library(doParallel)
#' 
#' # get four days of data
#' dat <- exdat %>% 
#'   filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:4)
#' 
#' ##
#' # run ebase with defaults, parallel
#' 
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2)
#' 
#' stopCluster(cl)
#' 
#' # plot credible intervals
#' credible_plot(res)
credible_plot <- function(res, params = c('a', 'Rt_vol', 'b')){
  
  toplo <- credible_prep(res, params = params, labels = TRUE)
 
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = Date, y = mean, group = grp)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.1)  +
    ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y', labeller = ggplot2::label_parsed, strip.position = 'left') +
    ggplot2::theme_minimal() + 
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
