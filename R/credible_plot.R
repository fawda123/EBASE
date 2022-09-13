#' Plot credible intervals for a, b, and Rt_vol
#' 
#' @param res output data frame from \code{\link{ebase}}
#' @param params character vector indicating which parameters to plot, one to any of \code{a}, \code{b}, or \code{Rt_vol} (default all)
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details This function plots 95% credible intervals (2.5th to 97.5th percentiles) for \code{a}, \code{b}, and/or \code{Rt_vol} using the output from \code{\link{ebase}}. Results in the plot are grouped by the \code{ndays} argument that was used in \code{\link{ebase}}.
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
credible_plot <- function(res, params = c('a', 'b', 'Rt_vol')){
  
  chk <- params %in% c('a', 'b', 'Rt_vol') 
  if(any(!chk))
    stop('param must be a, b, and/or Rt_vol') 
    
  paramslo <- paste0(params, 'lo') 
  paramshi <- paste0(params, 'hi')

  toplo <- res %>% 
    dplyr::select(any_of(c('Date', 'grp', params, paramslo, paramshi))) %>% 
    unique() %>% 
    tidyr::pivot_longer(-c('Date', 'grp'), names_to = 'var', values_to = 'val') %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(
      val = dplyr::case_when(
        Date != min(Date) ~ NA_real_, 
        T ~ val
      )
    ) %>% 
    mutate(
      var = case_when(
        !grepl('lo|hi', var) ~ paste0(var, 'mean'),
        T ~ var
      ), 
      est = gsub(paste(paste0('^', params), collapse = '|'), '', var),
      var = gsub('lo$|hi$|mean$', '', var), 
      var = factor(var, 
                   levels = c('a', 'b', 'Rt_vol'), 
                   labels = c('a~(mmol~m^{-3}~d^{-1})(W~m^{-2})', 
                              'b~(cm~hr^{-1})(m^{2}~s^{-2})',
                              'Rt[vol]~(mmol~m^{-3}~d^{-1})'
                   )
      )
    ) %>% 
    tidyr::pivot_wider(names_from = 'est', values_from = 'val')
  
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
