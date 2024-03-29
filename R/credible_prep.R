#' Get credible intervals for a, R, b
#' 
#' @param res output data frame from \code{\link{ebase}}
#' @param params character vector indicating which parameters to plot, one to any of \code{a}, \code{R}, or \code{b} (default all)
#' @param labels logical indicating of parameter labels are output as an expression for parsing in plot facets, default \code{FALSE}
#'
#' @return A data frame
#' @export
#'
#' @details This function gets 95% credible intervals (2.5th to 97.5th percentiles, approximate posterior distributions) for \code{a}, \code{R}, and/or \code{b} using the output from \code{\link{ebase}}. The function is used in \code{\link{credible_plot}}, but is provided as a separate function for convenience.
#' 
#' @examples 
#' # get credible intervals
#' credible_prep(exres)
credible_prep <- function(res, params = c('a', 'R', 'b'), labels = FALSE){
  
  chk <- params %in% c('a', 'R', 'b') 
  if(any(!chk))
    stop('param must be a, R, and/or b') 
  
  paramslo <- paste0(params, 'lo') 
  paramshi <- paste0(params, 'hi')
  
  labs <- c('a', 'R', 'b')
  if(labels)
    labs <- c('italic(a)~(mmol~m^{-2}~d^{-1})/(W~m^{-2})', 
              'italic(R)~(mmol~m^{-2}~d^{-1})',
              'italic(b)~(cm~hr^{-1})/(m^{2}~s^{-2})'
    )

  out <- res %>% 
    dplyr::select(dplyr::any_of(c('Date', 'grp', params, paramslo, paramshi))) %>% 
    na.omit() %>% 
    unique() %>% 
    tidyr::pivot_longer(-c('Date', 'grp'), names_to = 'var', values_to = 'val') %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(
      val = dplyr::case_when(
        Date != min(Date) ~ NA_real_, 
        T ~ val
      )
    ) %>% 
    dplyr::mutate(
      var = dplyr::case_when(
        !grepl('lo|hi', var) ~ paste0(var, 'mean'),
        T ~ var
      ), 
      est = gsub(paste(paste0('^', params), collapse = '|'), '', var),
      var = gsub('lo$|hi$|mean$', '', var), 
      var = factor(var, 
                   levels = c('a', 'R', 'b'), 
                   labels = labs
      )
    ) %>% 
    tidyr::pivot_wider(names_from = 'est', values_from = 'val')
 
  return(out)
  
}
