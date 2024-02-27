#' Plot results from EBASE 
#'
#' @param res output data frame from \code{\link{ebase}}
#' @param instantaneous logical indicating if results are instantaneous (default) or averaged to daily
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details All metabolic estimates are plotted as positive values (\code{D} is represented as net ingassing).
#' 
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(doParallel)
#' 
#' # get four days of data
#' dat <- exdat %>% 
#'   filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:4)
#' 
#' # run ebase with defaults, parallel
#' 
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' res <- ebase(dat, interval = 900, Z = 1.85, progress = TRUE, n.chains = 2)
#' 
#' stopCluster(cl)
#'
#' # plot instantaneous
#' ebase_plot(res)
#' 
#' # plot daily-averaged
#' ebase_plot(res, instantaneous = FALSE)
ebase_plot <- function(res, instantaneous = TRUE){
  
  # instantaneous results
  if(instantaneous){
    
    ylab <- expression(paste('Instantaneous (', O [2], ' mmol ', m^-2, ' ', d^-1, ')'))
    
    toplo <- res %>% 
      dplyr::select(DateTimeStamp, P, R, D) %>% 
      na.omit() %>% 
      tidyr::pivot_longer(cols = -matches('DateTimeStamp')) %>% 
      dplyr::rename(xval = DateTimeStamp)

  }  

  # daily averaged results
  if(!instantaneous){
    
    ylab <- expression(paste('Daily-averaged (', O [2], ' mmol ', m^-2, ' ', d^-1, ')'))
    
    toplo <- res %>% 
      dplyr::select(Date, P, R, D) %>% 
      tidyr::pivot_longer(cols = -matches('Date')) %>% 
      dplyr::group_by(Date, name) %>% 
      dplyr::summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% 
      dplyr::rename(xval = Date)
    
  }

  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = xval, y = value, color = name)) +
    ggplot2::geom_line() + 
    ggplot2::geom_point() + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::scale_color_discrete(
      breaks = c('P', 'R', 'D'),
      labels = c(expression(P), expression(R), expression(D))
    ) +
    ggplot2::labs(
      y = ylab, 
      color = NULL, 
      x = NULL
  )
  
  return(p)
  
}
