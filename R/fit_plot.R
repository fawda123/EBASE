#' Plot observed and modeled dissolved oxygen
#' 
#' @param res output data frame from \code{\link{ebase}}
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details Dissolved oxygen (mmol/m3) is plotted as observed from the input data (points) and modelled (lines) based on inputs to \code{\link{ebase}} 
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
#' # plot observed and modeled DO
#' fit_plot(res)
fit_plot <- function(res){
    
  toplo <- res
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = DateTimeStamp, y = DO_obs)) + 
    ggplot2::geom_point(ggplot2::aes(color = 'Observed')) + 
    ggplot2::geom_line(ggplot2::aes(y = DO_mod, color = 'Estimated')) + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = c(NA, 16), linetype = c(1, NA)))
    ) +
    ggplot2::labs(
      y = "Dissolved Oxygen (mmol/m3)",
      color = NULL,
      x = NULL
    )
  
  return(p)
  
}
