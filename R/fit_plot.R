#' Plot observed and modeled dissolved oxygen
#' 
#' @param res output data frame from \code{\link{ebase}}
#' @param bygroup logical indicating if the plot is faceted by group
#' @param scatter logical indicating if a scatter plot of modeled versus estimated dissolved oxygen is returned
#' @param showfit logical indicating if a linear fit is shown in the plot, applies only if \code{scatter = TRUE}
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details Dissolved oxygen (mmol/m3) is plotted as observed from the input data (points) and modeled (lines) based on inputs to \code{\link{ebase}} if \code{scatter = FALSE}.  A scatter plot of modeled versus estimated dissolved oxygen is returned if \code{scatter = TRUE}, including a linear fit if \code{showfit = TRUE}.  The plot is faceted by group based on the \code{ndays} argument to \code{\link{ebase}} if \code{bygroup = TRUE}.  The r-squared value of the fit between modeled and observed dissolved oxygen is also shown in the facet label for the group if \code{bygroup = TRUE}.
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
#' 
#' # plot observed and modeled DO by group
#' fit_plot(res, bygroup = TRUE)
#' 
#' # as scatter plot
#' fit_plot(res, scatter = TRUE)
#' 
#' # as scatter plot by group
#' fit_plot(res, scatter = TRUE, bygroup = TRUE)
#' 
fit_plot <- function(res, bygroup = FALSE, scatter = FALSE, showfit = TRUE){
    
  toplo <- res %>% 
    dplyr::mutate(
      grp = paste('Group', grp),
      rsq = paste0('R.Sq. ', round(100 * rsq, 0), '%')
    ) %>% 
    tidyr::unite(grp, c('grp', 'rsq'), sep = ' ,')
  
  if(!scatter)
    p <- ggplot2::ggplot(toplo, ggplot2::aes(x = DateTimeStamp, y = DO_obs, group = grp)) + 
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
  
  if(scatter){
    
    p <- ggplot2::ggplot(toplo, ggplot2::aes(x = DO_obs, y = DO_mod, group = grp)) + 
      ggplot2::geom_point() + 
      ggplot2::theme_minimal() +
      ggplot2::labs(
        y = "Modeled Dissolved Oxygen (mmol/m3)",
        x = "Observed Dissolved Oxygen (mmol/m3)"
      )
    
    if(showfit)
      p <- p +       
        ggplot2::geom_smooth(formula = y ~ x, method = 'lm', se = FALSE)
    
  }
  
  if(bygroup)
    p <- p + 
      ggplot2::facet_wrap(~grp, scales = 'free')
  
  return(p)
  
}
