#' Create a diagnostic plot showing interpolated values prior to metabolism estimates
#'
#' @inheritParams ebase_prep
#' @param param character string of the parameter to plot, one of \code{DO_obs}, \code{DO_sat}, \code{H}, \code{Temp}, \code{Sal}, \code{PAR}, \code{WSpd}, or \code{sc}
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#' 
#' @details Missing values in the input data can also be interpolated prior to estimating metabolism.  This is the default behavior and it is the responsibility of the user to verify that these interpolated values are not wildly inaccurate.  Missing values are linearly interpolated between non-missing values at the time step specified by the value in \code{interval}.  This works well for small gaps, but can easily create inaccurate values at gaps larger than a few hours. The plot from this function can be used to visually assess the interpolated gaps. 
#' 
#' @importFrom dplyr %>%
#'
#' @examples 
#' library(dplyr)
#' library(lubridate)
#' 
#' # get four days of data
#' dat <- exdat %>% 
#'   filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:4)
#'   
#' # create missing values
#' set.seed(222)
#' dat <- dat %>% 
#'   slice_sample(prop = 0.9) %>% 
#'   arrange(DateTimeStamp)
#'
#' interp_plot(dat, H = 1.85, interval = 900, param = 'DO_sat')
interp_plot <- function(dat, param = c('DO_obs', 'DO_sat', 'H', 'Temp', 'Sal', 'PAR', 'WSpd', 'sc'), H, interval, ndays = 1, interp = TRUE, maxgap = 1e6){
  
  param <- match.arg(param)
  
  labs <- c('DO (mmol/m3/)', 'DO sat (mmol/m3)', 'Water column height (m)', 'Water temperature (C)', 'Salinity (psu)', 'PAR (W/m2/s)', 'Wind speed (m/2)', 'Schmidt number')
  names(labs) <- c('DO_obs', 'DO_sat', 'H', 'Temp', 'Sal','PAR', 'WSpd', 'sc')

  dat <- ebase_prep(dat, H = H, interval = interval, ndays = ndays, interp = interp, maxgap = maxgap)

  toplo <- dat %>% 
    dplyr::rename(yval = !!param)

  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = DateTimeStamp, y = yval)) +
    ggplot2::geom_line() + 
    ggplot2::geom_point(ggplot2::aes(color = isinterp)) + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::labs(
      y = labs[param],
      color = "Interpolated?",
      x = NULL
    )
  
  return(p)
  
}