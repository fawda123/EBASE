#' Plot results from EBASE 
#'
#' @param res output data frame from \code{\link{ebase}}
#' @param asnem logical indicating if \code{NEM} is plotted with \code{P} and negative \code{R}, see details
#' @param instantaneous logical indicating if results are instantaneous (default) or averaged to daily
#'
#' @return A \code{\link[ggplot2]{ggplot}}
#' 
#' @export
#'
#' @details The plot shows \code{P}, \code{R}, and \code{D} over time as positive values if \code{asnem = F} (default). Positive values for \code{D} are ingassing, negative outgassing.  
#' 
#' If \code{asnem = T}, \code{NEM} is plotted as a separate line as the difference between \code{P} and \code{R}. \code{R} is shown as negative values to indicate the negative influence on \code{NEM}. \code{D} is also excluded.
#' 
#' If \code{instantaneous = F}, the plot shows daily-averaged values.  The y-axis units are the same for daily or instantaneous values.  
#' 
#' @examples
#' # plot instantaneous
#' ebase_plot(exres)
#' 
#' # plot NEM, negative R, exclude D
#' ebase_plot(exres, asnem = TRUE)
#' 
#' # plot daily-averaged
#' ebase_plot(exres, instantaneous = FALSE)
ebase_plot <- function(res, asnem = FALSE, instantaneous = TRUE){
  
  # instantaneous results
  if(instantaneous){
    
    ylab <- expression(paste('Instantaneous (', O [2], ' mmol ', m^-2, ' ', d^-1, ')'))
    
    toplo <- res %>% 
      dplyr::select(DateTimeStamp, P, R, D, NEM) %>% 
      na.omit() %>% 
      tidyr::pivot_longer(cols = -matches('DateTimeStamp')) %>% 
      dplyr::rename(xval = DateTimeStamp)

  }  

  # daily averaged results
  if(!instantaneous){
    
    ylab <- expression(paste('Daily-averaged (', O [2], ' mmol ', m^-2, ' ', d^-1, ')'))
    
    toplo <- res %>% 
      dplyr::select(Date, P, R, D, NEM) %>% 
      tidyr::pivot_longer(cols = -matches('Date')) %>% 
      dplyr::group_by(Date, name) %>% 
      dplyr::summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% 
      dplyr::rename(xval = Date)
    
  }

  # prep as nem plot
  if(asnem){
    
    toplo <- toplo %>% 
      dplyr::filter(name != 'D') |> 
      dplyr::mutate(
        value = dplyr::case_when(
          name == 'R' ~ -value,
          T ~ value
        )
      )
    
    brks <- c('P', 'R', 'NEM')
    lbs <- c(expression(P), expression(-R), expression(NEM))
    
  }
  
  # prep as default plot
  if(!asnem){
    
    toplo <- toplo %>% 
      dplyr::filter(name != 'NEM')
    
    brks <- c('P', 'R', 'D')
    lbs <- c(expression(P), expression(R), expression(D))
  
  }
  
  # plot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = xval, y = value, color = name)) +
    ggplot2::geom_line() + 
    ggplot2::geom_point() + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::scale_color_discrete(
      breaks = brks,
      labels = lbs
    ) +
    ggplot2::labs(
      y = ylab, 
      color = NULL, 
      x = NULL
  )
  
  return(p)
  
}
