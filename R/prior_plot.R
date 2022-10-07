#' Plot prior distributions for a, Rt_vol, and b
#'
#' @param aprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{a} parameter, see details
#' @param rprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{r} parameter, see details
#' @param bprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{b} parameter, see details
#' @param n numeric indicating number of random samples to draw from prior distributions
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details This function produces a plot of the prior distributions that are used in \code{\link{ebase}} for the \emph{a}, \emph{r}, and \emph{b} parameters for the optimization equation for estimating metabolism.  The \code{\link{ebase}} function uses the same default values for the arguments for \code{aprior}, \code{rprior}, and \code{bprior} as required for this function.  If the default values are changed for \code{\link{ebase}}, this function can be used to assess how changing characteristics of the prior distributions could influence the resulting parameter estimates and their posterior distributions (e.g., as shown with \code{\link{credible_plot}}.
#' 
#' All parameters follow a normal Gaussian distribution for the prios with the means and standard deviations defined by the arguments. All distributions are truncated to include only values greater than zero as required by the core metabolism equation. Truncated normal distributions are obtained using the \code{\link[truncnorm]{rtruncnorm}} function with the number of random samples defined by the \code{n} argument. 
#' 
#' The x-axis label for the \emph{r} parameter is shown using the volumetric notation for respiration for consistency with \code{\link{credible_plot}}. 
#' 
#' @examples
#' # default plot
#' prior_plot()
#' 
#' # changing the mean and standard deviation for the b parameter
#' prior_plot(bprior = c(0.4, 1))
prior_plot <- function(aprior = c(0.2, 0.96), rprior = c(20, 10), bprior = c(0.251, 0.04), n = 1000){
  
  labs <- c('a~(mmol~m^{-3}~d^{-1})(W~m^{-2})', 
            'Rt[vol]~(mmol~m^{-3}~d^{-1})',
            'b~(cm~hr^{-1})(m^{2}~s^{-2})'
  )
  
  aprior <- data.frame(var = 'aprior', mean = aprior[1], sd = aprior[2])
  rprior <- data.frame(var = 'rprior', mean = rprior[1], sd = rprior[2])
  bprior <- data.frame(var = 'bprior', mean = bprior[1], sd = bprior[2])
  
  toplo <- rbind(aprior, rprior, bprior) %>% 
    dplyr::group_by(var) %>% 
    dplyr::mutate(
      val = list(truncnorm::rtruncnorm(n, a = 0, mean = mean, sd = sd)), 
      var = factor(var, 
                     levels = c('aprior', 'rprior', 'bprior'), 
                     labels = labs
                     )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(var, val) %>% 
    tidyr::unnest('val')
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = val)) + 
    ggplot2::geom_density(fill = 'grey', adjust = 2) +
    ggplot2::facet_wrap(~var, scales = 'free', labeller = ggplot2::label_parsed, strip.position = 'bottom') +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      strip.placement = 'outside', 
      strip.text = ggplot2::element_text(size = ggplot2::rel(1))
    ) + 
    ggplot2::labs(
      y = 'Density', 
      x = NULL
    )
  
  return(p)

}
