#' Plot prior distributions for a, R, and b
#'
#' @param aprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{a} parameter, see details
#' @param rprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{R} parameter, see details
#' @param bprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{b} parameter, see details
#' @param bmax numeric value for the upper limit on the prior distribution for \code{bprior}, set as twice the default value of the mean
#' @param n numeric indicating number of random samples to draw from prior distributions
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @details This function produces a plot of the prior distributions that are used in \code{\link{ebase}} for the \emph{a}, \emph{R}, and \emph{b} parameters for the optimization equation for estimating metabolism.  The \code{\link{ebase}} function uses the same default values for the arguments for \code{aprior}, \code{rprior}, and \code{bprior} as required for this function.  If the default values are changed for \code{\link{ebase}}, this function can be used to assess how changing characteristics of the prior distributions could influence the resulting parameter estimates and their posterior distributions (e.g., as shown with \code{\link{credible_plot}}.
#' 
#' All parameters follow a normal Gaussian distribution for the priors with the means and standard deviations defined by the arguments. All distributions are truncated to include only values greater than zero as required by the core metabolism equation. The upper limit for \emph{b} is also set as twice the default value of the mean in the \code{bprior} argument. Truncated normal distributions are obtained using the \code{\link[truncnorm]{rtruncnorm}} function with the number of random samples defined by the \code{n} argument. 
#' 
#' The density curves for each parameter are normalized such that the peak values are always equal to 1. 
#' 
#' @examples
#' # default plot
#' prior_plot()
#' 
#' # changing the mean and standard deviation for the b parameter
#' prior_plot(bprior = c(0.2, 0.05))
prior_plot <- function(aprior = c(4, 2), rprior = c(300, 150), bprior = c(0.251, 0.125), bmax = 0.502, n = 1000){
  
  labs <- c('italic(a)~(mmol~m^{-2}~d^{-1})/(W~m^{-2})', 
            'italic(R)~(mmol~m^{-2}~d^{-1})',
            'italic(b)~(cm~hr^{-1})/(m^{2}~s^{-2})'
  )

  aprior <- data.frame(var = 'aprior', mean = aprior[1], sd = aprior[2])
  rprior <- data.frame(var = 'rprior', mean = rprior[1], sd = rprior[2])
  bprior <- data.frame(var = 'bprior', mean = bprior[1], sd = bprior[2])
  
  toplo <- rbind(aprior, rprior, bprior) %>% 
    dplyr::mutate(
      maxv = dplyr::case_when(
        var == 'aprior' ~ Inf, 
        var == 'rprior' ~ Inf, 
        var == 'bprior' ~ bmax
      )
    ) %>% 
    dplyr::group_by(var) %>% 
    dplyr::mutate(
      val = list(truncnorm::rtruncnorm(n, a = 0, b = maxv, mean = mean, sd = sd)), 
      var = factor(var, 
                     levels = c('aprior', 'rprior', 'bprior'), 
                     labels = labs
                     )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(var, val) %>% 
    tidyr::unnest('val')

  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = val)) + 
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)), fill = 'grey', adjust = 2) +
    ggplot2::facet_wrap(~var, scales = 'free', labeller = ggplot2::label_parsed, strip.position = 'bottom') +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      strip.placement = 'outside', 
      strip.text = ggplot2::element_text(size = ggplot2::rel(1))
    ) + 
    ggplot2::labs(
      y = 'Normalized density', 
      x = NULL
    )
  
  return(p)

}
