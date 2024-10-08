#' Example results for four days from Apalachicola NERRS
#'
#' Example results for four days from Apalachicola NERRS
#'
#'@format A data frame with 384 observations and 29 variables:
#' \describe{
#'   \item{DateTimeStamp}{POSIXct, format: "2012-06-01 00:00:00" "2012-06-01 00:15:00" ...}
#'   \item{Date}{Date, format: "2012-06-01" "2012-06-01" ...}
#'   \item{grp}{Numeric, grouping variable defined by \code{ndays} in \code{\link{ebase}}}
#'   \item{Z}{Numeric, depth in meters}
#'   \item{DO_obs}{Numeric, observed dissolved oxygen, mmol m-3}
#'   \item{DO_mod}{Numeric, modelled dissolved oxygen, mmol m-3}
#'   \item{DO_modlo}{Numeric, lower credible interval of modelled dissolved oxygen}
#'   \item{DO_modhi}{Numeric, upper credible interval of modelled dissolved oxygen}
#'   \item{dDO}{Numeric, change in dissolved oxygen, mmol m-3 d-1}
#'   \item{converge}{Character, convergence status}
#'   \item{rsq}{Numeric, R-squared value}
#'   \item{a}{Numeric, parameter a (mmol m-2 d-1)/(W m-2)}
#'   \item{alo}{Numeric, lower credible interval of parameter a}
#'   \item{ahi}{Numeric, upper credible interval of parameter a}
#'   \item{b}{Numeric, parameter b, (cm hr-1)/(m2 s-2)}
#'   \item{blo}{Numeric, lower credible interval of parameter b}
#'   \item{bhi}{Numeric, upper credible interval of parameter b}
#'   \item{P}{Numeric, production, O2 mmol m-2 d-1}
#'   \item{Plo}{Numeric, lower credible interval of parameter P}
#'   \item{Phi}{Numeric, upper credible interval of parameter P}
#'   \item{R}{Numeric, respiration, O2 mmol m-2 d-1}
#'   \item{Rlo}{Numeric, lower credible interval of parameter R}
#'   \item{Rhi}{Numeric, upper credible interval of parameter R}
#'   \item{NEM}{Numeric, net ecosystem metabolism, O2 mmol m-2 d-1}
#'   \item{NEMlo}{Numeric, lower credible interval of parameter NEM}
#'   \item{NEMhi}{Numeric, upper credible interval of parameter NEM}
#'   \item{D}{Numeric, gas exchange, O2 mmol m-2 d-1)}
#'   \item{Dlo}{Numeric, lower credible interval of parameter D}
#'   \item{Dhi}{Numeric, upper credible interval of parameter D}
#' }
#' @family utilities
#' 
#' @examples 
#' head(exres)
"exres"