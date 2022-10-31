#' Format ebase output
#'
#' @param out data.frame for model output
#' @param dat data.frame as returned by \code{\link{ebase_prep}}
#' @param interval timestep interval in seconds
#' @param maxinterp numeric value for minimum number of continuous observations that must not be interpolated within a group defined by \code{ndays} to assign as \code{NA} in output
#'
#' @details This function is used internally with \code{\link{ebase}} and should not be called by itself. 
#' 
#' @return Formatted output for \code{\link{ebase}} with interpolated rows as \code{NA} (except \code{Date} and \code{DateTimeStamp} as defined by \code{maxinterp}
#' @export
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
#' ##
#' # run ebase with defaults, parallel
#' 
#' \dontrun{
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' out <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2)
#' 
#' stopCluster(cl)
#'
#' dat <- ebase_prep(dat, H = 1.85, interval = 900, ndays = 1)
#' 
#' ebase_form(out, dat, interval = 900, maxinterp = 48) 
#' }
ebase_form <- function(out, dat, interval, maxinterp = 43200 / interval){
  
  # remove groups with more interpolated values defined by maxinterp
  idfun <- function(x){
    
    idv <- rle(x)
    out <- rep(seq_along(idv$lengths), idv$lengths)
    return(out)
    
  }

  out <- dat %>% 
    dplyr::select(DateTimeStamp, isinterp) %>% 
    dplyr::left_join(out, ., by = 'DateTimeStamp') %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(
      ids = idfun(isinterp)
    ) %>% 
    dplyr::group_by(grp, ids) %>% 
    dplyr::mutate(
      cnt = dplyr::n(),
      cnt = ifelse(ids == 1, cnt + 1, cnt)
    ) %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(
      cnt = ifelse(isinterp, cnt, 0),
      maxv = max(cnt)
    ) %>% 
    dplyr::filter(maxv < maxinterp) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-isinterp, -ids, -cnt, -maxv)
  
  # create NAs for removed values
  out <- data.frame(
      DateTimeStamp = seq(min(out$DateTimeStamp), max(out$DateTimeStamp), by = interval)
    ) %>% 
    dplyr::left_join(., out, by = 'DateTimeStamp') %>% 
    dplyr::mutate(Date = as.Date(DateTimeStamp, tz = attr(DateTimeStamp, 'tzone')))
  
  return(out)
  
}