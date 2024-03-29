% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebase_form.R
\name{ebase_form}
\alias{ebase_form}
\title{Format ebase output}
\usage{
ebase_form(out, dat, interval, maxinterp = 43200/interval)
}
\arguments{
\item{out}{data.frame for model output}

\item{dat}{data.frame as returned by \code{\link{ebase_prep}}}

\item{interval}{timestep interval in seconds}

\item{maxinterp}{numeric value for minimum number of continuous observations that must not be interpolated within a group defined by \code{ndays} to assign as \code{NA} in output}
}
\value{
Formatted output for \code{\link{ebase}} with interpolated rows as \code{NA} (except \code{Date} and \code{DateTimeStamp} as defined by \code{maxinterp}
}
\description{
Format ebase output
}
\details{
This function is used internally with \code{\link{ebase}} and should not be called by itself.
}
\examples{
library(dplyr)

# get four days of data
dat <- exdat \%>\% 
  filter(lubridate::month(DateTimeStamp) == 6) \%>\% 
  filter(lubridate::day(DateTimeStamp) \%in\% 1:4)
dat <- ebase_prep(dat, Z = 1.85, interval = 900, ndays = 1)

ebase_form(exres, dat, interval = 900, maxinterp = 48) 
}
