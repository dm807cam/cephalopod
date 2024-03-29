% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fun.R
\name{get_ohlc}
\alias{get_ohlc}
\title{Get OHLC data}
\usage{
get_ohlc(pair, since, interval)
}
\arguments{
\item{pair}{Asset pair \code{id} or \code{altname}.}

\item{since}{Return up to 720 OHLC data points since given timestamp.}

\item{interval}{Time intervals in minutes.}
}
\value{
OHLC data (dataframe)
}
\description{
Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of \code{since}.
}
\examples{
get_ohlc("XBTUSD", "2022-01-01", 1440)
}
