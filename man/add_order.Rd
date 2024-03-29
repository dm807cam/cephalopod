% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_fun.R
\name{add_order}
\alias{add_order}
\title{Place a new order}
\usage{
add_order(
  pair,
  type,
  ordertype,
  volume,
  price = NULL,
  leverage = "none",
  timeinforce = "GTC",
  oflags = "immediate",
  validate = FALSE
)
}
\arguments{
\item{pair}{Asset pair \code{id} or \code{altname}.}

\item{type}{Order direction \code{buy} or \code{sell}.}

\item{ordertype}{Order type e.g., \code{market}, \code{limit} ,\code{stop-loss}, \code{take-profit}, \code{stop-loss-limit}, \code{take-profit-limit}, \code{settle-position}.}

\item{volume}{Order quantity in terms of the base asset.}

\item{price}{Limit price for \code{limit} orders or trigger price for \code{stop-loss}, \code{stop-loss-limit}, \code{take-profit}, and \code{take-profit-limit} orders.}

\item{leverage}{Amount of leverage desired (default = "none").}

\item{timeinforce}{Time-in-force of the order to specify how long it should remain in the order book before being cancelled (default = "GTC"). GTC (Good-'til-cancelled) is default if the parameter is omitted. IOC (immediate-or-cancel) will immediately execute the amount possible and cancel any remaining balance rather than resting in the book. GTD (good-'til-date), if specified, must coincide with a desired \code{expiretm}.}

\item{oflags}{Options: \code{immediate} or \code{post}, this prevents placing a limit buy order that instantly matches against the sell side of the order book (and vice versa for sell orders) which would result in taker fees. The order will either get posted to the order book or be cancelled, ensuring a maker fee when executed.}

\item{validate}{Validate inputs only. Do not submit order (default = FALSE).}
}
\value{
Order confirmation (string)
}
\description{
Note: Check get_asset_pairs() for details on the available trading pairs, their price and quantity precisions, order minimums, and available leverage.
For more information refer to the Kraken Docs: https://docs.kraken.com/rest/#operation/addOrder
}
