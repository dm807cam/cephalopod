#' Get Kraken system status
#' 
#' Get the current system status or trading mode.
#' 
#' @return System status (string)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_sysstatus()
#' @export
get_sysstatus <- function() {
  url <- "https://api.kraken.com/0/public/SystemStatus"
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- out$result$status
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get Kraken server time
#' 
#' Get the server's time.
#' 
#' @return Server time (datetime)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_systime()
#' @export
get_systime <- function() {
  url <- "https://api.kraken.com/0/public/Time"
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- as.POSIXct(out$result$unixtime, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get asset pairs
#' 
#' Get tradeable asset pairs.
#'
#' @return Asset pairs (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_asset_pairs()
#' @export
get_asset_pairs <- function() {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/public/AssetPairs"
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- data.frame(do.call("rbind",out$result))
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get spread
#' 
#' Get recent spread.
#'
#' @param pair Asset pair `id` or `altname`.
#' @return Recent spread (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_spread("XBTUSD")
#' @export
get_spread <- function(pair) {
  
  # Check server status
  check_sysstatus()
  
  url <- base::paste0("https://api.kraken.com/0/public/Spread?pair=",pair)
  tmp <- jsonlite::fromJSON(url)
  if(length(tmp$error) == 0) {
    out <- data.frame(tmp$result[[1]])
    colnames(out) <- c("time", "bid", "ask")
    out[] <- lapply(out, as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,tmp$error[[1]]))
    out <- tmp$error
  }  
  return(out)
}

#' Get recent trades
#' 
#' Returns the last 1000 trades.
#'
#' @param pair Asset pair `id` or `altname`.
#' @return Recent trades (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_trades("XBTUSD")
#' @export
get_trades <- function(pair) {
  
  # Check server status
  check_sysstatus()
  
  url <- base::paste0("https://api.kraken.com/0/public/Trades?pair=",pair)
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- data.frame(out$result[[1]])
    colnames(out) <- c("price", "volume", "time", "buy/sell", "market/limit", "misc")
    out[] <- lapply(out, as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get OHLC data
#' 
#' Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of `since`.
#'
#' @param pair Asset pair `id` or `altname`.
#' @param since Return up to 720 OHLC data points since given timestamp.
#' @param interval Time intervals in minutes.
#' @return OHLC data (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_ohlc("XBTUSD", "2022-01-01", 1440)
#' @export
get_ohlc <- function(pair, since, interval) {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/public/OHLC?"
  parameters <- base::paste0("pair=", pair, "&since=", since, "&interval=", interval)
  out <- jsonlite::fromJSON(base::paste0(url, parameters))
  if(length(out$error) == 0) {
    out <- data.frame(out$result[[1]])
    colnames(out) <- c("time", "open", "high", "low", "close", "vwap", "volume", "count")
    out[] <- lapply(out, as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get all OHLC data
#' 
#' Get OHLC data for all available asset pairs
#' Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of `since`.
#'
#' @param since Return up to 720 OHLC data points since given timestamp.
#' @param interval Time intervals in minutes.
#' @param qfilter Filter to select a quote pair (Options: `ZEUR`, `ZUSD`, `ZAUD`, `XETH`, `XXBT`, `USDT`, `ZJPY`, `CHF`, `DAI`, `USDC`, `ZCAD`, `DOT`).
#' @param timeout Timeout in seconds between API requests.
#' @return OHLC data (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_ohlc("XBTUSD", "2022-01-01", 1440)
#' @export
get_all_ohlc <- function(since, interval, qfilter=NULL, timeout) {
  
  # Get asset pairs
  asset_pairs <- get_asset_pairs()
  if(!is.null(qfilter)) {
    asset_pairs <- asset_pairs[asset_pairs$quote == qfilter,]
  }
  
  # Download daily OHLC for all asset_pairs
  datalist <- list()
  pairs <- unique(asset_pairs$altname)
  for(ii in 1:length(pairs)) {
    tmp <- get_ohlc(pairs[[ii]],since,interval)
    tmp <- cbind(altname=pairs[[ii]], tmp)
    datalist[[ii]] <- tmp
    Sys.sleep(timeout)
  }
  out <- data.frame(do.call("rbind", datalist))
  
  return(out)
}

#' Get bids
#' 
#' Returns dataframe of bids in order book for an asset pair.
#'
#' @param pair Asset pair `id` or `altname`.
#' @return Bids (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_bids("XBTUSD")
#' @export
get_bids <- function(pair) {
  
  # Check server status
  check_sysstatus()
  
  url <- base::paste0("https://api.kraken.com/0/public/Depth?pair=",pair)
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- data.frame(out$result[[1]]$bids)
    colnames(out) <- c("price", "volume", "time")
    out[] <- lapply(out, as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get asks
#' 
#' Returns dataframe of asks in order book for an asset pair.
#'
#' @param pair Asset pair `id` or `altname`.
#' @return Asks (dataframe)
#' @importFrom jsonlite fromJSON
#' @examples
#' get_bids("XBTUSD")
#' @export
get_asks <- function(pair) {
  
  # Check server status
  check_sysstatus()
  
  url <- base::paste0("https://api.kraken.com/0/public/Depth?pair=",pair)
  out <- jsonlite::fromJSON(url)
  if(length(out$error) == 0) {
    out <- data.frame(out$result[[1]]$asks)
    colnames(out) <- c("price", "volume", "time")
    out[] <- lapply(out, as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  return(out)
}

#' Get balance
#' 
#' Retrieve a summary of collateral balances, margin position valuations, equity and margin level.
#'
#' @return Balance (dataframe)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_available_balance <- function() {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/Balance"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    out <- data.frame(do.call("rbind", out$result))
    colnames(out) <- c("balance")
    out[] <- lapply(out, as.numeric)
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Get trade balance
#' 
#' Retrieve a summary of collateral balances, margin position valuations, equity and margin level.
#'
#' @return Balance (dataframe)
#' @param asset Asset.
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_trade_balance <- function(asset) {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/TradeBalance"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&asset=", asset)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    out <- data.frame(do.call("rbind", out$result))
    colnames(out) <- c("balance")
    out[] <- lapply(out, as.numeric)
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Get open orders
#' 
#' Retrieve information about currently open orders.
#'
#' @return Open orders (dataframe)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_open_orders <- function() {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/OpenOrders"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)

  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    if(length(out$result$open) == 0) {
      warning("I could not find any open orders.", call. = F)
      out <- out$result$open
    } else {  
      out <- out$result$open
    }
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Get closed orders
#' 
#' Retrieve information about all closed orders (fullfilled and cancelled).
#'
#' @return Closed orders (dataframe)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_closed_orders <- function() {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/ClosedOrders"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    if(length(out$result$closed) == 0) {
      warning("I could not find any open orders.", call. = F)
      out <- out$result$closed
    } else {  
      out <- out$result$closed
    }
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Query Orders Info
#' 
#' Retrieve information about an order (fulfilled and cancelled).
#'
#' @param txid Order ID.
#' @return Order Info (dataframe)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_order_info <- function(txid) {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/QueryOrders"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&txid=", txid)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    if(length(out$result) == 0) {
      warning(base::paste0("I could not find order with txid: ", txid), call. = F)
      out <- out$result[[txid]]
    } else {  
      out <- out$result[[txid]]
    }
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}


#' Get trade history
#' 
#' Retrieve information about trades/fills. 50 results are returned at a time, the most recent by default.
#'
#' @param type Type of trade (default = "all). Options: `all`, `any position`, `closed position`, `closing position`, `no position`.
#' @param trades Whether or not to include trades related to position in output (default = FALSE).
#' @param start Optional: Starting unix timestamp or trade txid of results.
#' @param end Optional: Ending unix timestamp or trade txid of results.
#' @return Trade history (dataframe)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
get_trade_history <- function(type="all", trades=FALSE, start=NULL, end=NULL) { 
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/TradesHistory"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  
  if(!is.null(start) & !is.null(end)) {
    post <- base::paste0("nonce=", nonce, "&type=", type, "&trades=", trades, "&start=", start, "&end=", end)
  }
  
  if(!is.null(start) & is.null(end)) {
    post <- base::paste0("nonce=", nonce, "&type=", type, "&trades=", trades, "&start=", start)
  }
  
  if(is.null(start) & !is.null(end)) {
    post <- base::paste0("nonce=", nonce, "&type=", type, "&trades=", trades, "&end=", end)
  }
  
  if(is.null(start) & is.null(end)) {
    post <- base::paste0("nonce=", nonce, "&type=", type, "&trades=", trades)
  }

  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)

  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    out <- data.frame(do.call("rbind", out$result$trades))
    out[,c(4,7:11)] <- lapply(out[,c(4,7:11)], as.numeric)
    out$time <- as.POSIXct(out$time, origin="1970-01-01")
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}
