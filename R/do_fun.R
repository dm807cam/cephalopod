#' Place a new order
#' 
#' Note: Check get_asset_pairs() for details on the available trading pairs, their price and quantity precisions, order minimums, and available leverage. 
#' For more information refer to the Kraken Docs: https://docs.kraken.com/rest/#operation/addOrder
#'
#' @param pair Asset pair `id` or `altname`.
#' @param type Order direction `buy` or `sell`.
#' @param ordertype Order type e.g., `market`, `limit` ,`stop-loss`, `take-profit`, `stop-loss-limit`, `take-profit-limit`, `settle-position`.
#' @param volume Order quantity in terms of the base asset.
#' @param price Limit price for `limit` orders or trigger price for `stop-loss`, `stop-loss-limit`, `take-profit`, and `take-profit-limit` orders.
#' @param leverage Amount of leverage desired (default = "none").
#' @param timeinforce Time-in-force of the order to specify how long it should remain in the order book before being cancelled (default = "GTC"). GTC (Good-'til-cancelled) is default if the parameter is omitted. IOC (immediate-or-cancel) will immediately execute the amount possible and cancel any remaining balance rather than resting in the book. GTD (good-'til-date), if specified, must coincide with a desired `expiretm`.
#' @param oflags Options: `immediate` or `post`, this prevents placing a limit buy order that instantly matches against the sell side of the order book (and vice versa for sell orders) which would result in taker fees. The order will either get posted to the order book or be cancelled, ensuring a maker fee when executed.
#' @param validate Validate inputs only. Do not submit order (default = FALSE).
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
add_order <- function(pair, type, ordertype, volume, price = NULL, leverage = "none", timeinforce = "GTC", oflags = "immediate", validate=FALSE) {

  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/AddOrder"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  
  if(is.null(price)) {
  post <- base::paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype, "&volume=", volume, 
                       "&leverage=", leverage, "&timeinforce=", timeinforce, "&validate=", validate)
  } else {
  post <- base::paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype, "&volume=", volume,
                       "&price=", price, "&leverage=", leverage, "&timeinforce=", timeinforce, "&validate=", validate)
  }
  
  if(oflags == "post") {
    post <- paste0(post, "&oflags=", oflags)
  }
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    out <- out$result
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Cancel open order 
#' 
#' Cancel a particular open order (or set of open orders) by `txid`.
#'
#' @param txid Open order transaction ID (txid). A list of open order can be obtained with `get_open_orders()`.
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
cancel_order <- function(txid) { 
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/CancelOrder"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&txid=", txid)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    out <- out$result
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Cancel all open orders 
#' 
#' Cancel a particular open order (or set of open orders) by `txid`.
#' A list of open order can be obtained with `get_open_orders()`.
#'
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
cancel_all_orders <- function() { 
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/CancelAll"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    warning(base::paste0("I canceled: ",out$result[[1]]," orders for you."))
    out <- out$result[[1]]
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }  
  
  return(out)
}

#' Cancel all open orders after X
#' 
#' This function provides a "Dead Man's Switch" mechanism to protect the client from network malfunction, extreme latency or unexpected matching engine downtime. The client can send a request with a timeout (in seconds), that will start a countdown timer which will cancel all client orders when the timer expires. The client has to keep sending new requests to push back the trigger time, or deactivate the mechanism by specifying a timeout of 0. If the timer expires, all orders are cancelled and then the timer remains disabled until the client provides a new (non-zero) timeout.
#'
#' @param timeout Timeout in seconds
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
cancel_all_orders_afterX <- function(timeout) {
  
  # Check server status
  check_sysstatus()
  
  url <- "https://api.kraken.com/0/private/CancelAllOrdersAfter"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&timeout=", timeout)
  
  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)
  
  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))
  
  if(length(out$error) == 0) {
    warning(base::paste0("I will cancel all orders for you on: ",out$result[[1]],""))
  } else {
    warning(base::paste0(error_msg,out$error[[1]]))
    out <- out$error
  }
  
  return(out)
}
