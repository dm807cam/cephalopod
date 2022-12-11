#' Withdraw funds
#'
#' Withdraw funds to a reference address.
#'
#' @param key Withdrawal key name, as set up on your account.
#' @param amount Amount to be withdrawn.
#' @param asset Asset being withdrawn.
#' @return Confirmation message (string)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
withdraw_funds <- function(asset, key, amount) {
  # Check server status
  check_sysstatus()

  url <- "https://api.kraken.com/0/private/Withdraw"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&asset=", asset, "&key=", key, "&amount=", amount)

  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)

  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))

  if (length(out$error) == 0) {
    out <- out$result
  } else {
    warning(base::paste0(error_msg, out$error[[1]]))
    out <- out$error
  }

  return(out)
}

#' Withdraw status
#'
#' Retrieve information about recently requests withdrawals.
#'
#' @param asset Asset being withdrawn.
#' @return Confirmation message (string)
#' @importFrom RCurl base64Decode
#' @importFrom digest digest
#' @importFrom digest hmac
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @export
withdraw_status <- function(asset) {
  # Check server status
  check_sysstatus()

  url <- "https://api.kraken.com/0/private/WithdrawStatus"
  method_path <- base::gsub("^.*?kraken.com", "", url)
  nonce <- base::as.character(base::as.numeric(base::Sys.time()) * 1000000)
  post <- base::paste0("nonce=", nonce, "&asset=", asset)

  secret <- RCurl::base64Decode(private_key, mode = "raw")
  sha256 <- digest::digest(object = base::paste0(nonce, post), algo = "sha256", serialize = F, raw = T)
  hmac <- digest::hmac(key = secret, object = c(base::charToRaw(method_path), sha256), algo = "sha512", raw = T)

  out <- httr::content(httr::POST(url, body = post, httr::add_headers(c("API-Key" = public_key, "API-Sign" = RCurl::base64Encode(hmac)))))

  if (length(out$error) == 0) {
    out <- out$result
  } else {
    warning(base::paste0(error_msg, out$error[[1]]))
    out <- out$error
  }

  return(out)
}
