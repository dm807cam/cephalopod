# Define global variables to avoid package check to fail
utils::globalVariables(c("public_key", "private_key"))

# Default error message
error_msg <- "Arghh, I encountered an error: "

# Check server status
check_sysstatus <- function() {
  if (cephalopod::get_sysstatus() != "online") {
    stop(base::paste0(error_msg, "Can't connect to Kraken server."))
  }
}
