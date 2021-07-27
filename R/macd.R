#' calculates the moving average convergence/divergence (MACD) of a numeric vector
#'
#' @param FAST numeric: period of days for the fast series
#' @param SLOW numeric: period of days for the slow series
#' @param SIGNAL numeric: period of days for the signal
#' @param SERIES numeric vector of daily prices
#'
#' @return numeric

macd <- function(FAST = 12, SLOW = 26, SIGNAL = 9, SERIES){
  ema_fast <- ema(FAST, SERIES)
  ema_slow <- ema(SLOW, SERIES)
  MACD <- ema_slow - ema_fast
  MACD_signal <- ema(SIGNAL, MACD)

  total <- length(SERIES)

  buffer_NAs <- function(TOT, SER){c(rep(NA, TOT - length(SER)), SER)}

  MACD <- buffer_NAs(total, MACD)
  MACD_signal <- buffer_NAs(total, MACD_signal)

  output <- as.data.frame(cbind(MACD, MACD_signal))
  colnames(output) <- c("macd", "macd_signal")
  return(output)

}
