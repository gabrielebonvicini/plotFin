#' Interactive historical daily volatility
#'
#' Create an interacrtive graph of the historical daily volatility for the select
#' stocks based on log-returns.
#'
#'
#' @export
#'
#' @param ticker A character vector storing the tickers of the stocks of which you want the
#' correlation.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to  A single character variable storing the ending date up to the day in which we want
#' to download the stock data. The format should be "yyyy-mm-dd"
#'
#' @examples
#' \dontrun{
#' hist.vol.interact(c("AAPL","TSLA","AMZN"), from= "2020-01-01", to= "2022-01-01")
#' }
#'



hist.vol.interact<- function(ticker, from, to){

  # Download Adj. prices for the period chosen
  RET1 <- getSymbols(ticker, from=from , to=to,
                     auto.assign = T ) %>%
    purrr::map(~Ad(get(.))) %>% reduce(merge)

  #Calculate daily log-returns
  log_ret <-diff(log(RET1))
  log_ret <- log_ret[-c(1)]
  log_ret<- na.omit(log_ret)

  # Calulate daily volatility
  dailyVol <- rollapply(log_ret, width = 22, FUN = "sd")
  dailyVol <- na.omit(dailyVol)
  colnames(dailyVol) <- c( paste0(ticker) )

  # Plot the interactive graph of historical daily volatility
  dailyVol <- dygraph(dailyVol, main= "Historical daily volatility") %>%
    dyOptions(labelsUTC = TRUE, fillGraph=FALSE, fillAlpha=0.5, drawGrid = TRUE,
              colors="navy") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 6, highlightSeriesBackgroundAlpha = 0.4,
                hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)
  dailyVol

}



