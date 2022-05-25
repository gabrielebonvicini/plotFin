#' Interactive Aroon oscillator
#'
#' This function is build to analyze the Aroon oscillator of a stock in a chsosen period
#' of time. It gives:
#' 1) The candlestick graph of prices of the stock (open,high,low and close).
#' 2) The interactive graphs of Aroon down, Aroon up, and the Aroon oscillator (difference between Aroon up and down).
#' 3) It creates an HTML table that groups together open prices, closing prices, adjusted prices, high price, low price, daily volume, aaron up, aaron down and volume.
#'
#'
#' @export
#'
#' @param ticker A single character variable storing the ticker of the stock to be downloaded.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#'
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date up to the day in which we want
#' to download the stock data. The format should be "yyyy-mm-dd"
#'
#' @examples
#' \dontrun{
#' interactive.aaron("AAPL",from= "2020-01-01", to= "2021-01-01")
#' }



interactive.aaron<- function(ticker, from, to){
  RET1 <- getSymbols(ticker, from = from, to = to,
                     auto.assign = F)
  df <- data.frame(Date=index(RET1),coredata(RET1))

  A <- df[,2]
  d<- df[,5]
  B <- df[,3]
  c<- df[,4]

  fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                        open = ~A, close = d,
                        high = ~B, low = ~c)
  fig <- fig %>% plotly::layout(title = "Prices")


  RET1 <- RET1[,4]
  AAR <- aroon(RET1, n = 20)
  UP <- AAR [,1]
  DN <- AAR [,2]
  OSC <- AAR[,3]


  p1 <- dygraph(UP, main="Aroon UP") %>%
    dyOptions(labelsUTC = TRUE, fillGraph=FALSE, fillAlpha=0.1, drawGrid = FALSE, colors="midnightblue") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)

  p2 <- dygraph(DN, main = "Aroon DOWN") %>%
    dyOptions(labelsUTC = TRUE, fillGraph=FALSE, fillAlpha=0.1, drawGrid = FALSE, colors="firebrick") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)

  p3 <- dygraph(OSC, main= "Aroon OSCILLATOR") %>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="slateblue3") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)



  df<- df[,-1]
  gg <- cbind(df, UP, DN, OSC)
  Aroon_Data <- as.data.frame(gg)
  assign("Aroon_data", Aroon_Data, envir = .GlobalEnv)


  tab <- tab_df(Aroon_Data, title = "Aroon Oscillator table", footnote = NULL, col.header = NULL,
                show.type = FALSE, show.rownames = TRUE, show.footnote = FALSE,
                alternate.rows = TRUE, sort.column = NULL, encoding = "UTF-8",
                arc = 'color:blue;', file = NULL, use.viewer = FALSE)

  print(p2)
  print(p1)
  print(p3)
  print(fig)
  print(tab)
}



