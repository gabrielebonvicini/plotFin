#' Compute and Plot a correlation diagram between stocks
#'
#' Compute and Plot a correlation diagram between selected sotcks, or
#' between stocks and a (single) market index. After computing the correlations
#' a statistical test shows only the significant correlations. The non-significant
#' ones are crossed
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
#' @param to A single character variable storing the ending date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param market_index A single character vector storing the ticker of the market index
#' you want to download. Default to NULL
#'
#' @examples
#' \dontrun{
#' x = c("AAPL", "AMZN", "F", "JPM", "DD", "IBM", "INTC")
#' start = "1998-01-01"
#' end = "2015-01-01"
#'
#'
#' stock.correlation(ticker = x, market_index = "^GSPC", from = start, to = end  ) #S&P500
#' stock.correlation(ticker = x, market_index = "^IXIC", from = start, to= end  ) #Nasdaq Index
#' stock.correlation(ticker = x, market_index = "FTSEMIB.MI", from = start, to = end ) #FTSE MIB Index
#' stock.correlation(ticker = x, from = start, to = end) #No market index
#' }
#'
#'
#'


stock.correlation <- function( ticker, from, to, market_index = NULL) {
if ( is.null(market_index) ) {
  #download data
data <- getSymbols(ticker, from = from, to = to,
                   auto.assign = T, warnings = F  ) %>%
  purrr::map(~Ad(get(.))) %>% reduce(merge)


#remove NA cells
data <- na.omit(data)
#appropraite name
colnames(data) <-  ticker
#significance test
mtest = cor.mtest(data)   #put the data here and not the correlations


#start plotting
if ( length(ticker) > 6 ) {
  corrplot( cor(data), method = "number", mar = c(1,1,1,1),
            number.font = 4, number.cex = 0.6,
            p.mat = mtest$p, sig.level = 0.05 , insig = "blank",
            col = COL1( sequential =  "YlOrRd" ) , tl.cex = 0.7 )
  print("Please expand the Area dedicated to Plots to better visualize the correlation plot")
  print("Non significant correlation are crossed (sign.level = 0.05)")

} else {
  corrplot( cor(data), method = "number", mar = c(1,1,1,1),
            cl.ratio = 0.25, tl.cex = 0.9, number.cex = 0.9,
            p.mat = mtest$p, sig.level = 0.05)
  print("Non significant correlation are crossed (sign.level = 0.05)")
}
 } else {
  data <- getSymbols(ticker, from = from, to = to,
                     auto.assign = T  ) %>%
    purrr::map(~Ad(get(.))) %>% reduce(merge)

  data2 <- getSymbols(market_index, from = from, to = to,
                      env = NULL )
  data2 <- data2[ ,6]


  #remove NA cells
  data <- merge(data, data2)
  data <- na.omit(data)
  #appropraite name
  colnames(data) <- c( ticker , market_index )
  #significance test
  mtest = cor.mtest(data)   #put the data here and not the correlations


  #start plotting
  if ( length(ticker) > 6 ) {
    corrplot( cor(data), method = "number", mar = c(1,1,1,1),
              number.font = 4, number.cex = 0.6,
              p.mat = mtest$p, sig.level = 0.05 ,
              col = COL1( sequential =  "YlOrRd" ) , tl.cex = 0.7 )
    print("Please expand the Area dedicated to Plots to better visualize the correlation plot")
    print("Non significant correlation are crossed (sign.level = 0.05)")

  } else {
    corrplot( cor(data), method = "number", mar = c(1,1,1,1),
              cl.ratio = 0.25, tl.cex = 0.9, number.cex = 0.9,
              p.mat = mtest$p, sig.level = 0.05)
    print("Non significant correlation are crossed (sign.level = 0.05)")
  }

 }
}


