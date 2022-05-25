#' Z-score comparison 
#' 
#' A function  that makes comparison between the z-value of two stocks, 
#' in a monthly time frame. The value of the z-score shows how many standard deviations 
#' there are away from the mean. It has been calculated using the difference between 
#' the return of each ticket and its mean, divided by its standard deviation.
#' 
#' @export
#' 
#' @param ticker A character vector of length two storing  tickers of the stocks of 
#' which you want the z-score
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#' 
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#' 
#' @param to A single character variable storing the ending date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#' 
#' @examples 
#' \dontrun{ 
#' z.value(c("AAPL",  "MSFT"), from = "2020-01-01", to = "2021-06-06")
#' }
#' 

#create a function that gives gives a comparison between the z-value of two different stocks, putting in a ticker-list, from/to a defined year 

z.value <- function(ticker, from, to){
  
  #define ticker_1 and ticker_2 from ticker_list
  ticker_1 <-ticker[1]
  ticker_2 <- ticker[2]
  
  #import data for the first stock
  daily_stock_prices1 <-getSymbols( ticker_1, from = from, to= to
                                    , auto.assign = T ) %>%
    purrr:: map(~Ad(get(.))) %>% reduce(merge)
  
  
  monthly_stock_returns_1 <- periodReturn(daily_stock_prices1, period = 'monthly', from=from, to=to,
                                         type = 'log')
  
  
  #import data for the second stock
  daily_stock_prices2 <- getSymbols( ticker_2, from = from, to= to
                                     , auto.assign = T ) %>%
    purrr:: map(~Ad(get(.))) %>% reduce(merge)
  
  
  monthly_stock_returns_2<- periodReturn(daily_stock_prices2, period = 'monthly', from=from, to=to,
                                         type = 'log')
  
  #compute portfolio return
  return_ticker_1 <-monthly_stock_returns_1
  return_ticker_2 <- monthly_stock_returns_2
  
  #compute mean 
  M1 <-mean(return_ticker_1)
  M2<-mean(return_ticker_2)
  
  #compute standard deviation
  S1<-sd(return_ticker_1)
  S2<-sd(return_ticker_2)
  
  #compute z-value
  Z1<-(return_ticker_1- M1)/S1
  Z2 <-(return_ticker_2- M2)/S2
  Ztotal<-cbind(Z1,Z2)
  
  
  dygraphs::dygraph(Ztotal, main = "Z-score" , xlab = "Date", 
                    ylab = "z-value" ) %>% 
    dyOptions( colors = c("red", "green") ) %>%
    dySeries("monthly.returns.1", label =  paste0(ticker_2, " z-score") )  %>%
    dySeries("monthly.returns" , label = paste0(ticker_1," z-score")) %>% 
    dyLegend(show = "always") %>%
    dyRangeSelector() %>%
    print()
}


