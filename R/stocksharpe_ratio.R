#' Sharpe Ratio plot
#'
#' It is a function that represents the sharpe ratio of a portfolio composed of two stocks
#' in a given data frame. The Sharpe index is a metric that measures the extra return
#' realized by a portfolio (or fund) per unit of total risk borne in comparison to the
#' risk-free rate. This function has been created calculating the ratio between the
#' difference between the  portfolio return and risk free rate and  the standard
#' deviation of the portfolio.
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
#' @param weights Numerical vector of length two. It indicates the weight that each stock has
#'
#' @param risk_free Numerical variable of length one. It indicates the risk free rate
#'
#' @examples
#' \dontrun{
#' stock.sharpe.ratio( ticker = c("NFLX","AMZN"), weights=c(0.50,0.50),
#' from="2010-01-01", to="2022-01-01", risk_free = 0.05)
#' }


stock.sharpe.ratio <- function(ticker, from, to, risk_free, weights) {

  #define ticker_1 and ticker_2 from ticker_list
  ticker_1 <-ticker[1]
  ticker_2 <- ticker[2]

  #import data for the first stock
  daily_stock_prices_1 <-getSymbols( ticker_1, from = from, to= to
                                     , auto.assign = T ) %>%
    purrr:: map(~Ad(get(.))) %>% reduce(merge)

  D1<-na.omit(daily_stock_prices_1)


  monthly_stock_returns_1<- periodReturn(D1, period = 'monthly', from=from, to=to,
                                         type = 'log')
  MSR1 <- na.omit(monthly_stock_returns_1)


  #import data for the second stock

  daily_stock_prices_2 <- getSymbols( ticker_2, from = from, to= to
                                      , auto.assign = T ) %>%
    purrr:: map(~Ad(get(.))) %>% reduce(merge)

  D2<-na.omit(daily_stock_prices_2)


  monthly_stock_returns_2<- periodReturn(D2, period = 'monthly', from=from, to=to,
                                         type = 'log')
  MSR2 <-na.omit(monthly_stock_returns_2)

  #compute stock volatility
  stock_volatility_1 <- sd(daily_stock_prices_1)
  stock_volatility_2 <-sd(daily_stock_prices_2)

  ##compute portfolio returns
  return_ticker_1 <- MSR1
  return_ticker_2 <- MSR2

  #plot(portfolio_monthly_returns)
  total_return <- merge.xts(return_ticker_1,return_ticker_2 )
  portfolio_monthly_returns <- Return.portfolio(total_return,weights ,from=from,to=to)
  PMR <- na.omit( portfolio_monthly_returns)


  ##compute porfolio sd
  sd1 <-na.omit(stock_volatility_1)
  sd2 <- na.omit(stock_volatility_2)

  c <- CoVariance(return_ticker_1,return_ticker_2)

  sd <- weights[1]*sd1+weights[2]*sd2+(2*weights[1]*weights[2]*c)^(0.5)

  ##compute sharpe ratio
  Data <- PMR
  sharpe = (Data[]-risk_free)/sd

  #compute the dygraph of sharpe ratio
  dygraph(sharpe, main = "Sharpe Ratio") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dyRangeSelector()


}



