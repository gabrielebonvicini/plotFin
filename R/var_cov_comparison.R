#' Comparing Variance,Volatility,Covariance and Beta
#'
#' This function enables to select different stocks and creates a table that compares
#' variance,volatility, covariance and beta of these stocks with a market index of your
#' choice.
#'
#'
#' @param ticker A character vector storing the tickers of the stocks of which you want the
#' correlation.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#'
#' @param market_index A single character variable storing the ticker of the market index
#' you want to download. Default to "^GSPC" (S&P500)
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date up to the day in which we want
#' to download the stock data. The format should be "yyyy-mm-dd"
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' var.cov.comparison(ticker =c("AA","AAPL","AXP","BA","BAC","CAT","CVX","DD","DIS",
#' "GE","HD","HPQ","INTC","IBM","JNJ","JPM","KO","MCD","MMM",
#' "MRK","MSFT","NKE","PFE","PG","TRV","VZ","WMT","XOM","T"),
#' from = "2020-01-01", to = "2022-01-01" )
#'
#'
#' var.cov.comparison( ticker = c ("AAPL" , "AMZN" ), market_index =   "FTSEMIB.MI",
#'from = "2020-01-01" , to = "2022-01-01" )
#'
#' }
#'
#'


var.cov.comparison <- function(ticker , market_index = "^GSPC" ,from ,to ) {
  #download data of the first variable
  RET1 <- getSymbols(ticker, from = from, to = to
                     , auto.assign = T ) %>%
    purrr::map(~Ad(get(.))) %>% reduce(merge)
  #download data of the market index
  RETM <- getSymbols(market_index, from = from, to = to,
                     env = NULL)
  #take only the adjusted close
  RETM <- RETM[ ,6]
  #compute daily returns
  RET1 <-diff(log(RET1))
  RETM <-diff(log(RETM))
  #remove the NA first row
  RET1 <- RET1[-c(1)]
  RETM <- RETM[-c(1)]
  #Some stocks has less data
  #make them equal removing the NA cells
  RET1 <- na.omit(RET1)
  #and setting equal to the number of return of the market
  if ( nrow(RET1) != nrow(RETM) ) {
    f <-  merge(RET1,RETM) %>% na.omit() #if they have different number of rows the merge function will
    RET1 <- f[ ,1:length(ticker)]            #return NA where there are the missing values
    RETM <- f[ , length(ticker)+1]
  }
  #compute the variance
  volatility_x = NULL
  variance_x = NULL
  for ( i in 1 : length(ticker) ) { #we need the for loop because ticker will be a vector
    variance_x[i] <- var(RET1[ , i])     #with different stocks
    volatility_x[i] <- sqrt(variance_x[i])
  }
  #compute the beta
  covariance = NULL
  beta1 = NULL
  for ( t in 1 : length(ticker) ) {
    covariance[t] <- cov(RET1[ ,t], RETM)
    variance_z <- var(RETM)
    beta1[t] <- (covariance[t]/variance_z)
  }

  #Create the data frame
  data2 <- rbind( variance_x, volatility_x, covariance, beta1)
  if (market_index == "^GSPC") {
    rownames(data2) <- c("Variance" ,"Volatility",
                         "Covariance (Stock , S&P500)",
                         "Beta (Stock , S&P500)")
  } else {
    rownames(data2) <- c("Variance" ,"Volatility",
                         print( paste0("Covariance (Stock , ",market_index,")" ) ),
                         print( paste0("Beta (Stock , ",market_index,")") ) )
  }
  colnames(data2) <- ticker
  data2 <- as.data.frame(data2)


  #give the data in the envinroment
  Stock_Returns <<- RET1
  Index_Returns <<- RETM

  #give name to the columns of the 2 dataset
  colnames(Stock_Returns) <- ticker     #non me le runna !!!!
  colnames(Index_Returns) <- market_index

  print( "Variances, Volatilities and Returns should be read as percentage")

  #create the  value in console and a table
  data2 %>% print()
  data2 %>%
    kbl(caption = "Stock's parameters comparison", col.names = ticker ) %>%
    kable_classic(full_width = F, html_font = "Cambria") %>% return()
}










