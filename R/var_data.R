#' Compute the VaR for different percentiles
#'
#' Compute the VaR for different perncentiles and plot it in tables of various format: one in
#' console, one as a data frame, and one as an image.
#'
#' @export
#'
#' @param ticker A single character variable storing the ticker of the stock to be downloaded.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#' This variable can also be a data.frame with either two columns (one with dates in format %Y%m%d
#' and one with daily return of the portfolio), or one column with the daily returns of the
#' portfolio and as rownames the dates. See examples
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date up to the day in which we want
#' to download the stock data. The format should be "yyyy-mm-dd"
#'
#' @param p Numeric vector containing the desired percentile. Could also be a single numeric
#' variable. Default to p = c(0.99,0.95)
#'
#' @param W Amount invested in the portfolio (or stock) stored in the variable "ticker".
#' Default to W = 1
#'
#' @examples
#' \dontrun{
#' #With ticker as a single character variable
#' VaR.data("MSFT" , from = "2017-01-01", to = "2020-01-01" , p = c(0.95,0.97,0.99))
#'
#' #With a data frame of one single column of returns and dates as name of the rows as ticker
#' returns <- rnorm(n = 1827, mean = 0 , sd = 3)/100
#' dates <- seq(as.Date("2015-01-01"), as.Date("2020-01-01"), by = "days")
#' df <- data.frame(returns)
#' rownames(df) <- dates
#' VaR.data(df, from = "2017-01-01", to = "2020-01-01", p=0.99)
#'
#' #With a data frame of two columns as ticker, one of dates and one of returns
#' returns <- rnorm(n = 1827, mean = 0 , sd = 3)/100
#' dates <- seq(as.Date("2015-01-01"), as.Date("2020-01-01"), by = "days")
#' df <- data.frame(dates, returns)
#' VaR.data(df, from, to )
#' }

VaR.data <- function(ticker, from, to, p = c(0.99,0.95), W = 1) {

  if ( is.character(ticker) == T ) {

    prices<- Ad(getSymbols(ticker, auto.assign = F, from=from, to = to))
    returns<- diff(log(prices))
    #compute the historical and modified VaR
    H_VaR <- c()
    M_VaR <- c()
    G_VaR <- c()
    for ( i in 1:length(p)) {
      H_VaR[i]<- c(VaR(returns, p[i], method="historical"))*W
      M_VaR[i]<-c(VaR(returns, p[i], method="modified"))*W
      G_VaR[i]<-c(VaR(returns, p[i], method="gaussian"))*W
    }
    Historical <- H_VaR
    Modified <- M_VaR
    Gaussian <- G_VaR
    Percentiles <- paste0( "P",p)
    VaRs<-data.frame(Percentiles, Historical, Modified,Gaussian)
    knitr::kable(VaRs,"pipe",col.name=c("Percentile", "Historical","Modified", "Gaussian"),
                 align=c("l","c","c")) %>% print()
    View(VaRs)

    VaRs %>%
      kbl( caption = print( paste0("VaR of ", ticker,  " returns") ),
           col.names = c( "Percentiles", "Historical","Modified", "Gaussian")) %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      print()

  } else if ( is.data.frame(ticker) == T ) {
    if ( length(ticker) == 1) {

      H_VaR <- c()
      M_VaR <- c()
      G_VaR <- c()
      for ( i in 1:length(p)) {
        H_VaR[i]<- c(VaR(ticker[,1], p[i], method="historical"))*W
        M_VaR[i]<-c(VaR(ticker[,1], p[i], method="modified"))*W
        G_VaR[i]<-c(VaR(ticker[,1], p[i], method= "gaussian"))*W
      }
      Historical <- H_VaR
      Modified <- M_VaR
      Gaussian <- G_VaR
      Percentiles <- paste0( "P",p)
      VaRs<-data.frame(Percentiles, Historical, Modified, Gaussian)
      knitr::kable(VaRs,"pipe",col.name=c("Percentile", "Historical","Modified", "Gaussian"),
                   align=c("l","c","c")) %>% print()
      View(VaRs)

      VaRs %>%
        kbl( caption = print( paste0("VaR of the Portfolio returns") ),
             col.names = c( "Percentiles", "Historical","Modified", "Gaussian")) %>%
        kable_classic(full_width = F, html_font = "Cambria") %>%
        print()

    } else if ( length(ticker == 2)) {

      H_VaR <- c()
      M_VaR <- c()
      G_VaR <- c()
      for ( i in 1:length(p)) {
        H_VaR[i]<- c(VaR(ticker[,2], p[i], method="historical"))*W
        M_VaR[i]<-c(VaR(ticker[,2], p[i], method="modified"))*W
        G_VaR[i]<-c(VaR(ticker[,2], p[i], method= "gaussian"))*W
      }
      Historical <- H_VaR
      Modified <- M_VaR
      Gaussian <- G_VaR
      Percentiles <- paste0( "P",p)
      VaRs<-data.frame(Percentiles, Historical, Modified, Gaussian)
      knitr::kable(VaRs,"pipe",col.name=c("Percentile", "Historical","Modified", "Gaussian"),
                   align=c("l","c","c")) %>% print()
      View(VaRs)

      VaRs %>%
        kbl( caption = print( paste0("VaR of the Portfolio returns") ),
             col.names = c( "Percentiles", "Historical","Modified", "Gaussian")) %>%
        kable_classic(full_width = F, html_font = "Cambria") %>%
        print()

    } else {
      print("If not a stock ticker, the variable ticker should be a data frame with either two columns (one with dates in format %Y%d%m and one with daily return of the portfolio), or one column with the daily returns of the portfolio and as rownames the dates ")
    }

  }

}




