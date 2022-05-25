#' CAPM data estimation
#'
#' CAPM Capital Asset Pricing Model is a concept based on an equation that describes the
#' relationship between the expected value of the returns and the risk of investing in
#' a security that is indicated by the Beta of the returns and the risk free rate. So, the market
#' risk premium indicates how much the underlying assest returns differ from the risk-free
#' rate. Negative Risk Premium tells us that the investment is a bad one thus you should
#' invest in the rf stock or another asset. The Capital Market line makes a relation between
#' all the optimal combined portfolios of risky assets with the risk free asset. The CML also
#' is the only track permitted to a portfolio that outperforms the efficient frontier,
#' therefore there cannot be portfolios that use the same asset above the Capital Market Line.
#' As a consequence, the slope of the Capital Market Line defines the Sharpe ratio namely
#' the average of the excess returns of the risk free per the volatility of the portfolio
#' excess returns, Sharpe Ratio=(Rp???Rf)/??p. The Security Market line shows the movements
#' of the CAPM against any expected return of the entire market.
#'
#' @export
#'
#' @param ticker A single character variable storing the tickers of the stocks of which you want the
#' the data.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param risk_free Numerical variable of length one. It indicates the risk free rate
#'
#' @examples
#' \dontrun{
#' risk_free <- 0.78
#' ticker <- "MSFT"
#' from <-"2015-01-01"
#' to <- "2020-01-01"
#' CAPM.data(ticker = ticker, from, to , risk_free = risk_free)
#' }
#'
#'


CAPM.data <- function (ticker, from, to , risk_free ) {

  prices<- getSymbols(ticker, auto.assign = T, from=from, to = to) %>%
  map(~Ad(get(.))) %>% reduce(merge)
  Ra<-dailyReturn(prices)

  mktreturn<- Ad(getSymbols("^GSPC", auto.assign=F, from=from, to = to , env = NULL))
  Rb<-dailyReturn((mktreturn))

  x<- c(PerformanceAnalytics::CAPM.CML.slope(Rb, risk_free)) #sharpe ratio

  y<- c(PerformanceAnalytics::CAPM.CML(Ra, Rb, risk_free)) #excess expected return

  z<- c(PerformanceAnalytics::CAPM.RiskPremium(Ra, risk_free)) #How much asset return differs from risk free

  f<- c(PerformanceAnalytics::CAPM.SML.slope(Rb, risk_free)) #

  CAPM_Data<- c("Capital Market Line Slope", "Capital Market Line", "Risk Premium", "Security Market Line Slope")
  Values<- c(x,y,z,f)
  CAPM<- data.frame(CAPM_Data, Values)
  View(CAPM)
  knitr::kable(CAPM,"pipe", col.names = c("CAPM_Data","Values"), align=c("l","c","c"))


  CAPM %>%
    kbl( caption =  paste0("CAPM Data for ", ticker),
         col.names = c( "CAPM Data" , "Values")) %>%
    kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
    print()

}
