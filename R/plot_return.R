#' Plot returns of a given stock
#'
#' Plot a time series of the return of a given stock.
#'
#' @export
#'
#' @param ticker A single character variable storing the ticker of the stock to be downloaded.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param time A single character variable which indicate the desired time frame of the
#' returns. Should be one among the following: "days" ; "weeks" ; "months" ; "quarters" ;
#' "years".
#' Default to "days"
#'
#' @examples
#' \dontrun{
#' plot.return( "AAPL"  ,"2016-01-01" ,"2022-01-01" , time = "quarters")
#'
#' ##if you want to store a dataset in the envinroment
#' AAPL_ret <- plot.return( "AAPL"  ,"2016-01-01" ,"2022-01-01" , time = "quarters")
#' }



plot.return <- function(ticker, from, to , time = "days" ) {
  #Download the data
  data <- getSymbols( ticker , from = from , to = to,
                      auto.assign = T ) %>%
    map( ~Ad(get(.)) ) %>% #take only the adj close
    reduce(merge)
  #Convert data to time period
  data <- xts::to.period(data, period = time)
  data <- data$data.Close
  #Transform prices into returns
  data <- diff( log(data) )*100
  data <- na.omit(data)
  #Conditions on the graph
  dygraph(data, ylab = "Return", xlab = ticker)  %>%
    dyOptions(fillGraph = T, fillAlpha = 0.2 ) %>%
    dySeries(label = print(paste0(ticker , " return")) ) %>%
    dyLegend(show = "always") %>%
    dyRangeSelector() %>%
    print()

  #Display a data.frame in the env
  'colnames<-'(data.frame(data) , print(paste0(ticker, " returns")) ) %>% return()
}







