#' @title Fama French (Three factors) and CAPM Estimation
#'
#' @description  A function estimating the parameters of the Fama-French three factors model and the coefficients
#' of the CAPM model. The Fama-French three factors are: the market factor, SMB (Small Minus Big),
#' HML (High Minus Low).
#'
#' @export
#'
#' @param ticker A single character variable storing the ticker of the stock to be downloaded.
#' Note that since data are downloaded from Yahoo finance, the ticker should be the same as
#' the ones reported on Yahoo finance. See examples.
#' This variable can also be a data.frame with either two columns (one with dates in format %Y%m%d
#' and one with daily return of the portfolio), or one column with the daily returns of the
#' portfolio and as rownames the dates.
#'
#' @param from A single character variable storing the starting date from which we want to
#' download the data. The format should be "yyyy-mm-dd"
#'
#' @param to A single character variable storing the ending date up to the day in which we want
#' to download the stock data. The format should be "yyyy-mm-dd"
#'
#'
#' @examples
#' \dontrun{
#' ff3.Capm.estimation("AAPL", "2015-01-01", "2020-01-01")
#'
#'returns <- rnorm(n = 1827, mean = 0 , sd = 2.5)
#'dates <- seq(as.Date("2015-01-01"), as.Date("2020-01-01"), by = "days")
#'df <- data.frame(returns)
#'rownames(df) <- dates
#'ff3.Capm.estimation(df, "2015-01-01" , "2020-01-01")
#'
#'
#'returns <- rnorm(n = 1827, mean = 0 , sd = 2.5)
#'dates <- seq(as.Date("2015-01-01"), as.Date("2020-01-01"), by = "days")
#'df <- data.frame(dates, returns)
#'ff3.Capm.estimation(df, "2015-01-01" , "2020-01-01")
#'
#'}


ff3.Capm.estimation <- function (ticker, from, to ) {

  #Download the FF data
  url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
  temporary_file <- tempfile()
  download.file(url = url, destfile = temporary_file)
  ff_data <- unzip(temporary_file)
  ff_data <- read.csv(ff_data, skip = 3) #OURCE: https://www.codingfinance.com/post/2018-05-31-download-ff-data-in-r/


  ## Warning the user if there are too much missing values in the data frame
  if ( sum(is.na(ff_data)) > 10 ) {
    print( paste0("Warning: the FF-data frame had ", sum(is.na(ff_data)), " missing values" ))
  }

  ff_data <- na.omit(ff_data)
  ###################### IF STATEMENT ########################
  # if ( mean(is.na(ff_data < 1)) ) {
  #          na.omit(ff_data)
  # } else {
  #  print("too many NA values in the dataset")
  #  }
  #############################################################


  #Download the returns of the stock or import a data frame already installed in the
  #envinronment with A column with the dates (%Y-%m-%d) and a column with the return
  #of the portfolio in this order, or a column with only the returns and rownames as
  #dates
  if ( is.character(ticker) == T ) {
    y <- getSymbols(ticker, from = from, to = to,
                    auto.assign = T) %>% map(~Ad(get(.))) %>% reduce(merge)
    y <- diff(log(y))  #return of our stock
    y <- na.omit(y)
    y <- as.data.frame(y)
    y <- cbind( rownames(y), y)
    name <- colnames(y)[-1]
    colnames(y) <- c("Date", name)
    rownames(y) <- seq( 1, by = 1 , to = dim(y)[1] )
    colnames(y) <- c( "Date", paste0(ticker, " returns") )
  } else if ( is.data.frame(ticker) == T) {
    if ( length(ticker) == 1) {
      y <- ticker
      y <- na.omit(y)
      y <- as.data.frame(y)
      y <- cbind( rownames(y), y)
      name <- colnames(y)[-1]
      colnames(y) <- c("Date", name)
      rownames(y) <- seq( 1, by = 1 , to = dim(y)[1] )
    } else if ( length(ticker == 2) ) {
      y <- ticker
      y <- na.omit(y)
      name <- colnames(y)[-1]
      colnames(y) <- c("Date", name)
      rownames(y) <- seq( 1, by = 1 , to = dim(y)[1] )
      colnames(y) <- c( "Date", paste0("Portfolio returns") )
      rownames(y) <- seq( 1, by = 1 , to = dim(y)[1] )
      y$Date <- as.character(y$Date)
    } else {
      print("If not a stock ticker, the variable ticker should be a data frame with either two columns (one with dates in format %Y%d%m and one with daily return of the portfolio), or one column with the daily returns of the portfolio and as rownames the dates  ")
    }
  }


  Y <- as.matrix(y)   #we will need it afterward

  #Set date in Date format
  ff_data[,1] <- as.character(ff_data[,1])
  ff_data[,1] <- as.Date(ff_data[,1], tryFormats = "%Y%m%d")

  #Now we shift the first column (with the dates) in the left
  rownames(ff_data) <- ff_data[,1]
  ff_data <- data.frame(ff_data)

  #Now we can remove the first column
  ff_data <- ff_data[ , -1]

  #divide per 100 since they are not in percentage term
  ff_data <- ff_data/100

  ###Match the Dates in the two data frame###
  #firstly give name to the dates column
  ff_data <- cbind( rownames(ff_data), ff_data) #create the new column with the dates
  name <- colnames(ff_data)[-1]   #store the name of the other column in a variable
  colnames(ff_data) <- c("Date", name)
  rownames(ff_data) <- seq( 1, by = 1 , to = dim(ff_data)[1] )


  #Then we can match the data
  data_merged <- merge(ff_data, y, by.y = "Date", by.x = "Date",
                       all.y = T)
  data_merged <- na.omit(data_merged)

  #Create two different data sets
  ff_data <- data_merged[ ,1:length(ff_data)]
  y <- cbind( data_merged[, 1], data_merged[, -(1:length(ff_data)) ] ) %>%
    as.data.frame()

  ############### BETA ESTIMATION ####################
  ####################################################


  X <- cbind( rep(1, nrow(ff_data) ), ff_data$Mkt.RF, ff_data$SMB, ff_data$HML)
  #consider that Y is the return - risk free
  y[ ,2] <- as.numeric(y[ ,2])
  Y <- y[ ,2] - ff_data$RF
  Y <- as.matrix(Y)


  #Using linear regression model
  X_1 <- X[ ,-1]
  data_regression_FF = data.frame(X_1,Y)
  regression_FF <- lm(Y~X_1, data = data_regression_FF)
  regression_FF <- summary.lm(regression_FF) #store it in a variable


  #comparison with the CAPM model
  X_CAPM <- X_1[ ,1]
  data_regres_CAPM <- data.frame(X_CAPM, Y)
  regression_CAPM <- lm(Y~X_CAPM, data = data_regres_CAPM)
  regression_CAPM <- summary(regression_CAPM)

  #Merge the results (CAPM and FF) to create a table
  regression_merged <- rbind(regression_FF$coefficients, regression_CAPM$coefficients )

  #the table
  rownames(regression_merged) <- c("FF Intercept", "Market factor", "SMB", "HML",
                                   "CAPM Intercept", "Market factor")

  if ( is.character(ticker) == T ) { ######### IF
    regression_merged %>%
      kbl( caption = print( paste0("Coefficient estimates for ", ticker) ),
           col.names = c( "Coefficient Estimate", "Std. Error",
                          "t value", "p-value")) %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      pack_rows("FF 3 Factors Model", 1, 4 ) %>%
      pack_rows("CAPM Model", 5 , 6) %>%
      print()

    ##And an output of the data in the envinroment
    colnames(y) <- c( "Date", print( paste0(ticker, " returns") ) )
    assign("FF_3_factors_data", ff_data, envir = .GlobalEnv)
    assign("Stock/Portfolio_retunr", y , envir = .GlobalEnv)


    #And in console the beta
    print_regression_FF <- regression_FF$coefficients
    rownames(print_regression_FF) <- c("Intercept FF", "Market factor", "SMB", "HML")
    print(print_regression_FF)

    print_regression_CAPM <- regression_CAPM$coefficients
    rownames(print_regression_CAPM) <- c("Intercept CAPM", "Market factor")
    print(print_regression_CAPM) } else if (is.data.frame(ticker) == T) { ##########ELSE
      regression_merged %>%
        kbl( caption = print( paste0("Coefficient estimates for the Portfolio") ),
             col.names = c( "Coefficient Estimate", "Std. Error",
                            "t value", "p-value")) %>%
        kable_classic(full_width = F, html_font = "Cambria") %>%
        pack_rows("FF 3 Factors Model", 1, 4 ) %>%
        pack_rows("CAPM Model", 5 , 6) %>%
        print()

      ##And an output of the data in the envinroment
      colnames(y) <- c( "Date", print( paste0("Portfolio returns") ) )
      assign("FF_3_factors_data", ff_data, envir = .GlobalEnv)
      assign("Stock.Portfolio_return", y , envir = .GlobalEnv)


      #And in console the beta
      print_regression_FF <- regression_FF$coefficients
      rownames(print_regression_FF) <- c("Intercept FF", "Market factor", "SMB", "HML")
      print(print_regression_FF)

      print_regression_CAPM <- regression_CAPM$coefficients
      rownames(print_regression_CAPM) <- c("Intercept CAPM", "Market factor")
      print(print_regression_CAPM)
    }

}



