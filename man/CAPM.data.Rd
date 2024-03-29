% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capm_data.R
\name{CAPM.data}
\alias{CAPM.data}
\title{CAPM data estimation}
\usage{
CAPM.data(ticker, from, to, risk_free)
}
\arguments{
\item{ticker}{A single character variable storing the tickers of the stocks of which you want the
the data.
Note that since data are downloaded from Yahoo finance, the ticker should be the same as
the ones reported on Yahoo finance. See examples.}

\item{from}{A single character variable storing the starting date from which we want to
download the data. The format should be "yyyy-mm-dd"}

\item{to}{A single character variable storing the ending date from which we want to
download the data. The format should be "yyyy-mm-dd"}

\item{risk_free}{Numerical variable of length one. It indicates the risk free rate}
}
\description{
CAPM Capital Asset Pricing Model is a concept based on an equation that describes the
relationship between the expected value of the returns and the risk of investing in
a security that is indicated by the Beta of the returns and the risk free rate. So, the market
risk premium indicates how much the underlying assest returns differ from the risk-free
rate. Negative Risk Premium tells us that the investment is a bad one thus you should
invest in the rf stock or another asset. The Capital Market line makes a relation between
all the optimal combined portfolios of risky assets with the risk free asset. The CML also
is the only track permitted to a portfolio that outperforms the efficient frontier,
therefore there cannot be portfolios that use the same asset above the Capital Market Line.
As a consequence, the slope of the Capital Market Line defines the Sharpe ratio namely
the average of the excess returns of the risk free per the volatility of the portfolio
excess returns, Sharpe Ratio=(Rp???Rf)/??p. The Security Market line shows the movements
of the CAPM against any expected return of the entire market.
}
\examples{
\dontrun{
risk_free <- 0.78
ticker <- "MSFT"
from <-"2015-01-01"
to <- "2020-01-01"
CAPM.data(ticker = ticker, from, to , risk_free = risk_free)
}


}
