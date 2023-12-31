% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{convert.to.XxYear}
\alias{convert.to.XxYear}
\title{Convert Data to Year-Based Frequency}
\usage{
convert.to.XxYear(variable, x, aggregateFun)
}
\arguments{
\item{variable}{A variable.}

\item{x}{Determines the number of partitions in each year, for example, use 12 for monthly data.}

\item{aggregateFun}{Function to aggregate data within each interval.}
}
\value{
A variable with year-based frequency.
}
\description{
Use this function to convert time-series data (currently implemented: daily) to time-series data with year-based frequency such as monthly, quarterly, yearly, etc.
}
\details{
See the details section of the \code{\link{convert.to.daily}} function.
}
\examples{
startFreq <- f.daily(c(2023,1,1))
v <- variable(c(1:(365*2)), startFreq)
w <- convert.to.XxYear(v,12,function(x)mean(x))

}
