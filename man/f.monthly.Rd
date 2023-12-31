% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency.R
\name{f.monthly}
\alias{f.monthly}
\title{Create a Monthly Frequency}
\usage{
f.monthly(year, month)
}
\arguments{
\item{year}{An integer representing the year of the observation.}

\item{month}{An integer representing the month of the observation (It should be between 1 to 12).}
}
\value{
An object of class \code{ldtf} which is also a list with the following members:
\item{class}{Determines the class of this frequency.}
\item{year}{Determines the \code{year}.}
\item{month}{Determines the \code{month}.}
}
\description{
Use this function to create a frequency for time-series data that occurs monthly.
}
\details{
In order to use the \code{\link{as.frequency}} function for this type of frequency,
you need the following information:
\itemize{
\item \strong{Character Format} \code{"#m#"} (first # is the \code{year}, second # is
the \code{month} (1 to 12); e.g., 2010m8 or 2010m12. Note that 2000m0 or 2000m13 are invalid.
\item \strong{Class Id} \code{"m"}
}
}
\examples{

m0 <- f.monthly(2020, 2)
#     this is a monthly frequency that refers to the second month of the year 2020.

m0_value_str <-  as.character(m0) # this will be '2020M2'.
m0_class_str <- get.class.id(m0) # this will be 'm'.

m_new <- as.frequency("2021m3", "m")
#     this is a monthly frequency that refers to the third month of the year 2021.

# Don't make the following mistakes:
\donttest{
m_invalid <- try(f.monthly(2020, 0))
m_invalid <- try(f.monthly(2020, 5))
m_invalid <- try(as.frequency("2021m0", "m"))
m_invalid <- try(as.frequency("2021m13", "m"))
m_invalid <- try(as.frequency("2021", "m"))
}

}
