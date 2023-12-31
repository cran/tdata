% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency.R
\name{f.x.times.z.years}
\alias{f.x.times.z.years}
\title{Create an \code{X-Times-Z-Years} Frequency}
\usage{
f.x.times.z.years(year, x, z, position)
}
\arguments{
\item{year}{An integer representing the year of the observation.}

\item{x}{An integer representing the number of partitions in each \code{z} years. It should be a positive integer.}

\item{z}{An integer representing the number of years. It should be a positive integer.}

\item{position}{An integer representing the position of the current observation. It should be a positive integer and cannot be larger than \code{x}.}
}
\value{
An object of class \code{ldtf}, which is also a list with the following members:
\item{class}{The class of this frequency.}
\item{year}{The \code{year}.}
\item{z}{The value: \code{z}.}
\item{x}{The value: \code{x}.}
\item{position}{The \code{position}.}
}
\description{
Use this function to create a frequency for time-series data that occurs \code{x} times every \code{z} years.
}
\details{
In order to use the \code{\link{as.frequency}} function for this type of frequency,
you need the following information:
\itemize{
\item \strong{Character Format} \code{"#:#"} (Similar to \code{X-Times-A-Year}. Note that the string representation refers to the first year of the interval.)
\item \strong{Class Id} \code{"x#z#"} (first '#' is the value: \code{x},
second '#' is the value: \code{z}; e.g., x23z4 means 23 times every 4 years)
}
}
\examples{

xtzy0 <- f.x.times.z.years(2020, 3, 2, 3)
# This frequency divides the year 2020 into 3 partitions and
# refers to the last partition. The next observation
# belongs to 2022 (not the next year).

xtzy_value_str <- as.character(xtzy0) # This will be '2020:3'.
xtzy_class_str <- get.class.id(xtzy0) # This will be 'x3z2'.

xtzy_new <- as.frequency("2021:3", "x3z4")
# This frequency divides the year 2021 into 3 partitions
# and refers to the last partition. The next observation occurs after 4 years.

# Don't make the following mistakes:
\donttest{
xtzy_invalid <- try(f.x.times.z.years(2020, 3, 5, 0))
xtzy_invalid <- try(f.x.times.z.years(2020, 3, 0, 1))
xtzy_invalid <- try(as.frequency("2021:25", "x24y2"))
}

}
