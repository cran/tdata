% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency.R
\name{f.list.date}
\alias{f.list.date}
\title{Create a \code{List-Date} Frequency}
\usage{
f.list.date(items, value = NULL, reformat = TRUE)
}
\arguments{
\item{items}{The items in the list in \code{YYYYMMDD} format.}

\item{value}{The current value in \code{YYYYMMDD} format. If null, the first value in \code{items} is used.}

\item{reformat}{If the elements of \code{items} are not in \code{YYYYMMDD} format, set this to be \code{TRUE}.}
}
\value{
An object of class \code{ldtf}. It is also a list with the following members:
\item{class}{Determines the class of this frequency.}
\item{items}{Determines the \code{items}.}
\item{value}{Determines the \code{value}.}
}
\description{
Use this frequency for data with date labels. It is generally a list of dates, but it can also be used to label observations outside this list.
}
\details{
In order to use the \code{\link{as.frequency}} function for this type of frequency,
you need the following information:
\itemize{
\item \strong{Character Format}: \code{"YYYYMMDD"} (i.e., the \code{item})
\item \strong{Class Id}: \code{Ld} or \code{Ld:...} (where '...' represents the semi-colon-separated \code{items})
}
}
\examples{

Ld0 <- f.list.date(c("20231101","20220903","20200823","20230303"), "20200823")

Ld0_value_str <-  as.character(Ld0) # this will be '20200823'.
Ld0_class_str <- get.class.id(Ld0)
#      this will be 'Ld:20231101;20220903;20200823;20230303'.

Ld_new <- as.frequency("20231101", "Ld:20231101;20220903;20200823;20230303")
Ld_new0 <- as.frequency("20231101", "Ld")
#     compared to the previous one, its items will be empty

# Don't make the following mistakes:
\donttest{
Ld_invalid <- try(as.frequency("20231102", "Ld:20231101;20220903;20200823;20230303"))
  # 'E' is not a member of the list
Ld_invalid <- try(f.list.date(c("20231101","20220903","20200823","20230303"), "20231102"))
}

}
