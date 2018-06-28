\name{data}
\alias{data}
\docType{data}
\title{Daily precipitation, maximum and minimum temperature dataset}
\description{
  This data set contains daily precipitation, maximum (Tmax) and minimum (Tmin) temperature
  in format "RClimdex", each row is for one calendar day. The order of the columns
  is: Year - Month - RR - Tmax - Tmin
  }
\usage{data}
\format{
  Data frame with columns.
  \describe{
    \item{\code{year}}{a numeric vector, year}
    \item{\code{month}}{a numeric vector, month}
    \item{\code{day}}{a numeric vector, day}
    \item{\code{rr}}{a numeric vector, precipitation}
    \item{\code{tmax}}{a numeric vector, daily maximum temperature}
    \item{\code{tmin}}{a numeric vector, daily minimum temperature}
  }
}
\examples{
data(data)
## maybe str(data) ; plot(data) ...
}
\keyword{datasets}
