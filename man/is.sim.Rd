% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_data.R
\name{is.sim}
\alias{is.sim}
\title{Check format of simulated data list}
\usage{
is.sim(sim_list)
}
\arguments{
\item{sim_list}{List of simulation results to check}
}
\value{
TRUE if sim_list has the good format, FALSE otherwise (+ warnings)
}
\description{
Check format of simulated data list
}
\examples{
\donttest{
sim_list <- list(
  sit1 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
    var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
  ),
  sit2 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
    var1 = c(1.3, 2)
  )
)
CroptimizR:::is.sim(sim_list)

# Missing Date column
sim_list <- list(
  sit1 = data.frame(var1 = c(1.1, 1.5), var2 = c(NA, 2.1)),
  sit2 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
    var1 = c(1.3, 2)
  )
)
CroptimizR:::is.sim(sim_list)

# Bad Date format
sim_list <- list(
  sit1 = data.frame(
    Date = c("2009-11-30", "2009-12-10"),
    var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
  ),
  sit2 = data.frame(Date = c("2009-11-30", "2009-12-5"), var1 = c(1.3, 2))
)
CroptimizR:::is.sim(sim_list)
}

}
\keyword{internal}
