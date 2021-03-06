% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/realtor.R
\name{get_realtor_locations_vitality}
\alias{get_realtor_locations_vitality}
\title{Meta level market data}
\usage{
get_realtor_locations_vitality(location_names = NULL, return_message = TRUE,
  ...)
}
\arguments{
\item{location_names}{vector of location names, location name must contain
a city name and a comma ie "Brooklyn, NY"}

\item{return_message}{if \code{TRUE} returns a message}

\item{...}{}
}
\value{
a \code{data_frame}
}
\description{
Acquires meta level information
for specified markets
}
\examples{
get_realtor_locations_vitality(location_names = c("La Jolla, CA", "Manhattan, NY", "Bethany, DE"))
}
