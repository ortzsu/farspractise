% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Generating filenames}
\usage{
make_filename(year)
}
\arguments{
\item{year}{an integer}
}
\value{
The function generates a filename with the year number given by the user.
}
\description{
The "make_filename" function produces "accident_year.csv.bz2"-type filename,
where "year" is given by the user.
}
\examples{
\dontrun{
make_filename(2013)
}

}

