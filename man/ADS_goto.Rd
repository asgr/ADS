\name{ADS_goto}
\alias{ADS_goto}
\title{
Open a NASA ADS Paper in the Default Browser
}
\description{
Opens the NASA ADS abstract page for a given bibcode in the system default web
browser.
}
\usage{
ADS_goto(paper = "2015PASA...32...33R")
}
\arguments{
  \item{paper}{
Character scalar; a unique NASA ADS bibcode (paper ID).
}
}
\value{
Called for its side effect (opens a browser tab).  Returns \code{NULL} invisibly.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ADS_metrics}}, \code{\link{ADS_export}}
}
\examples{
\dontrun{
# Open the abstract page for a specific paper
ADS_goto("2015PASA...32...33R")
}
}
