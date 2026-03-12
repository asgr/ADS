\name{get_ADS_info}
\alias{get_ADS_info}
\title{
Extract a Specific Metric from an ADS_metrics Object
}
\description{
Helper function that extracts a named scalar metric (e.g. citation count or read
count) for every paper stored in an \code{'ADS_metrics'} object and returns a
tidy data frame.
}
\usage{
get_ADS_info(ADS_metrics, type = 'citation stats',
             info = 'number of citing papers')
}
\arguments{
  \item{ADS_metrics}{
An object of class \code{'ADS_metrics'}, i.e. the output of \code{\link{ADS_metrics}}.
}
  \item{type}{
Character scalar; the top-level metric category to extract.  Common values
include \code{"citation stats"} and \code{"basic stats"}.
}
  \item{info}{
Character scalar; the specific metric to extract within the chosen \code{type}.
Common values include \code{"number of citing papers"} (default) and
\code{"total number of reads"}.
}
}
\value{
A data frame with columns:
\item{paper}{Character; the bibcode of the paper.}
\item{info}{Numeric; the requested metric value.  Papers for which the metric is
unavailable are returned as \code{0L}.}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ADS_metrics}}
}
\examples{
\dontrun{
m <- ADS_metrics(papers = c("2015PASA...32...33R", "2011MNRAS.418.1587T"),
                 Authorisation = "my_token_here")

# Total citations per paper
get_ADS_info(m, type = "citation stats", info = "number of citing papers")

# Total reads per paper
get_ADS_info(m, type = "basic stats", info = "total number of reads")
}
}
