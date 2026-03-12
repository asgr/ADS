\name{ADS_metrics}
\alias{ADS_metrics}
\alias{print.ADS_metrics}
\alias{plot.ADS_metrics}
\title{
Retrieve and Display NASA ADS Citation Metrics
}
\description{
\code{ADS_metrics} fetches citation and read metrics from the NASA ADS API for one or
more papers identified by their bibcodes. The returned object has \code{print} and
\code{plot} S3 methods for convenient summary output and visualisation.
}
\usage{
ADS_metrics(papers = "2015PASA...32...33R", Authorisation = NULL)

\method{print}{ADS_metrics}(x, ...)

\method{plot}{ADS_metrics}(x, ...)
}
\arguments{
  \item{papers}{
Character vector; one or more unique NASA ADS bibcodes (paper IDs).
}
  \item{Authorisation}{
Character scalar; NASA ADS API token. Obtain yours at
\url{https://ui.adsabs.harvard.edu/user/settings/token}.
}
  \item{x}{
An object of class \code{'ADS_metrics'}, i.e. the output of \code{ADS_metrics}.
}
  \item{\dots}{
Further arguments passed to \code{print} or \code{plot}.
}
}
\details{
\code{ADS_metrics} calls the \verb{/v1/metrics} endpoint of the NASA ADS API once per
bibcode.  The raw HTTP responses are stored in the returned list together with the
vector of bibcodes.

\code{print.ADS_metrics} prints the total citation count, the H-index, and a
per-paper summary sorted in decreasing citation order.

\code{plot.ADS_metrics} draws a bar chart of citations (sorted descending) with a
red dashed line indicating the H-index. Bars are coloured by total read count (blue =
few reads, green = intermediate, red = many reads).
}
\value{
\code{ADS_metrics} returns an object of class \code{'ADS_metrics'} invisibly.  It is a
list with two components:
\item{papers}{Character vector of bibcodes.}
\item{metrics}{List of raw \code{httr} response objects, one per bibcode.}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ADS_library}}, \code{\link{ADS_export}}, \code{\link{get_ADS_info}},
\code{\link{ADS_goto}}
}
\examples{
\dontrun{
# Retrieve metrics for a single paper
m <- ADS_metrics(papers = "2015PASA...32...33R", Authorisation = "my_token_here")
print(m)
plot(m)

# Retrieve metrics for multiple papers from a library
papers <- unlist(httr::content(
  ADS_library(library = "-I4xxWbuR_-7f2b77Te36Q", Authorisation = "my_token_here")
)$documents)
m_all <- ADS_metrics(papers = papers, Authorisation = "my_token_here")
print(m_all)
plot(m_all)
}
}
