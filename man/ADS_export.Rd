\name{ADS_export}
\alias{ADS_export}
\alias{print.ADS_export}
\title{
Export Formatted Citations from NASA ADS
}
\description{
\code{ADS_export} retrieves formatted citation strings for one or more papers from
the NASA ADS custom-export endpoint.  The returned object has a \code{print} S3
method that writes the formatted strings to the console.
}
\usage{
ADS_export(papers = "2015PASA...32...33R", Authorisation = NULL,
  format = "\%T \%5.3L, \%Y, \%q, \%V, \%p, C=\%c")

\method{print}{ADS_export}(x, ...)
}
\arguments{
  \item{papers}{
Character vector; one or more unique NASA ADS bibcodes (paper IDs).
}
  \item{Authorisation}{
Character scalar; NASA ADS API token. Obtain yours at
\url{https://ui.adsabs.harvard.edu/user/settings/token}.
}
  \item{format}{
Character scalar; ADS custom export format string.  The default produces a
human-readable string containing the title, up to five authors (truncated with
\emph{et al.}), year, journal, volume, page, and citation count.  See the ADS
documentation for available format tokens.
}
  \item{x}{
An object of class \code{'ADS_export'}, i.e. the output of \code{ADS_export}.
}
  \item{\dots}{
Further arguments passed to \code{print}.
}
}
\details{
Each bibcode results in one call to the \verb{/v1/export/custom} ADS API endpoint.
The raw HTTP responses are stored in the returned list.

\code{print.ADS_export} concatenates the formatted strings returned by the API
and writes them to the console.
}
\value{
\code{ADS_export} returns an object of class \code{'ADS_export'} invisibly.  It is a
list with two components:
\item{papers}{Character vector of bibcodes.}
\item{export}{List of raw \code{httr} response objects, one per bibcode.}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ADS_library}}, \code{\link{ADS_metrics}}, \code{\link{ADS_goto}}
}
\examples{
\dontrun{
ex <- ADS_export(papers = "2015PASA...32...33R", Authorisation = "my_token_here")
print(ex)
}
}
