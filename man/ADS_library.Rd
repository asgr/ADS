\name{ADS_library}
\alias{ADS_library}
\title{
Retrieve Papers from a NASA ADS Library
}
\description{
Fetches the list of bibcodes (paper IDs) stored in a private or public NASA ADS
library via the ADS API.
}
\usage{
ADS_library(library = NULL, Authorisation = NULL, rows = 1000)
}
\arguments{
  \item{library}{
Character scalar; the unique ADS library ID.  You can find this in the URL when
browsing your library on \url{https://ui.adsabs.harvard.edu}.
}
  \item{Authorisation}{
Character scalar; NASA ADS API token. Obtain yours at
\url{https://ui.adsabs.harvard.edu/user/settings/token}.
}
  \item{rows}{
Integer scalar; maximum number of papers to return from the library (default 1000).
}
}
\value{
Returns an \code{httr} response object invisibly.  Use
\code{httr::content(result)$documents} to extract the vector of bibcodes.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ADS_metrics}}, \code{\link{ADS_export}}, \code{\link{ADS_goto}}
}
\examples{
\dontrun{
# Fetch up to 1000 bibcodes from a library
resp <- ADS_library(library = "-I4xxWbuR_-7f2b77Te36Q", Authorisation = "my_token_here")
papers <- unlist(httr::content(resp)$documents)
head(papers)
}
}
