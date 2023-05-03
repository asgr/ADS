\name{ADS}
\alias{ADS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NASA ADS API Functions
}
\description{
Various functions to interact with NASA ADS Libraries and Papers.
}
\usage{
ADS_metrics(papers = "2015PASA...32...33R", Authorisation = NULL)
ADS_export(papers = "2015PASA...32...33R", format = "\%T \%5.3L, \%Y, \%q, \%V, \%p,
  C=\%c", Authorisation = NULL)
ADS_library(library = NULL, Authorisation = NULL, rows = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{papers}{
Character vector; unique paper ADS IDs.
}
  \item{Authorisation}{
Character scalar; NASA ADS API token. You can get this at https://ui.adsabs.harvard.edu/user/settings/token.
}
  \item{format}{
Character scalar; custom ADS export format.
}
  \item{rows}{
Integer scalar; maximum number of papers to return.
}
}
\value{
Erm, complicated. Do not worry about this too much!
}
\author{
Aaron Robotham
}

\examples{
\dontrun{
#Get papers (this is my private library, so you will need to find your own):
ASGRfirst = unlist(content(ADS_library(library="-I4xxWbuR_-7f2b77Te36Q",
  Authorisation="my_token_here"))$documents)
#Get metrics:
ASGRfirst_metrics = ADS_metrics(papers=ASGRfirst, Authorisation="my_token_here)
print(ASGRfirst_metrics)
plot(ASGRfirst_metrics)
}
}
