\name{ADS}
\alias{ADS}
\alias{ADS_library}
\alias{ADS_metrics}
\alias{ADS_export}
\alias{print.ADS_metrics}
\alias{print.ADS_export}
\alias{plot.ADS_metrics}
\alias{get_ADS_info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NASA ADS API Functions
}
\description{
Various functions to interact with NASA ADS Libraries and Papers.
}
\usage{
ADS_metrics(papers = "2015PASA...32...33R", Authorisation = NULL)
ADS_export(papers = "2015PASA...32...33R", Authorisation = NULL,
  format = "\%T \%5.3L, \%Y, \%q, \%V, \%p, C=\%c")
ADS_library(library = NULL, Authorisation = NULL, rows = 1000)

get_ADS_info(ADS_metrics, type = 'citation stats', info = 'number of citing papers')

\method{print}{ADS_metrics}(x, ...)
\method{print}{ADS_export}(x, ...)

\method{plot}{ADS_metrics}(x, ...)
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
  \item{library}{
Character scalar; ADS library ID.
}
  \item{rows}{
Integer scalar; maximum number of papers to return.
}
  \item{ADS_metrics}{
List; object of class 'ADS_metrics', i.e. output of \code{ADS_metrics}.
}
  \item{type}{
Character scalar; type of metric wanted.
}
  \item{info}{
Character scalar; info of metric wanted (given the \option{type}.
}
  \item{x}{
An object of a relevant class.
}
  \item{\dots}{
Other arguments passed onto \code{print} and \code{plot}.
}
}
\value{
Erm, complicated. Don't worry about this too much!

For the plots, the colour reflects the total reads (blue is few, green is middle, red is many).
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
