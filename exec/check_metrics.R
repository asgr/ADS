library(ADS)
library(httr)
library(magicaxis)

ADS_token <- trimws(Sys.getenv("ADS_TOKEN"))
if (nchar(ADS_token) == 0L) stop("No ADS_TOKEN set")

fetch_library_papers = function(library_id, label, authorisation) {
  resp = ADS_library(library=library_id, Authorisation=authorisation)
  if (http_error(resp)) {
    stop(sprintf("ADS library API error (%s): HTTP %d - %s",
                 label, status_code(resp), content(resp, as="text", encoding="UTF-8")))
  }
  papers = unlist(content(resp)$documents)
  if (length(papers) == 0L) {
    stop(sprintf("No papers returned for %s library - check ADS_TOKEN and library ID", label))
  }
  papers
}

#get library paper IDs
ASGR_first = fetch_library_papers("-I4xxWbuR_-7f2b77Te36Q", "ASGR_first", ADS_token)
#get metrics
ASGR_first_metrics = ADS_metrics(papers=ASGR_first, Authorisation=ADS_token)
#plot
png(paste0('data/ASGR_first_metrics.png'), width=800, height=800)
plot(ASGR_first_metrics)
legend('top', legend=Sys.Date())
dev.off()

#get library paper IDs
#ASGR_all = fetch_library_papers("aiv7eTyxRsCqB7jtDJw1YQ", "ASGR_all", ADS_token)
#get metrics
#ASGR_all_metrics = ADS_metrics(papers=ASGR_all, Authorisation=ADS_token)
#plot
#png(paste0('data/ASGR_all_metrics.png'), width=800, height=800)
#plot(ASGR_all_metrics)
#legend('top', legend=Sys.Date())
#dev.off()
