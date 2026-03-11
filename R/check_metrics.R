library(ADS)
library(httr)

ADS_token <- Sys.getenv("ADS_TOKEN")
if (nchar(ADS_token) == 0L) stop("No ADS_TOKEN set")

#get library paper IDs
ASGR_first = unlist(content(ADS_library(library="-I4xxWbuR_-7f2b77Te36Q", Authorisation=ADS_token))$documents)
#get metrics
ASGR_first_metrics = ADS_metrics(papers=ASGR_first, Authorisation=ADS_token)
#plot
png(paste0('data/ASGR_first_metrics.png'), width=800, height=800)
plot(ASGR_first_metrics)
legend('top', legend=Sys.Date())
dev.off()

#get library paper IDs
ASGR_all = unlist(content(ADS_library(library="aiv7eTyxRsCqB7jtDJw1YQ", Authorisation=ADS_token))$documents)
#get metrics
ASGR_all_metrics = ADS_metrics(papers=ASGR_all, Authorisation=ADS_token)
#plot
png(paste0('data/ASGR_all_metrics.png'), width=800, height=800)
plot(ASGR_all_metrics)
legend('top', legend=Sys.Date())
dev.off()
