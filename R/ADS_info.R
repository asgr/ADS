ADS_library = function(library=NULL, Authorisation=NULL, rows=1000){

  headers = c(
    `Authorization` = paste0("Bearer:",Authorisation),
    `Content-Type` = "application/json"
  )

  output = GET(url = paste0("https://api.adsabs.harvard.edu/v1/biblib/libraries/", library,"/?rows=",rows), add_headers(.headers=headers))

  #class(output) = 'ADS_library'
  return(invisible(output))
}

ADS_metrics = function(papers="2015PASA...32...33R", Authorisation=NULL){

  headers = c(
    `Authorization` = paste0("Bearer:",Authorisation),
    `Content-Type` = "application/json"
  )

  paper = NULL
  metrics = foreach(paper=papers)%do%{
    body = paste0('{"bibcodes": ["',paper,'"]}')
    return(POST(url = "https://api.adsabs.harvard.edu/v1/metrics", add_headers(.headers=headers), body = body))
  }

  output = list(papers=papers, metrics=metrics)
  class(output) = 'ADS_metrics'
  return(invisible(output))
}

print.ADS_metrics = function(x, ...){
  i = NULL
  cites = foreach(i = 1:length(x$papers), .combine='c')%do%{
    content(x$metrics[[i]])$`citation stats`$`number of citing papers`
  }
  cite_sort = sort(cites, decreasing = TRUE)
  H_index = max(which(1:length(cite_sort) <= cite_sort))

  cat(paste0('Total: ', sum(cites)), '\n')
  cat(paste0('H-Index: ', H_index), '\n\n')

  cat(paste0(x$papers,': ',cites), sep='\n')
}

plot.ADS_metrics = function(x, ...){
  i = NULL
  cites = foreach(i = 1:length(x$papers), .combine='c')%do%{
    content(x$metrics[[i]])$`citation stats`$`number of citing papers`
  }
  cite_order = order(cites, decreasing = TRUE)
  H_index = max(which(1:length(cites[cite_order]) <= cites[cite_order]))

  par(mar=c(12.1,5.1,1.1,1.1))
  xloc = barplot(cites[cite_order], names.arg=x$papers[cite_order], ylab='Cites', las=2, col='pink', ...)
  abline(h=seq(0,max(cites),by=100), lty=2, col='grey')
  abline(h=seq(0,max(cites),by=20), lty=3, col='lightgrey')
  abline(h=H_index, lty=2, col='red')
  abline(v=xloc[H_index], lty=2, col='red')
  legend('topright', legend=c(
    paste0('Papers: ', length(cites)),
    paste0('Cites: ', sum(cites)),
    paste0('H-Index: ', H_index)
  ), lty=c(NA,NA,2), col=c(NA,NA,'red'), bg='white')
}

ADS_export = function(papers="2015PASA...32...33R", Authorisation=NULL, format="%T %5.3L, %Y, %q, %V, %p, C=%c"){

  headers = c(
    `Authorization` = paste0("Bearer:",Authorisation),
    `Content-Type` = "application/json"
  )

  paper = NULL
  export = foreach(paper = papers)%do%{
    body = paste0('{"bibcode": ["',paper,'"], "format": "%T %5.3L, %Y, %q, %V, %p, C=%c"}')
    return(POST(url = "https://api.adsabs.harvard.edu/v1/export/custom", add_headers(.headers=headers), body = body))
  }

  output = list(papers=papers, export=export)
  class(output) = 'ADS_export'
  return(invisible(output))
}

print.ADS_export = function(x, ...){
  i = NULL
  info = foreach(i = 1:length(x$papers), .combine='c')%do%{
    paste0(x$papers[i],': ', content(x$export[[i]])$export)
  }
  cat(info)
}
