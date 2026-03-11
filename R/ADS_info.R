ADS_library = function(library=NULL, Authorisation=NULL, rows=1000){

  headers = c(
    `Authorization` = paste0("Bearer ",Authorisation),
    `Content-Type` = "application/json"
  )

  output = GET(url = paste0("https://api.adsabs.harvard.edu/v1/biblib/libraries/", library,"?rows=",rows), add_headers(.headers=headers))

  #class(output) = 'ADS_library'
  return(invisible(output))
}

ADS_metrics = function(papers="2015PASA...32...33R", Authorisation=NULL){

  headers = c(
    `Authorization` = paste0("Bearer ",Authorisation),
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
  cites = get_ADS_info(ADS_metrics=x)$info
  cite_order = order(cites, decreasing = TRUE)
  H_index_pos = which(1:length(cites[cite_order]) <= cites[cite_order])
  H_index = if(length(H_index_pos) == 0L) 0L else max(H_index_pos)

  reads = get_ADS_info(ADS_metrics=x, "basic stats", "total number of reads")$info

  cat(paste0('Total: ', sum(cites)), '\n')
  cat(paste0('H-Index: ', H_index), '\n\n')

  cat(paste0(x$papers[cite_order],' ',cites[cite_order], ' ', reads[cite_order]), sep='\n')
}

plot.ADS_metrics = function(x, ...){
  cites = get_ADS_info(ADS_metrics=x)$info
  cite_order = order(cites, decreasing = TRUE)
  H_index_pos = which(1:length(cites[cite_order]) <= cites[cite_order])
  H_index = if(length(H_index_pos) == 0L) 0L else max(H_index_pos)

  reads = get_ADS_info(ADS_metrics=x, "basic stats", "total number of reads")$info

  par(mar=c(12.1,5.1,1.1,1.1))
  bar_cols = if(any(reads > 0)) {
    hcl(magmap(reads[cite_order], flip=TRUE, range=c(0,240), stretch='log', bad=240)$map)
  } else {
    rep(hcl(240), length(reads))
  }
  xloc = barplot(cites[cite_order], names.arg=x$papers[cite_order], ylab='Cites', las=2,
                 border=NA, col=bar_cols, ...)
  abline(h=seq(0,max(cites),by=100), lty=2, col='grey')
  abline(h=seq(0,max(cites),by=20), lty=3, col='lightgrey')
  if(H_index > 0){
    abline(h=H_index, lty=2, col='red')
    abline(v=xloc[H_index], lty=2, col='red')
  }
  legend('topright', legend=c(
    paste0('Papers: ', length(cites)),
    paste0('Cites: ', sum(cites)),
    paste0('H-Index: ', H_index),
    paste0('i1000: ', sum(cites >= 1000)),
    paste0('i100: ', sum(cites >= 100)),
    paste0('i10: ', sum(cites >= 10))
  ), lty=c(NA,NA,2,NA,NA,NA), col=c(NA,NA,'red',NA,NA,NA), bg='white')
}

ADS_export = function(papers="2015PASA...32...33R", Authorisation=NULL, format="%T %5.3L, %Y, %q, %V, %p, C=%c"){

  headers = c(
    `Authorization` = paste0("Bearer ",Authorisation),
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

get_ADS_info = function(ADS_metrics, type='citation stats', info='number of citing papers'){
  i = NULL
  info = foreach(i = seq_len(length(ADS_metrics$papers)), .combine='c')%do%{
    resp = ADS_metrics$metrics[[i]]
    if(http_error(resp)){
      warning(sprintf("ADS metrics API error for %s: HTTP %d", ADS_metrics$papers[i], status_code(resp)))
      0L
    } else {
      temp = content(resp)[[type]][[info]]
      if(length(temp) == 0) 0L else temp
    }
  }
  return(data.frame(paper=ADS_metrics$papers, info=info))
}

ADS_goto = function(paper="2015PASA...32...33R"){
  browseURL(paste0('https://ui.adsabs.harvard.edu/abs/',paper))
}
