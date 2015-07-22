
gsSearch = function(searchterm='extra-pair bird', nhits = 30) {  
  
  pages = trunc((nhits/10))
  allout = list()
  for (i in 0:(pages)) {
    url    = paste0('http://scholar.google.com/scholar?start=',i*10,'&hl=en&as_sdt=1,5&as_vis=1&q=', gsub(' ','+',searchterm),'&scisbd=1',collapse='')
    urlres = getURL(url)  
    
    foo = htmlTreeParse(urlres,error=function(...){}, useInternalNodes = TRUE,encoding='utf-8')   
    
    fullresult = getNodeSet(foo,"//div[@class='gs_r']")
    out = lapply(fullresult,function(x) data.frame(AuSoY   = paste0(unlist(xpathSApply(x,".//div[@class='gs_a']",xmlValue)),''),
                                                   title   = paste0(unlist(xpathApply(x,".//h3",xmlValue)   ),''),
                                                   pdflink = paste0(unlist(xpathApply(x,".//div[@class='gs_md_wp gs_ttss']/a",function(z) xmlGetAttr(z,name='href'))),''),
                                                   url     = paste0(unlist(xpathApply(x,".//h3/a",function(z) xmlGetAttr(z,name='href'))),''),
                                                   stringsAsFactors = FALSE)
    )
    out = do.call(rbind,out)
    out$authors = gsub(' -.*','',out$AuSoY)
    out$year = gsub('\\D','',out$AuSoY)
    out$journal = gsub('(,.*)|([0123456789]{4})','',gsub('(^.*?- )|( -.*?$)','',out$AuSoY))
    out$AuSoY = NULL
    allout[[i+1]] = out
  }
  allout = do.call(rbind,allout)
  return(allout)
}
