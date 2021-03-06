
#LAMR
#LAMR can handle multiple queries per request
{
makeLAMRQry = function(d, id,validFields = c('atitle','stitle','year')) {
  #one line in d = one queryid
  if (missing(id))   d$id = row.names(d) else d$id = d[,id]
                      
  vars = intersect(names(d),validFields)
  if (length(vars) == 0) stop(paste('variable names must be one or more of',paste(validFields,collapse=',')))
  
  d    = d[,c(vars,'id')]
 
    qry = reshape(d,varying = c(vars),
                v.names = 'val',
                timevar = 'thing',
                times = c(vars),
                direction = 'long')
  return(qry)
}

postLAMRReq = function(qry,url    = "https://ws.isiknowledge.com/cps/xrpc") {
  
  wokHeader = paste('<?xml version="1.0" encoding="UTF-8" ?> 
                    <request xmlns="http://www.isinet.com/xrpc42" >
                    <fn name="LinksAMR.retrieve"> 
                    <list> 
                    <map> 
                    <!-- our validation should go via IP -->  
                    </map> 
                    <map> 
                    <list name="WOS"> 
                    <val>timesCited</val>
                    <val>ut</val> 
                    <val>doi</val> 
                    <val>sourceURL</val> 
                    <val>citingArticlesURL</val> 
                    <val>relatedRecordsURL</val> 
                    </list> 
                    </map> 
                    <map> ')
  wokFooter = paste('</map>
                    </list> 
                    </fn> 
                    </request>  ')
  
  #make cite-ids
  qsplit = split(qry,qry$id)
  cids = ''
  for (i in 1:length(qsplit)) {
    top = paste0('<map name="id_',qsplit[[i]]$id[1],'">')
    body = paste0('<val name="',qsplit[[i]]$thing,'">',qsplit[[i]]$val,'</val>',collapse='')
    end = '</map>'
    cids = paste0(cids,top,body,end)
  }
  
  wokTree = paste(wokHeader,cids,wokFooter)
 
  h = basicTextGatherer()
  
  curlPerform(url=url,
              postfields=wokTree,
              writefunction = h$update,
              verbose = TRUE)
  res = xmlRoot(xmlTreeParse(h$value()))
  
  resList = getNodeSet(res,'/*["fn"]/*["map"]/*["map"]/*["map"]')
  
  out = lapply(resList, function(x) {
                               id = as.character(gsub('id_','',xmlAttrs(x)))
                               d = getNodeSet(x,'//*["map"]/*["map"]/*["val"]')
                               values = lapply(d,xmlValue)
                               names =lapply(d,xmlAttrs)
                               d = data.frame(id,values,stringsAsFactors=FALSE)
                               names(d) = c('id',unlist(names))
                               return(d)
                             })
  #lapply(xmlToList(res),function(x) d = rbind(x)
   #                   data.frame(rbind(x),stringsAsFactors = FALSE))
  
  return(rbind.fill(out))
}

}

#wokSearchLite:
# SID per session has to be included in http request header.
# searchLite handles one query per request

wokq = function(qry,nmatches= 50,first = 1, SID=getWokSID()) {
  
  d = wokGetRes(header = makeWokHeader(SID=SID),
            body = makeWokBody(qry=qry,count=nmatches,first=first)) 
  dd = wokParseResult(d, nodes = '//records')
  return(dd)
}
  
{

getWokSID = function(wokAuthUrl = 'http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate',
                     wokAuthXML = xmlRoot(xmlTreeParse('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:auth="http://auth.cxf.wokmws.thomsonreuters.com">
<soapenv:Header/>
                          <soapenv:Body>
                          <auth:authenticate/>
                          </soapenv:Body>
                          </soapenv:Envelope>'))) {
   
  SIDtree = xmlRoot(xmlTreeParse(postForm(wokAuthUrl,.opts=list("postfields" = saveXML(wokAuthXML)),style='POST')))
  return(xmlValue(SIDtree))
}

makeWokQry = function(fields,strings, validFields = c('AU','PY','SO','SU','TI','TS')) {
  fields = toupper(fields)
  strings = strings[which(fields%in%validFields)]
  fields = intersect(fields,validFields)
  if (length(fields) == 0 ) stop(paste('variable names must be one or more of',paste(validFields,collapse=',')))
  
  d = data.frame(field = fields, qry = strings)
  return(paste0(d$field,'=(',d$qry,')',collapse=' AND '))
}
  
  
  
makeWokBody = function(qry, count = 50, first =1,soapHead = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
<soapenv:Header/>
<soapenv:Body>
<woksearchlite:search>
<queryParameters>
<databaseId>WOS</databaseId>
<userQuery>',
                       soapMiddle = '</userQuery>
<queryLanguage>en</queryLanguage>
</queryParameters>
<retrieveParameters>', soapFoot = '</retrieveParameters>
</woksearchlite:search>
</soapenv:Body>
</soapenv:Envelope>') {
  
  nmatches = paste0('<firstRecord>',first,'</firstRecord>
                      <count>',count,'</count>')
  return(paste0(soapHead,qry,soapMiddle,nmatches,soapFoot))
  
  
}

makeWokHeader = function(SID = getWokSID()) {
  return(c(Encoding = 'UTF-8', Cookie = paste0('SID="',SID,'"'),
           'Content-Type' = "text/xml; charset=utf-8"))
}

wokGetRes = function(header,body,wokSliteURL = 'http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite') {
  h = basicTextGatherer()
  
    curlPerform(url=wokSliteURL,
              httpheader=header,
              postfields=body,
              writefunction = h$update,
              verbose = TRUE)
  
 res = xmlRoot(xmlTreeParse(h$value()))
 }

wokExtract = function(xmlthing,xpaththing) {
  lapply(getNodeSet(xmlthing,xpaththing),function(x) xmlValue(x))
}

wokParseResult = function(res, nodes){
  
  records = getNodeSet(res,nodes)
  
  rec = lapply(records, function(x) { 
                          y = xmlToList(x)
                          d = list()  
                          for (i in 1:length(y)) {
                            if (length(y[[i]])>1) { #nodes where variable name is in 'label' tag}
                              names(y[[i]]) = gsub('1','',c('label',paste0(y[[i]][1],c(1:(length(y[[i]])-1)))))
                              y[[i]] = y[[i]][-1]
                              d[[i]] = data.frame(do.call(cbind,y[[i]]),stringsAsFactors = FALSE)
                            } else d[i] = data.frame(cbind(y[i]),stringsAsFactors = FALSE) #variable name is tag name
                            if (length(names(d[[i]])) ==0) d[i]=NA
                          }
                          d = do.call(cbind,d)
                        })
  return(rbind.fill(rec))
  
}
   #rec = lapply(records, function(x) { titles    = wokExtract(x,'//title/value')
    #                                   sourceVal = as.vector(wokExtract(x,'//source/value'))
     #                                  sourceLab  =t(wokExtract(x,'//source/label'))
      #                                 authors   = wokExtract(x,'//authors/value')
       #                                authorLab = t(paste0('author_',c(1:length(authors))))
        #                               authorVal = as.vector(authors)
         #                              doiLoc    = which(grepl('10\\.',wokExtract(x,'//other/value')))
          #                             doi       = ifelse(length(doiLoc>0),wokExtract(x,'//other/value')[doiLoc],NA)
           #                            uid       = wokExtract(x,'//uid')
            #                           out       = data.frame(titles, sourceVal, authorVal,doi,uid,stringsAsFactors = FALSE)
             #                          names(out) = c('title',sourceLab,authorLab,'doi','wok_uid')
              #                         return(out)
               #                                                               })
 
  #return(rbind.fill(rec))
# }
}  
uidGet = function(uids,SID = getWokSID()) {
  soapHead = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"\nxmlns:woksearch=\"http://woksearchlite.v3.wokmws.thomsonreuters.com\">\n<soapenv:Header/>\n<soapenv:Body>\n<woksearch:retrieveById>\n<databaseId>WOS</databaseId>"
  qry = paste0("<uid>WOS:",uids,"</uid>")
  soapFoot = paste0("<queryLanguage>en</queryLanguage><retrieveParameters><firstRecord>1</firstRecord><count>",length(uids),"</count></retrieveParameters></woksearch:retrieveById></soapenv:Body></soapenv:Envelope>")
  body = paste0(soapHead, paste(qry,collapse = ''),soapFoot)
  d = wokGetRes(header = makeWokHeader(SID = SID), 
                body = body)
  dd = wokParseResult(d, nodes = "//records")
  return(dd)
}