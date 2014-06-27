\name{wokq}
\alias{wokq}
\alias{isiR}
\alias{postLAMRReq}
\alias{makeLAMRQry}
\alias{uidGet}

\title{
query web of knowledge
}
\description{
query web of knowledge using searchLite (to do a keyword search) or RetrieveByID to find references with known web of knowledge ID, or to retrieve metadata for certain references (using LAMR, link article match retrieval)
}
\usage{
wokq(qry, nmatches = 50, first = 1, SID = getWokSID())
postLAMRReq(qry = makeLAMRQry(d))
uidGet(uids,SID = getWokSID())
}

\arguments{
  \item{qry (wokq)}{
wokq: a querystring as requested by web of knowledge. Valid fields are AU,PY,SO,SU,TI,TS,DO for author, publication year, source, subject, title, topic, and doi respectively. See web of knowledge documentation for all operators and options available. example string: 'TS = "extra-pair". uses wokSearchLite soap service.
}
\item{uids (uidGet)}{
uids: uid -Web of knowledge unique identifier- to retrieve. Can also handle vector of uids. Uses retrieveById soap service. currently only retrieves uid's from WOS database.
}
  \item{qry (LAMR)}{
use makeLAMRQry(d) to transform a dataframe d with a row for each reference you want to have matched into something postLAMRReq() understands. current valid fields for your dataframe are atitle, stitle, and year for article title, source title and publication year respectively. Other variables in your dataframe are ignored. You have to provide at least atitle and stitle to get any results.}
  \item{nmatches}{
number of matches to retrieve, starting from the number entered for 'first'
}
  \item{first}{
first matchnumber to retrieve.
}
  \item{SID}{
sessionID for web of knowledge. If you intend to do more than one query, get a sessionID first: SID = getWokSID() and pass this to wokq. There's a maximum on the number of sessionIDs we can get per day.
}
}
\details{
}
\value{
\item{wokq }{a dataframe with bibliographical data for each match}
\item{postLAMRReq }{a dataframe with the metadata for each query. note that LAMR returns query results out of the original order, if you post multiple queries at once. Use the id variable to trace results back to the original query.}

}
\author{
AR
}
