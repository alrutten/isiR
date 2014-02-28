\name{wokq}
\alias{wokq}
\alias{isiR}
\alias{postLAMRReq}
\alias(makeLAMRQry}

\title{
query web of knowledge
}
\description{
query web of knowledge using searchLite (to do a keyword search) or to retrieve metadata for certain references (using LAMR, link article match retrieval)
}
\usage{
wokq(qry, nmatches = 50, first = 1, SID = getWokSID())
postLAMRReq(qry = makeLAMRQry(d))
}

\arguments{
  \item{qry (wokq)}{
wokq: a querystring as requested by web of knowledge. Valid fields are AU,PY,SO,SU,TI,TS for author, publication year, source, subject, title, and topic respectively. See web of knowledge documentation for all operators and options available. example string: 'TS = "extra-pair"
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
\item{postLAMRReq }{currently an XMLtree with the metadata for each match, but there will be a parser for this too}

}
\author{
AR
}
