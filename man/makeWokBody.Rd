\name{makeWokBody}
\alias{makeWokBody}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeWokBody(qry, count = 50, first = 1, soapHead = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"\nxmlns:woksearchlite=\"http://woksearchlite.v3.wokmws.thomsonreuters.com\">\n<soapenv:Header/>\n<soapenv:Body>\n<woksearchlite:search>\n<queryParameters>\n<databaseId>WOS</databaseId>\n<userQuery>", soapMiddle = "</userQuery>\n<queryLanguage>en</queryLanguage>\n</queryParameters>\n<retrieveParameters>", soapFoot = "</retrieveParameters>\n</woksearchlite:search>\n</soapenv:Body>\n</soapenv:Envelope>")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qry}{
%%     ~~Describe \code{qry} here~~
}
  \item{count}{
%%     ~~Describe \code{count} here~~
}
  \item{first}{
%%     ~~Describe \code{first} here~~
}
  \item{soapHead}{
%%     ~~Describe \code{soapHead} here~~
}
  \item{soapMiddle}{
%%     ~~Describe \code{soapMiddle} here~~
}
  \item{soapFoot}{
%%     ~~Describe \code{soapFoot} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (qry, count = 50, first = 1, soapHead = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"\nxmlns:woksearchlite=\"http://woksearchlite.v3.wokmws.thomsonreuters.com\">\n<soapenv:Header/>\n<soapenv:Body>\n<woksearchlite:search>\n<queryParameters>\n<databaseId>WOS</databaseId>\n<userQuery>", 
    soapMiddle = "</userQuery>\n<queryLanguage>en</queryLanguage>\n</queryParameters>\n<retrieveParameters>", 
    soapFoot = "</retrieveParameters>\n</woksearchlite:search>\n</soapenv:Body>\n</soapenv:Envelope>") 
{
    nmatches = paste0("<firstRecord>", first, "</firstRecord>\n                      <count>", 
        count, "</count>")
    return(paste0(soapHead, qry, soapMiddle, nmatches, soapFoot))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
