\name{getWokSID}
\alias{getWokSID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getWokSID(wokAuthUrl = "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate", wokAuthXML = xmlRoot(xmlTreeParse("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"\nxmlns:auth=\"http://auth.cxf.wokmws.thomsonreuters.com\">\n<soapenv:Header/>\n                          <soapenv:Body>\n                          <auth:authenticate/>\n                          </soapenv:Body>\n                          </soapenv:Envelope>")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wokAuthUrl}{
%%     ~~Describe \code{wokAuthUrl} here~~
}
  \item{wokAuthXML}{
%%     ~~Describe \code{wokAuthXML} here~~
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
function (wokAuthUrl = "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate", 
    wokAuthXML = xmlRoot(xmlTreeParse("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"\nxmlns:auth=\"http://auth.cxf.wokmws.thomsonreuters.com\">\n<soapenv:Header/>\n                          <soapenv:Body>\n                          <auth:authenticate/>\n                          </soapenv:Body>\n                          </soapenv:Envelope>"))) 
{
    SIDtree = xmlRoot(xmlTreeParse(postForm(wokAuthUrl, .opts = list(postfields = saveXML(wokAuthXML)), 
        style = "POST")))
    return(xmlValue(SIDtree))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
