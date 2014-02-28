\name{postLAMRReq}
\alias{postLAMRReq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
postLAMRReq(qry, url = "https://ws.isiknowledge.com/cps/xrpc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qry}{
%%     ~~Describe \code{qry} here~~
}
  \item{url}{
%%     ~~Describe \code{url} here~~
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
function (qry, url = "https://ws.isiknowledge.com/cps/xrpc") 
{
    wokHeader = paste("<?xml version=\"1.0\" encoding=\"UTF-8\" ?> \n                    <request xmlns=\"http://www.isinet.com/xrpc42\" >\n                    <fn name=\"LinksAMR.retrieve\"> \n                    <list> \n                    <map> \n                    <!-- our validation should go via IP -->  \n                    </map> \n                    <map> \n                    <list name=\"WOS\"> \n                    <val>timesCited</val>\n                    <val>ut</val> \n                    <val>doi</val> \n                    <val>sourceURL</val> \n                    <val>citingArticlesURL</val> \n                    <val>relatedRecordsURL</val> \n                    </list> \n                    </map> \n                    <map> ")
    wokFooter = paste("</map>\n                    </list> \n                    </fn> \n                    </request>  ")
    qsplit = split(qry, qry$id)
    cids = ""
    for (i in 1:length(qsplit)) {
        top = paste0("<map name=\"id_", i, "\">")
        body = paste0("<val name=\"", qsplit[[i]]$thing, "\">", 
            qsplit[[i]]$val, "</val>", collapse = "")
        end = "</map>"
        cids = paste0(cids, top, body, end)
    }
    wokTree = xmlRoot(xmlTreeParse(paste(wokHeader, cids, wokFooter)))
    return(xmlTreeParse(postForm(url, .opts = list(postfields = saveXML(wokTree)), 
        style = "POST")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
