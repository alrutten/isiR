\name{wokParseResult}
\alias{wokParseResult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wokParseResult(res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
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
function (res) 
{
    records = getNodeSet(res, "//records")
    rec = lapply(records, function(x) {
        titles = wokExtract(x, "//title/value")
        sourceVal = t(wokExtract(x, "//source/value"))
        sourceLab = t(wokExtract(x, "//source/label"))
        authors = wokExtract(x, "//authors/value")
        authorLab = t(paste0("author_", c(1:length(authors))))
        authorVal = t(authors)
        doiLoc = which(grepl("10\\.", wokExtract(x, "//other/value")))
        doi = ifelse(length(doiLoc > 0), wokExtract(x, "//other/value")[doiLoc], 
            NA)
        uid = wokExtract(x, "//uid")
        out = data.frame(titles, sourceVal, authorVal, doi, uid)
        names(out) = c("title", sourceLab, authorLab, "doi", 
            "wok_uid")
        return(out)
    })
    return(do.call(rbind.fill, rec))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
