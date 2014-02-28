\name{makeLAMRQry}
\alias{makeLAMRQry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeLAMRQry(d, validFields = c("atitle", "stitle", "year"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{validFields}{
%%     ~~Describe \code{validFields} here~~
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
function (d, validFields = c("atitle", "stitle", "year")) 
{
    vars = intersect(names(d), validFields)
    if (length(vars) == 0) 
        stop(paste("variable names must be one or more of", paste(validFields, 
            collapse = ",")))
    d = d[, vars]
    d$id = row.names(d)
    qry = reshape(d, varying = c(vars), v.names = "val", timevar = "thing", 
        times = c(vars), direction = "long")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
