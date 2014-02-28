\name{makeWokQry}
\alias{makeWokQry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeWokQry(fields, strings, validFields = c("AU", "PY", "SO", "SU", "TI", "TS"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fields}{
%%     ~~Describe \code{fields} here~~
}
  \item{strings}{
%%     ~~Describe \code{strings} here~~
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
function (fields, strings, validFields = c("AU", "PY", "SO", 
    "SU", "TI", "TS")) 
{
    fields = toupper(fields)
    strings = strings[which(fields \%in\% validFields)]
    fields = intersect(fields, validFields)
    if (length(fields) == 0) 
        stop(paste("variable names must be one or more of", paste(validFields, 
            collapse = ",")))
    d = data.frame(field = fields, qry = strings)
    return(paste0(d$field, "=(", d$qry, ")", collapse = " AND "))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
