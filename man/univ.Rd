\name{univ}
\alias{univ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
univariate calcualted for data framework
}
\description{
univariate calculation for all independent variables in a data framework
mean, median, variance, std, missing rate, unique rate
}
\usage{
univ(Data, keeplist = NULL, intmiss = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
data frame with at least two columns
}
  \item{keeplist}{
Name of the Independent Variables keept for capping,
if missing then for all Independent Variables
}
  \item{intmiss}{
automatically fill missing values. defuat for numarical vairable is NA, but can be 0
(there would be multiple missing types of a model)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
test:
data is from the Titanic project https://www.kaggle.com/c/titanic/data
traindata <- read.csv('train.csv',header=T,na.strings=c(""))
Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
univ(Data)
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
function (Data, keeplist = NULL, intmiss = NULL)
{
    if (!is.null(keeplist)) {
        Data <- Data[, keeplist]
    }
    if (is.null(intmiss)) {
        intmiss <- 0
    }
    nums <- sapply(Data, is.numeric)
    Data <- Data[, nums]
    varlist <- sapply(Data, function(x) {
        data.frame(mean = mean(x), median = median(x), var = var(x),
            sd = sd(x), nmiss = sum(is.na(x)), n = length(x),
            missrate = sum(is.na(x))/length(x))
    })
    transfvar <- data.frame(t(varlist))
    nums <- sapply(Data, is.numeric)
    filldata <- Data[, nums]
    filldata[is.na(filldata)] <- intmiss
    Data <- filldata
    return(transfvar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
