\name{capping}
\alias{capping}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outliers Cappying
}
\description{
Capping inputs by diferent level for numarical variables
there are two types of capping, one is by quantile, the other is by standard deviation
}
\usage{
capping(Data, keeplist = NULL, y, level = NULL, captype = "quant")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
data frame with at least two columns
}
  \item{keeplist}{
 Name of the Independent Variables keept for capping, if missing then for all Independent Variables
}
  \item{y}{
Name of the dependent variable
}
  \item{level}{
by the level of quantile/standard deviation to cap, would be any numerical value
captype='quant', could be +-1 or +-5, defaut +-5
captype='std', could be +-3 or +-5, defaut +-3
}
  \item{captype}{
 two captype, by quantile 'quant', or by standard deviation 'std'.
captype='quant', means the capping low/high bound would be low/high quantile level
captype='std', means the capping low/high bound would be low/high standard deviation level
}
}
\details{
test:
data is from the Titanic project https://www.kaggle.com/c/titanic/data
traindata <- read.csv('train.csv',header=T,na.strings=c(""))
Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
capping(Data,  y='Survived')
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
Yuan Yao
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
function (Data, keeplist = NULL, y, level = NULL, captype = "quant")
{
    if (is.null(keeplist)) {
        keeplist <- names(Data)[names(Data) != y]
    }
    if (is.null(level)) {
        if (captype == "quant") {
            level <- 5
        }
        if (captype == "std") {
            level <- 3
        }
    }
    if (captype == "quant") {
        varlist <- lapply(keeplist, function(x) {
            quantiles <- quantile(Data[, x], c(level/100, (100 -
                level)/100))
            Data[, x][Data[, x] < quantiles[1]] <- quantiles[1]
            Data[, x][Data[, x] > quantiles[2]] <- quantiles[2]
            Data[, x]
        })
    }
    if (captype == "std") {
        varlist <- lapply(keeplist, function(x) {
            stdbond <- c(mean(Data[, x]) - level * sd(Data[,
                x]), mean(Data[, x]) + level * sd(Data[, x]))
            Data[, x][Data[, x] < stdbond[1]] <- stdbond[1]
            Data[, x][Data[, x] > stdbond[2]] <- stdbond[2]
            Data[, x]
        })
    }
    return(varlist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
