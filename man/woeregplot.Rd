\name{woeregplot}
\alias{woeregplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the charts for regresion models
}
\description{
Plot charts for each vairable in keeplist,
including, relation chart, score chart, ROC curve chart
}
\usage{
woeregplot(Data, keeplist = NULL, y, model)
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
  \item{y}{
Name of the dependent Variables
}
  \item{model}{
Name of the model fitted by regression
}
}
\details{
Note: this is for credit modeling, and the most useful charts are relation chart, score chart and ROC curve Chart

}
\value{
test:
data is from the Titanic project https://www.kaggle.com/c/titanic/data
traindata <- read.csv('train.csv',header=T,na.strings=c(""))
Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
model<-woereg(Data=Data,keeplist=c('Fare','Pclass'),droplist=c('Sex','Embarked'),y='Survived')
library(magrittr)
library(ggplot2)
library(ROCR)
woeregplot(Data=Data,y='Survived',model=model)
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
function (Data, keeplist = NULL, y, model)
{
    if (is.null(keeplist)) {
        vallist <- coef(summary(model))[, 4]
        vallist <- vallist[!(names(vallist) \%in\% "(Intercept)")]
        keeplist <- c(names(vallist), y)
    }
    lscore <- predict(model, Data, type = "link")
    rscore <- predict(model, Data, type = "response")
    sdata <- data.frame(link = lscore, response = rscore, y = Data[,
        y], stringsAsFactors = FALSE)
    sdata \%>\% ggplot(aes(x = link, y = response, col = y)) +
        geom_point() + geom_rug() + ggtitle("link and response scores")
    rocp <- prediction(rscore, Data[, y])
    rocperf <- performance(rocp, "tpr", "fpr")
    plot(rocperf, colorize = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
