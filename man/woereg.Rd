\name{woereg}
\alias{woereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
regressions and classifications for data frame in credit firld
}
\description{
apply logistical regression and trees to credit related data frame, to calcualte the credit score
}
\usage{
woereg(Data, keeplist = NULL, droplist = NULL, y, regtype = "logit", p_value = NULL, n = NULL)
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
  \item{droplist}{
Name of the Independent Variables keept for capping,
if missing then for all Independent Variables
}
  \item{y}{
Name of the dependent Variables
}
  \item{regtype}{
there are mutilpe regresiion types:
logistical regression, with regtype='logit', currently available, which is default
linear regression, with regtype='logit', currently under building
multinomial logistic regression, with regtype='multi', currently under building
ordinal logistic regression, with regtype='ordinal', currently under building
}
  \item{p_value}{
Name of the dependent Variables
the p-value cutoff, would be value between 0-1, for example 0.01, 0.05, the default value is 0.01
}
  \item{n}{
number of iterations
the number of logistical iterations would run, default 3 (later will based on opt p_value)
}
}
\details{
Note: I used glm.R to train mulptple iterations, since every time we use GLM in logistic regression to build credit models,
there would be multiple training iterations and it's very hard to do variable keeping/droping each time.
Later, I will have more regression types added into this interface, by using param regtype

}
\value{
test:(currectly only works for numarical variables, since the charactor variable names changed in GLM coeficient)
data is from the Titanic project https://www.kaggle.com/c/titanic/data
traindata <- read.csv('train.csv',header=T,na.strings=c(""))
Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
keeplist= c('Fare','Pclass')
droplist= c('Sex','Embarked')
woereg(Data=Data,keeplist=c('Fare','Pclass'),droplist=c('Sex','Embarked'),y='Survived', regtype='logit', p_value= 0.01, n=3)


results:
             Estimate Std. Error z value Pr(>|z|)
             (Intercept)  3.188881   0.488922   6.522 6.93e-11 ***
             Pclass      -1.130074   0.142036  -7.956 1.77e-15 ***
             Age         -0.040656   0.006782  -5.994 2.04e-09 ***
             Fare         0.003213   0.002326   1.381    0.167
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
function (Data, keeplist = NULL, droplist = NULL, y, regtype = "logit",
    p_value = NULL, n = NULL)
{
    if (!is.null(droplist)) {
        Data <- Data[, !(names(Data) \%in\% droplist)]
    }
    nums <- sapply(Data, is.numeric)
    Data <- Data[, nums]
    if (is.null(p_value)) {
        p_value <- 0.01
    }
    if (is.null(n)) {
        n <- 3
    }
    for (i in 1:n) {
        f <- paste(as.name(y), "~.")
        model <- glm(f, family = binomial(link = "logit"), data = Data)
        pvalue <- round(coef(summary(model))[, 4], 6)
        varsel <- pvalue[which(pvalue <= p_value)]
        drops <- c(droplist, "(Intercept)")
        vallist <- varsel[!(names(varsel) \%in\% drops)]
        varselname <- unique(c(names(vallist), keeplist))
        if (!is.null(varselname)) {
            Data <- subset(Data, select = c(y, varselname))
        }
    }
    summary(model)
    return(model)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
