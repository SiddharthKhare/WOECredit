#' univariate
#'
#' @author
#' Yuan Yao
#' @title
#' univariate calcualted for data framework
#' @description
#' univariate calculation for all independent variables in a data framework
#' mean, median, variance, std, missing rate, unique rate
#' @name univ
#' @param Data data frame with at least two columns
#' @param keeplist : Name of the Independent Variables keept for capping, 
#' if missing then for all Independent Variables
#' @param intmiss : automatically fill missing values. defuat for numarical vairable is NA, but can be 0
#' (there would be multiple missing types of a model)
#' 
#' 
#' test:
#' data is from the Titanic project https://www.kaggle.com/c/titanic/data
#' traindata <- read.csv('train.csv',header=T,na.strings=c(""))
#' Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
#' univ(Data)

univ <-function(Data,keeplist=NULL,intmiss=NULL)
{
  #' select the only keep list from data frame
  if(!is.null(keeplist)) {
    Data <- Data[,keeplist]
  }
  if(is.null(intmiss)) {
    intmiss <- 0
  }

  
  #' univ for numerical variables
  nums <- sapply(Data, is.numeric)
  Data<-Data[ , nums]
  
  varlist <- sapply(Data, function(x) {
    data.frame(
      mean = mean(x),
      median = median(x),
      var = var(x), 
      sd = sd(x),
      nmiss=sum(is.na(x)), 
      n=length(x), 
      missrate=sum(is.na(x))/length(x)
    )
  })
  #' get the univariate report
  transfvar <- data.frame(t(varlist))
  #' fill the missing values

  nums <- sapply(Data, is.numeric)
  filldata<- Data[,nums]
  filldata[is.na(filldata)] <- intmiss
  Data<-filldata
  return(transfvar)
}
