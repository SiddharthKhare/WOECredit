#' capping
#'
#' @author
#' Yuan Yao
#' @title
#' Outliers Cappying
#' @description
#' Capping inputs by diferent level for numarical variables
#' there are two types of capping, one is by quantile, the other is by standard deviation
#' @name capping
#' @param Data data frame with at least two columns
#' @param keeplist : Name of the Independent Variables keept for capping,
#' if missing then for all Independent Variables
#' @param y : Name of the dependent variable
#' @param level : by the level of quantile/standard deviation to cap, would be any numerical value
#' captype='quant', could be +-1 or +-5, defaut +-5
#' captype='std', could be +-3 or +-5, defaut +-3
#' @param captype : two captype, by quantile 'quant', or by standard deviation 'std'.
#' captype='quant', means the capping low/high bound would be low/high quantile level
#' captype='std', means the capping low/high bound would be low/high standard deviation level
#'
#' test:
#' data is from the Titanic project https://www.kaggle.com/c/titanic/data
#' traindata <- read.csv('train.csv',header=T,na.strings=c(""))
#' Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
#' capping(Data,  y='Survived')



capping <-function(Data,keeplist=NULL,y,level=NULL,captype='quant')
{
#' check if the keeplist variable is null or not
  if(is.null(keeplist)) {
    keeplist <- names(Data)[names(Data) != y]
  }
  if(is.null(level)) {
    if(captype=='quant') {
      level <- 5
    }
    if(captype=='std'){
      level <- 3
    }
  }
  if(captype=='quant') {
    varlist <- lapply(keeplist, function (x) {
    quantiles <- quantile( Data[,x], c(level/100, (100-level)/100 ) )
    Data[,x][ Data[,x] < quantiles[1] ] <- quantiles[1]
    Data[,x][ Data[,x] > quantiles[2] ] <- quantiles[2]
    Data[,x]
    }
    )
  }
  if(captype=='std') {
  varlist <- lapply(keeplist, function (x) {
    stdbond <-   c(mean(Data[,x])- level*sd(Data[,x]),mean(Data[,x])+ level*sd(Data[,x]))
    Data[,x][ Data[,x] < stdbond[1] ] <- stdbond[1]
    Data[,x][ Data[,x] > stdbond[2] ] <- stdbond[2]
    Data[,x]
    }
    )
  }
  return(varlist)
}
