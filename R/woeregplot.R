#' woeregplot
#'
#' @author
#' Yuan Yao
#' @title
#' Plot the charts for regresion models
#' @description
#' Plot charts for each vairable in keeplist, 
#' including, relation chart, score chart, ROC curve chart
#' @name woeregplot
#' @param Data data frame with at least two columns
#' @param keeplist : Name of the Independent Variables keept for capping, 
#' if missing then for all Independent Variables
#' @param y : Name of the dependent Variables 
#' @param model : Name of the model fitted by regression
#' 
#' Note: this is for credit modeling, and the most useful charts are relation chart, score chart and ROC curve Chart
#' 
#' test:
#' data is from the Titanic project https://www.kaggle.com/c/titanic/data
#' traindata <- read.csv('train.csv',header=T,na.strings=c(""))
#' Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
#' model<-woereg(Data=Data,keeplist=c('Fare','Pclass'),droplist=c('Sex','Embarked'),y='Survived')
#' library(magrittr)
#' library(ggplot2)
#' library(ROCR)
#' woeregplot(Data=Data,y='Survived',model=model)


woeregplot <-function(Data,keeplist=NULL,y,model)
{
  #' check if the keeplist variable is null or not
  if(is.null(keeplist)) {
    #' get the coeficient
    vallist<- coef(summary(model))[,4]
    #' remove intercept
    vallist<- vallist[ !(names(vallist) %in% '(Intercept)')]
    #' finalize the keeplist
    keeplist<- c(names(vallist),y)
  }
  #########################
  #' plot the link to response chart
  lscore <- predict(model, Data, type="link")
  rscore <- predict(model, Data, type="response")
  sdata <- data.frame(link=lscore,response=rscore,y=Data[,y],stringsAsFactors=FALSE)
  #' plot the chart
  sdata %>% ggplot(aes(x=link, y=response, col=y)) + geom_point() + geom_rug() + 
    ggtitle("link and response scores")
  #########################
  #' plot the ROC curve
  rocp <- prediction(rscore, Data[,y])
  rocperf <- performance(rocp, 'tpr','fpr')
  plot(rocperf, colorize = TRUE)
}
