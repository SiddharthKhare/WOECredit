#' ivdf
#'
#' @author
#' Yuan Yao
#' @title
#' Information Value for DataFrame
#' @description
#' calculate the information value for dataframe, with multiple binning methods
#' @name ivdf
#' @param Data data frame with at least two columns
#' @param keeplist : Name of the Independent Variables keept for capping, 
#' if missing then for all Independent Variables
#' @param y : Name of the dependent Variables 
#' @param bintype : there are three binning types: 
#' Bucket Binning ('bucket'), bucket binning creates equal-length bins and assigns the data to one of these bins.
#' Quantile Binning ('quantile'), quantile binning aims to assign the same number of observations to each bin, 
#' if the number of observations is evenly divisible by the number of bins. 
#' Optimal Binning ('opt'), optimal Binning aims to assign each bin with classified optimized results.
#' @param p : the minumume quantile for a binning interval, defult is 0.1
#' for 'bucket', the binning number would be 1/p, the default number is 10
#' for 'quantile', the binning number would be 1/p, the default number is 10
#' for 'opt', the binning number would be restults from Conditional Inference Trees
#'
#' NOTE: There are multiple R packages to calulcate IV, but there is no one for credit score model area. 
#' To writing this code, I have reviewed different R packages, including smbinning.R, woe.R, creat_iv.R, iv.R, mult.R
#' the output format from smbinning.R is very easy to understand, so I adopt the similar format of the output,
#' but smbinning.R is only for optmized binning (bins always <5 in real work), but sometimes we need quantile/bucket binning in credit models. 
#' Also smbinning.R would only work on single variable, the ivdf would work on the whole set of the variables
#' creat_iv.R is not suitable to work on dataframe. mult.R only provides IV seleclting, but you cannot change the binning size or binng method. 
#' The following code is improved based on 4 R packages related to binning and WOE area from others' previous work, great thanks to them.
#' 
#' test:
#' data is from the Titanic project https://www.kaggle.com/c/titanic/data
#' traindata <- read.csv('train.csv',header=T,na.strings=c(""))
#' Data <- subset(traindata,select=c(2,3,5,6,7,8,10,12))
#' library(partykit)
#' library(gsubfn)
#' library(sqldf)
#' binbyopt(Data,y='Survived',x='Age',p=0.1)
#' ivdf(Data,y='Survived')
#' ivdf(Data,y='Survived',bintype='quantile')


ivdf <-function(Data,keeplist=NULL,y, bintype='opt', p=NULL)
{
  #' select the only keep list from data frame
  if(is.null(keeplist)) {
    keeplist <- names(Data)[names(Data) != y]
  }
  #' IV for numerical variables
  nums <- sapply(Data, is.numeric)
  Data<-Data[ , nums]
  #' set the default p value
  if(is.null(p)) {
    p <- 0.1
  }
  #' IV calculation, first binning by one vairable
  binbyopt <- function(Data,y,x,p=p){
    # use the ctree for optimal binning
      ctree<-ctree(formula(paste(y,"~",x)), data=Data, na.action=na.exclude,control=ctree_control(minbucket=ceiling(round(p*nrow(Data)))))
      bins<-width(ctree)
      if (bins<2){return("bins < 2 ")}
      # get all the cutoff buckets
      bandlist<-data.frame(matrix(ncol=0,nrow=0))  
      n<-length(ctree) # Number of nodes
      for (i in 1:n) {
        bandlist<-rbind(bandlist,ctree[i]$node$split$breaks)
      }
      j<-which(names(df)==x) # Find Column for x
      mincutpoint<-min(Data[,x],na.rm=T) # Calculte Min cut point
      maxcutpoint<-max(Data[,x],na.rm=T) # Calculte Max cut point
      bandlist<-rbind(bandlist,maxcutpoint,mincutpoint)
      bandlist<-bandlist[order(bandlist[,1]),] # Sort / converts to a ordered vector (asc)
      bandlist<-ifelse(bandlist<0,trunc(10000*bandlist)/10000,ceiling(10000*bandlist)/10000) # Round to 4 dec. to avoid borderline cases
      # IV part 1
      ivt<-data.frame(matrix(ncol=0,nrow=0)) # Empty table
      n<-length(bandlist) # Number of cutpoits
      Total<- fn$sqldf("select count(*) from Data where $y is not null and $y in (0,1) ")
      G<- fn$sqldf("select count(*) from Data where $y is not null and $y=1 ")
      B<- fn$sqldf("select count(*) from Data where $y is not null and $y=0 ")
      LnGB=log(G/B)
      # for all non-missing values
      for (i in 2:n) {
        if (i==2) {cutpointlow=bandlist[i-1]-0.01} else {cutpointlow=bandlist[i-1]} # make sure this is smaller than the minimume
        cutpointhigh=bandlist[i]
        ivt=rbind(ivt,
                  fn$sqldf(
                    "select '<= $cutpointhigh' as Cutpoint,
                    sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntRec,
                    sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntGood,
                    sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntBad,
                    sum(case when $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntCumRec,
                    sum(case when $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntCumGood,
                    sum(case when $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntCumBad
                    from Data where $x is not NULL and $y is not NULL")
                  )
      }
      # for Missing Data
      x.na=fn$sqldf("select count(*) from Data where $x is null")  
      if(x.na>0){
        ivt=rbind(ivt,
                  fn$sqldf(
                    "select 'Missing' as Cutpoint,
                    sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                    sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                    sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                    sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                    sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                    sum(case when $y =0 then 1 else 0 end) as CntCumBad
                    from Data where $y is not NULL")
                  )
      } else {
        ivt=rbind(ivt,
                  fn$sqldf(
                    "select 'Missing' as Cutpoint,
                    0 as CntRec,
                    0 as CntGood,
                    0 as CntBad,
                    sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                    sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                    sum(case when $y =0 then 1 else 0 end) as CntCumBad
                    from Data where $y is not NULL"))
      }
      # calculate the IV and WOE
      options(scipen=999) # Remove Scientific Notation
      ivt$PctRec <- round(ivt$CntRec/Total[1,1],4)
      ivt$GoodRate <- round(ivt$CntGood/ivt$CntRec,4)
      ivt$BadRate <- round(ivt$CntBad/ivt$CntRec,4)
      ivt$Odds <- round(ivt$CntGood/ivt$CntBad,4)
      ivt$LnOdds <- round(log(ivt$CntGood/ivt$CntBad),4)
      ivt$WoE <- round(log(ivt$CntGood/ivt$CntBad)-LnGB[1,1],4)
      ivt$IV <- round(ivt$WoE*(ivt$CntGood/G[1,1]-ivt$CntBad/B[1,1]),4)
      totalwoe <- sum(ivt$WoE)
      iv<- 0
      for (k in 1:(nrow(ivt)))
      {
        if(is.finite(ivt[k,'IV'])) {i=ivt[k,'IV']} else {i=0.0000}
        iv=iv+i
      } 
      # the final list
      list(x=x,col_id=j, WOElist=ivt,iv=iv,ctree=ctree,bands=bandlist,missing=x.na[1,1])
  }
  binbyquantile <- function(Data,y,x,p=p){
    # use quantile for binning
    quantiles<- quantile( Data[is.na(Data[,x])==0,x], seq(0, 1, p)) 
    bins=length(quantiles)
    if (bins<2){return("bins < 2 ")}
    # get all the cutoff buckets
    bandlist=data.frame(matrix(ncol=0,nrow=0))  
    for (i in 1:bins) {
      bandlist=rbind(bandlist,quantiles[i])
    }
    bandlist=bandlist[order(bandlist[,1]),] # Sort / converts to a ordered vector (asc)
    bandlist=ifelse(bandlist<0,trunc(10000*bandlist)/10000,ceiling(10000*bandlist)/10000) # Round to 4 dec. to avoid borderline cases
    # IV part 1
    j=which(names(df)==x) # Find Column for x
    ivt=data.frame(matrix(ncol=0,nrow=0)) # Empty table
    n=length(bandlist) # Number of cutpoits
    Total<- fn$sqldf("select count(*) from Data where $y is not null and $y in (0,1) ")
    G<- fn$sqldf("select count(*) from Data where $y is not null and $y=1 ")
    B<- fn$sqldf("select count(*) from Data where $y is not null and $y=0 ")
    LnGB=log(G/B)
    # for all non-missing values
    for (i in 2:n) {
      if (i==2) {cutpointlow=bandlist[i-1]-0.01} else {cutpointlow=bandlist[i-1]} # make sure this is smaller than the minimume
      cutpointhigh=bandlist[i]
      ivt=rbind(ivt,
                fn$sqldf(
                  "select '<= $cutpointhigh' as Cutpoint,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntBad,
                  sum(case when $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntCumGood,
                  sum(case when $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntCumBad
                  from Data where $x is not NULL and $y is not NULL")
                )
    }
    # for Missing Data
    x.na=fn$sqldf("select count(*) from Data where $x is null")  
    if(x.na>0){
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                  sum(case when $y =0 then 1 else 0 end) as CntCumBad
                  from Data where $y is not NULL")
                )
    } else {
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  0 as CntRec,
                  0 as CntGood,
                  0 as CntBad,
                  sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                  sum(case when $y =0 then 1 else 0 end) as CntCumBad
                  from Data where $y is not NULL"))
    }
    # calculate the IV and WOE
    options(scipen=999) # Remove Scientific Notation
    ivt$PctRec <- round(ivt$CntRec/Total[1,1],4)
    ivt$GoodRate <- round(ivt$CntGood/ivt$CntRec,4)
    ivt$BadRate <- round(ivt$CntBad/ivt$CntRec,4)
    ivt$Odds <- round(ivt$CntGood/ivt$CntBad,4)
    ivt$LnOdds <- round(log(ivt$CntGood/ivt$CntBad),4)
    ivt$WoE <- round(log(ivt$CntGood/ivt$CntBad)-LnGB[1,1],4)
    ivt$IV <- round(ivt$WoE*(ivt$CntGood/G[1,1]-ivt$CntBad/B[1,1]),4)
    totalwoe <- sum(ivt$WoE)
    iv<- 0
    for (k in 1:(nrow(ivt)))
    {
      if(is.finite(ivt[k,'IV'])) {i=ivt[k,'IV']} else {i=0.0000}
      iv=iv+i
    }
    # the final list
    list(x=x,col_id=j,WOElist=ivt,iv=iv,bands=bandlist, missing=x.na[1,1])
    }  
  
  binbyfix <- function(Data,y,x,p=p){
    # use even length inteval for binning
    j=which(names(df)==x) # Find Column for x
    mincutpoint=min(Data[,x],na.rm=T) # Calculte Min cut point
    maxcutpoint=max(Data[,x],na.rm=T) # Calculte Max cut point
    fixstep=(maxcutpoint-mincutpoint)*p
    bandlist=seq(mincutpoint,maxcutpoint,fixstep)
    bandlist=ifelse(bandlist<0,trunc(10000*bandlist)/10000,ceiling(10000*bandlist)/10000) # Round to 4 dec. to avoid borderline cases
    # IV part 1
    ivt=data.frame(matrix(ncol=0,nrow=0)) # Empty table
    n=length(bandlist) # Number of cutpoits
    Total<- fn$sqldf("select count(*) from Data where $y is not null and $y in (0,1) ")
    G<- fn$sqldf("select count(*) from Data where $y is not null and $y=1 ")
    B<- fn$sqldf("select count(*) from Data where $y is not null and $y=0 ")
    LnGB=log(G/B)
    # for all non-missing values
    for (i in 2:n) {
      if (i==2) {cutpointlow=bandlist[i-1]-0.01} else {cutpointlow=bandlist[i-1]} # make sure this is smaller than the minimume
      cutpointhigh=bandlist[i]
      ivt=rbind(ivt,
                fn$sqldf(
                  "select '<= $cutpointhigh' as Cutpoint,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x > $cutpointlow and $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntBad,
                  sum(case when $x <= $cutpointhigh and $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $x <= $cutpointhigh and $y=1 then 1 else 0 end) as CntCumGood,
                  sum(case when $x <= $cutpointhigh and $y=0 then 1 else 0 end) as CntCumBad
                  from Data where $x is not NULL and $y is not NULL")
                )
    }
    # for Missing Data
    x.na=fn$sqldf("select count(*) from Data where $x is null")  
    if(x.na>0){
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                  sum(case when $y =0 then 1 else 0 end) as CntCumBad
                  from Data where $y is not NULL")
                )
    } else {
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  0 as CntRec,
                  0 as CntGood,
                  0 as CntBad,
                  sum(case when $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $y =1 then 1 else 0 end)  as CntCumGood,
                  sum(case when $y =0 then 1 else 0 end) as CntCumBad
                  from Data where $y is not NULL"))
    }
    # calculate the IV and WOE
    options(scipen=999) # Remove Scientific Notation
    ivt$PctRec <- round(ivt$CntRec/Total[1,1],4)
    ivt$GoodRate <- round(ivt$CntGood/ivt$CntRec,4)
    ivt$BadRate <- round(ivt$CntBad/ivt$CntRec,4)
    ivt$Odds <- round(ivt$CntGood/ivt$CntBad,4)
    ivt$LnOdds <- round(log(ivt$CntGood/ivt$CntBad),4)
    ivt$WoE <- round(log(ivt$CntGood/ivt$CntBad)-LnGB[1,1],4)
    ivt$IV <- round(ivt$WoE*(ivt$CntGood/G[1,1]-ivt$CntBad/B[1,1]),4)
    totalwoe <- sum(ivt$WoE)
    iv<- 0
    for (k in 1:(nrow(ivt)))
    {
      if(is.finite(ivt[k,'IV'])) {i=ivt[k,'IV']} else {i=0.0000}
      iv=iv+i
    }
    # the final list
    list(x=x,col_id=j,WOElist=ivt,iv=iv,bands=bandlist, missing=x.na[1,1])
    }  
  # test on keeplist<- c('MtgBal01','Bal01')
  ncol=length(keeplist) 
  if(bintype=='opt') {
    ivlist <- lapply(keeplist, function (x) {
      binbyopt(Data,y,x,p=p)
    }
    )  
  }
  if(bintype=='quantile') {
    ivlist <- lapply(keeplist, function (x) {
      binbyquantile(Data,y,x,p=p)
    }
    )  
  }
  if(bintype=='bucket') {
    ivlist <- lapply(keeplist, function (x) {
      binbyfix(Data,y,x,p=p)
    }
    )  
  }
  ivlist
}

