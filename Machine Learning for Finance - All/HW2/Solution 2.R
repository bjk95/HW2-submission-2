setwd("C:/Users/User/Dropbox/University Studings/ML for FInance/Assignments/HW2")
library(xts); library(quantmod)

##load GoyalWelch data table
sp500 = as.xts(read.zoo('GoyalMonthly2005.csv',sep=',',header=TRUE, format='%Y-%m-%d'))
names(sp500)
mt=sp500['1927/2005'] ##from 1927 ntis starts
#[1927/1932], [1933/1970], [1971/1997], [1998/2005].

# S&P500 log equity premium (GSPCep)
IndexDiv = mt$Index + mt$D12
logretdiv <- log(IndexDiv) - log(mt$Index)
logRfree = log(mt$Rfree + 1)
GSPCep <- logretdiv - logRfree
rm(IndexDiv,logretdiv,logRfree)

#Predictors
dfy <- mt$BAA -mt$AAA
bm <-mt$b.m
names(GSPCep) = "GSPCep"; names(bm) = "bm"; names(dfy) ="dfy"   

# Merging and cleaning
mt <- merge(GSPCep,dfy,bm)
rm(bm,dfy,GSPCep,sp500)

## QUESTION 1
#Casuality test
library(vars)
library(lmtest)

tbl_func<-function(ts,ep,ind=0,contamp=0){
  if(contamp==0){
    res<-lapply(ep,function(t){grang_test(ts,t,ind)})
    len<-unlist(lapply(res,function(t){length(t)}))
    for(i in 1:4){
      res[[i]]<-c(res[[i]],rep(NA,max(len)-len[i]) )
    }
    res<-matrix(unlist(res),ncol = 9,nrow = 4,byrow = TRUE)
    row.names(res)<-ep
    colnames(res)<-paste("lag",1:9,sep = "")
    return(res)
  }else{
    res<-lapply(ep,function(t){grang_test_contamp(mt,t,ind)})
    len<-unlist(lapply(res,function(t){length(t)}))
    for(i in 1:4){
      res[[i]]<-c(res[[i]],rep(NA,max(len)-len[i]) )
    }
    res<-matrix(unlist(res),ncol = 9,nrow = 4,byrow = TRUE)
    row.names(res)<-ep
    colnames(res)<-paste("lag",1:9,sep = "")
    return(res)
  }
  
}

grang_test<-function(ts,ep,ind=0){
  p <- VARselect(ts[ep]$GSPCep,lag.max=30,type="const")$selection["HQ(n)"]
  if(ind==0){
    casual<- lapply(1:p,function(h){round( grangertest(GSPCep~dfy,data=ts[ep],order = h)$`Pr(>F)`[2],digits = 4)})
    casual<-unlist(casual,use.names = FALSE)
  }else{
    casual<- lapply(1:p,function(h){round( grangertest(GSPCep~bm,data=ts[ep],order = h)$`Pr(>F)`[2],digits = 4)})
    casual<-unlist(casual,use.names = FALSE)
  }
  
  return(casual)
}

grang_test_contamp<-function(ts,ep,ind=0){
  p <- VARselect(ts[ep]$GSPCep,lag.max=30,type="const")$selection["HQ(n)"]
  if(ind==0){
    casual<- lapply(1:p,function(h){round( grangertest(dfy~GSPCep,data=ts[ep],order = h)$`Pr(>F)`[2],digits = 4)})
    casual<-unlist(casual,use.names = FALSE)
  }else{
    casual<- lapply(1:p,function(h){round( grangertest(bm~GSPCep,data=ts[ep],order = h)$`Pr(>F)`[2],digits = 4)})
    casual<-unlist(casual,use.names = FALSE)
  }
  
  return(casual)
}

#Influence of dfy on GSPCep. NA - is produced for lags which are not optimal, but we need to put something to produce table.
tbl_func(mt,c('1927/1932', '1933/1970', '1971/1997', '1998/2005'))

#Influence of GSPCep on dfy. 
tbl_func(mt,c('1927/1932', '1933/1970', '1971/1997', '1998/2005'),contamp = 1)

#Comparing two tables we can figure out "true" and contamporaneous casuality. 

#Influence of bm on GSPCep.
tbl_func(mt,c('1927/1932', '1933/1970', '1971/1997', '1998/2005'),ind = 1)

#Influence of GSPCep on bm.
tbl_func(mt,c('1927/1932', '1933/1970', '1971/1997', '1998/2005'),ind = 1,contamp = 1)



## QUESTION 2

#Preparing Data
data <- readRDS("WorldMarkts99_17.RDS")
markets <- ls(data)
returns <- xts()
per<- "daily" 
for(i in seq_along(markets)) {
  sym <- markets[i]
  returns <- merge(returns, 
                   periodReturn(Ad(get(sym,envir=data)),period=per,type = "log"))
}
returns <-returns[-1,] ##remove first row == 0
returns[is.na(returns)]<-0
colnames(returns) <-markets
rm(sym,i,markets,per,data)
returns<-returns['2004/2006']

casual_test<-function(ts){

  nms<- names(ts)
  fr<-array(list(), dim=c(11,11))
  for(i in 1:11){
    res<-list()
    for(j in 1:11 ){
      
      if(i!=j){
        casual<- lapply(1:4,function(t){round( grangertest(ts[,j],ts[,i],order = t)$`Pr(>F)`[2],digits = 4)})
        casual<-unlist(casual,use.names = FALSE)
        casual<-(casual<0.05)*1
        fr[[j,i]]<-casual
      }else{
        fr[[j,i]]<-c(9,9,9,9)
      }
    }

  }
  colnames(fr)<-nms
  row.names(fr)<-nms
  return(fr)
}

vol_func<-function(ts){
  vol<-c()
  for(i in 1:length(ts)){
    if(i==1)vol[i]<-(ts[i])^2 else vol[i]<- sum(ts[1:i]^2)
  }
  return(vol)
}

vol_wrapper<-function(ts){
  
  res<-matrix( rep(999,nrow(ts)*ncol(ts)),nrow=nrow(ts),ncol=ncol(ts) ) 
  
  for(i in 1:ncol(ts)){
    if(i==1) res[,i]<-vol_func(ts[,i]) else res[,i]<-vol_func(ts[,i])
  }
  colnames(res)<-names(ts)
  return(res)
}

# Casuality for logReturns and Volatility
ret<-casual_test(returns)
vol<-casual_test(vol_wrapper(returns))


# QUESTION 3

#Preparing Data
data <- readRDS("WorldMarkts99_17.RDS")
markets <- ls(data)
returns <- xts()
per<- "daily" 
for(i in seq_along(markets)) {
  sym <- markets[i]
  returns <- merge(returns, 
                   periodReturn(Ad(get(sym,envir=data)),period=per,type = "log"))
}
returns <-returns[-1,] ##remove first row == 0
returns[is.na(returns)]<-0
colnames(returns) <-markets
rm(sym,i,markets,per,data)
returns<-returns['2004/2006']

clustering<-function(rtn){
  dist <-as.dist(2*(1-cor(rtn,method = "kendall")))
  cltr <-hclust(dist,method="ward.D2")
  return(cltr)
}

cutoff_plot<-function(cltr){
  plot(cltr,main="Hierachial Clustering",axes=TRUE,xlab="",sub="")
  hh<-sort(cltr$height)
  K<-mean(hh[1:length(cltr$height)])
  abline(h=K,lty=2,lwd=2)
  
  groups <- cutree(cltr, h = K)
  return(groups)
}


cluster_wrapper<-function(rtn){
  hc<-clustering(rtn)
  cutoff_plot(hc)
}

tab_func<-function(ts){
  par(mfrow=c(2,3))
  res<-lapply(c("200406","200407/200412","200501/200506","200507/200512","200601/200606","200607/200612"),function(t){cluster_wrapper(ts[t])}) 
  for(i in 1:length(res)){
    if(i==1)fr<-res[[i]] else fr<-rbind(fr,res[[i]]) 
  }
  rownames(fr)<-c("2004-01/2004-06","2004-07/2004-12","2005-01/2005-06","2005-07/2005-12","2006-01/2006-06","2006-07/2006-12")
  par(mfrow=c(1,1))
  return(fr)
}

tab_func(returns)

