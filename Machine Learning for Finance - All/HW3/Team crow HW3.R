#Team Crow - Simon Neymeyer & Bradley Kester#

#Assignment 3#

library(e1071) ##for svm
library(neuralnet)
library(caret)  
library(Metrics) 
library(xts)

require(PortfolioAnalytics)
require(GenSA)
require(quantmod)
require(plyr)


###### PROBLEM 1 ######
#Frequency of sampling
tau=1 #data is daily. tau=20 (month), tau=60 (quarterly)

#Importing data
x <- read.csv('data/NFLX.csv', sep = ",", header = TRUE)
#remove the char columns to get all numeric
x <- x[,-(2:3)]
##use Date as index
data <- as.xts(zoo(as.matrix(x[,-1]), as.Date(as.character(x[,1]))))

## Target :  Adj.Close Price (or return?)
target <- data$Adj.Close
#target=diff(log(data$Adj.Close),diff=tau)  ##compute tau-period returns
#target=na.trim(target-mean(na.omit(target)))

##Features: 1,2,3-lags of Sentiment indices. Here shown how to one set:
##Bullish set
#positive = na.omit(data$positivescore) ##feature:positivescore sentiment index


######## BEARISH INDICATORS #########


##Bearish set
negative = na.omit(data$negativescore) 
negativeP= na.omit(data$negativePartscr)
uncertainty= na.omit(data$uncertaintyscore)
uncertaintyP= na.omit(data$uncertaintyPartscr)
findown= na.omit(data$findownscore)
findownP= na.omit(data$findownPartscr)

bearish <- merge(na.trim(lag(negative,1)),na.trim(lag(negative,2)),na.trim(lag(negative,3)),
                 na.trim(lag(negativeP,1)),na.trim(lag(negativeP,2)),na.trim(lag(negativeP,3)),
                 na.trim(lag(uncertainty,1)),na.trim(lag(uncertainty,2)),na.trim(lag(uncertainty,3)),
                 na.trim(lag(uncertaintyP,1)),na.trim(lag(uncertaintyP,2)),na.trim(lag(uncertaintyP,3)),
                 na.trim(lag(findown,1)),na.trim(lag(findown,2)),na.trim(lag(findown,3)),
                 na.trim(lag(findownP,1)),na.trim(lag(findownP,2)),na.trim(lag(findownP,3)),
                 all=FALSE)

bearnames <- c("Neg.1","Neg.2","Neg.3",
               "NegP.1","NegP.2","NegP.3",
               "uncer.1","uncer.2","uncer.3",
               "uncerP.1","uncerP.2","uncerP.3",
               "find.1","find.2","find.3",
               "findP.1","findP.2","findP.3")

##Model Inputs:
##Define matrix of features (each column is a feature)
#Features: lags 1,2,3 of target with (or w/o)  lags 1,2,3 of sentiments
feat = merge(na.trim(lag(target,1)),na.trim(lag(target,2)),na.trim(lag(target,3)),
             bearish,
             #add other features here,
             all=FALSE)

##add TARGET. We want to predict RETURN with and without Sentiment indices
##for without test comment line for lags of sentiment (above and in names below)
datasetALL = merge(feat,target,all=FALSE)
colnames(datasetALL) = c("lag.1", "lag.2", "lag.3",
                         bearnames,
                         #names of other features,
                         "TARGET")

epoc1<-'2017-01/2017-06'
epoc2<-'2017-07/2017-12'

##Analise each epoch
dataset<-datasetALL[epoc1] ##rerun with epoc2

##Divide data into training (75%) and testing (25%). Use caret methods
## caret randomizes data 
index = 1:nrow(dataset)
trainindex= createDataPartition(index,p=0.75,list=FALSE)
##process class sets as data frames
training = as.data.frame(dataset[trainindex,])
rownames(training) = NULL
testing = as.data.frame(dataset[-trainindex,])
rownames(testing) = NULL


##MODELS: OPTION LAZY: one svm, one nnet built w/o tuning  (or tune by hand)
type="eps-regression" ##regression
#parameters that can be tuned: gamma & cost
u= -2 ## -3,-2,-1,0,1,2,3
w= 4.5 ##1.5,-1,0.5,2,3,4
gam=10^{u}; cost=10^{w}
##The higher the cost produce less support vectors, increases accuracy
##However we may overfit
##svm(features,target,type, ...)
svmFit = svm (training[,-ncol(training)], training[,ncol(training)],
              type=type,
              kernel= "radial",
              gamma=gam,
              cost=cost
)
summary(svmFit)
##build predictor
predsvm = predict(svmFit, testing[,-ncol(testing)])

##Fit a  2-hidden ffnet: 
##input layer=ncol(dataset)-1; output=1. We apply rule
##The number of hidden neurons should be 2/3 the size of the input layer, plus the size of the output layer
#h1=ceiling(ncol(dataset)/2)+1; h2=ceiling(ncol(dataset)/2)-1
#xtra=0; ## xtra=3 ##an amplifier
#n <- names(training)
#f <- as.formula(paste("training$TARGET ~", paste("training$",n[!n %in% "TARGET"], collapse = " + ")))
#nnetFit <- neuralnet(f,data=training[,-ncol(training)],
#                     hidden=c(h1+xtra,h2+xtra),rep=2,stepmax = 1e+05,linear.output=T)


size=ceiling(ncol(dataset)*0.66)  
nnetFit = nnet(training[,-ncol(training)], training[,ncol(training)],
               size=size,skip=FALSE, maxit=10^4,decay=10^{-2},trace=F,linout=T)
summary(nnetFit) ##gives description w/weights
##build predictor type="raw"
prednet<-predict(nnetFit,testing[,-ncol(testing)],type="raw")

##plot the Nnet, for correct presentation remove any current plots from viewew
plot(nnetFit)
##Build predictor
prednet<-compute(nnetFit,testing[,-ncol(testing)])

##measure error prediction for both models
MSE.nn <- sum((prednet$net.result-testing$TARGET)^2)/nrow(testing)
MSE.svm <- sum((predsvm-testing$TARGET)^2)/nrow(testing)

##compare MSE
print(paste(MSE.nn, MSE.svm))


###FURTHER EVALUATION
actualTS=testing$TARGET
##1. Evaluation for TARGET prediction. Residual sum of squares
ssr.nn=sum((prednet$net.result-actualTS)^2)
ssr.svm = sum((predsvm-actualTS)^2)
##Normalize Residual Mean Square Error (NRMSE)
nrmse.nn = sqrt(ssr.nn/((length(actualTS)-1)*var(actualTS)))
nrmse.svm = sqrt(ssr.svm/((length(actualTS)-1)*var(actualTS)))
##percentage of outperforming direct sample mean (sample expected value)
pcorrect.nn = (1-nrmse.nn)*100;  
pcorrect.svm = (1-nrmse.svm)*100;  
##compare  
print(paste(pcorrect.nn, pcorrect.svm))

######## BULLISH INDICATORS #########

dataset_B<-datasetALL[epoc2]

##Bullish set
positive = na.omit(data$positivescore) 
positiveP= na.omit(data$positivePartscr)
certainty= na.omit(data$certaintyscore)
certaintyP= na.omit(data$certaintyPartscr)
finup= na.omit(data$finupscore)
finupP= na.omit(data$finupPartscr)

bullish <- merge(na.trim(lag(positive,1)),na.trim(lag(positive,2)),na.trim(lag(positive,3)),
                 na.trim(lag(positiveP,1)),na.trim(lag(positiveP,2)),na.trim(lag(positiveP,3)),
                 na.trim(lag(certainty,1)),na.trim(lag(certainty,2)),na.trim(lag(certainty,3)),
                 na.trim(lag(certaintyP,1)),na.trim(lag(certaintyP,2)),na.trim(lag(certaintyP,3)),
                 na.trim(lag(finup,1)),na.trim(lag(finup,2)),na.trim(lag(finup,3)),
                 na.trim(lag(finupP,1)),na.trim(lag(finupP,2)),na.trim(lag(finupP,3)),
                 all=FALSE)

bullnames <- c("Pos.1","Pos.2","Pos.3",
               "PosP.1","PosP.2","PosP.3",
               "cer.1","cer.2","cer.3",
               "cerP.1","cerP.2","cerP.3",
               "finu.1","finu.2","finu.3",
               "finuP.1","finuP.2","finuP.3")

##Model Inputs:
##Define matrix of features (each column is a feature)
#Features: lags 1,2,3 of target with (or w/o)  lags 1,2,3 of sentiments
featbull = merge(na.trim(lag(target,1)),na.trim(lag(target,2)),na.trim(lag(target,3)),
             bullish,
             #add other features here,
             all=FALSE)

##add TARGET. We want to predict RETURN with and without Sentiment indices
##for without test comment line for lags of sentiment (above and in names below)
datasetALL_B = merge(featbull,target,all=FALSE)
colnames(datasetALL_B) = c("lag.1", "lag.2", "lag.3",
                         bullnames,
                         #names of other features,
                         "TARGET")

##Analise each epoch
dataset_B<-datasetALL_B[epoc2] ##rerun with epoc2

##Divide data into training (75%) and testing (25%). Use caret methods
## caret randomizes data 
index_B = 1:nrow(dataset_B)
trainindex_B= createDataPartition(index_B,p=0.75,list=FALSE)
##process class sets as data frames
training_B = as.data.frame(dataset_B[trainindex_B,])
rownames(training_B) = NULL
testing_B = as.data.frame(dataset_B[-trainindex_B,])
rownames(testing_B) = NULL


##The higher the cost produce less support vectors, increases accuracy
##However we may overfit
##svm(features,target,type, ...)
svmFit_B = svm (training_B[,-ncol(training_B)], training_B[,ncol(training_B)],
              type=type,
              kernel= "radial",
              gamma=gam,
              cost=cost
)
summary(svmFit_B)
##build predictor
predsvm_B = predict(svmFit_B, testing_B[,-ncol(testing_B)])

##Fit a  2-hidden ffnet: 
##input layer=ncol(dataset)-1; output=1. We apply rule
##The number of hidden neurons should be 2/3 the size of the input layer, plus the size of the output layer
h1_B=ceiling(ncol(dataset_B)/2)+1; h2_B=ceiling(ncol(dataset_B)/2)-1
xtra=0; ## xtra=3 ##an amplifier
n_B <- names(training_B)
bull_form <-"training_B$ lag.1 + training_B$ lag.2 + training_B$ lag.3 + training_B$ Pos.1 + training_B$ Pos.2 + training_B$ Pos.3 + training_B$ PosP.1 + training_B$ PosP.2 + training_B$ PosP.3 + training_B$ cer.1 + training_B$ cer.2 + training_B$ cer.3 + training_B$ cerP.1 + training_B$ cerP.2 + training_B$ cerP.3 + training_B$ finu.1 + training_B$ finu.2 + training_B$ finu.3 + training_B$ finuP.1 + training_B$ finuP.2 + training_B$ finuP.3"
f_B <- as.formula(paste("training_B$TARGET ~", bull_form))
nnetFit_B <- neuralnet(f_B,data=training_B[,-ncol(training_B)],
                     hidden=c(h1_B+xtra,h2_B+xtra),rep=2,stepmax = 1e+05,linear.output=T)

##plot the Nnet, for correct presentation remove any current plots from viewew
plot(nnetFit_B)
##Build predictor
prednet_B<-compute(nnetFit_B,testing_B[,-ncol(testing_B)])

##measure error prediction for both models
MSE.nn_B <- sum((prednet_B$net.result-testing_B$TARGET)^2)/nrow(testing_B)
MSE.svm_B <- sum((predsvm_B-testing_B$TARGET)^2)/nrow(testing_B)
#NOTE TO SELF -  CREATE PRESVM

##compare MSE
print(paste(MSE.nn_B, MSE.svm_B))


###FURTHER EVALUATION
actualTS_B=testing_B$TARGET
##1. Evaluation for TARGET prediction. Residual sum of squares
ssr.nn_B =sum((prednet_B$net.result-actualTS_B)^2)
ssr.svm_B = sum((predsvm_B-actualTS_B)^2)
##Normalize Residual Mean Square Error (NRMSE)
nrmse.nn_B = sqrt(ssr.nn_B)-1*var(actualTS_B)
nrmse.svm_B = sqrt(ssr.svm_B/((length(actualTS_B)-1)*var(actualTS_B)))
##percentage of outperforming direct sample mean (sample expected value)
pcorrect.nn_B = (1-nrmse.nn_B)*100;  
pcorrect.svm_B = (1-nrmse.svm_B)*100;  
##compare  
print(paste(pcorrect.nn_B, pcorrect.svm_B))

########## ONLY LAGS ############

dataset_NS <- cbind(dataset$lag.1, dataset$lag.2, dataset$lag.3, dataset$TARGET)
colnames(dataset_NS) <- c("lag.1", "lag.2", "lag.3", "TARGET")
##Divide data into training (75%) and testing (25%). Use caret methods
## caret randomizes data 
index_NS = 1:nrow(dataset_NS)
trainindex_NS= createDataPartition(index_NS,p=0.75,list=FALSE)
##process class sets as data frames
training_NS = as.data.frame(dataset_NS[trainindex_NS,])
rownames(training_NS) = NULL
testing_NS = as.data.frame(dataset_NS[-trainindex_NS,])
rownames(testing_NS) = NULL
##svm(features,target,type, ...)
svmFit_NS = svm (training_NS[,-ncol(training_NS)], training_NS[,ncol(training_NS)],
                type=type,
                kernel= "radial",
                gamma=gam,
                cost=cost
)
summary(svmFit_NS)
##build predictor
predsvm_NS = predict(svmFit_NS, testing_NS[,-ncol(testing_NS)])

##Fit a  2-hidden ffnet: 
##input layer=ncol(dataset)-1; output=1. We apply rule
##The number of hidden neurons should be 2/3 the size of the input layer, plus the size of the output layer
h1_NS=ceiling(ncol(dataset_NS)/2)+1; h2_NS = ceiling(ncol(dataset_NS)/2)-1
xtra=0; ## xtra=3 ##an amplifier
n_NS <- names(training_NS)
NS_form <-"training_B$ lag.1 + training_B$ lag.2 + training_B$ lag.3"
f_NS <- as.formula(paste("training_NS$TARGET ~", NS_form))
nnetFit_NS <- neuralnet(f_NS,data=training_NS[,-ncol(training_NS)],
                       hidden=c(h1_NS+xtra,h2_NS+xtra),rep=2,stepmax = 1e+05,linear.output=T)

##plot the Nnet, for correct presentation remove any current plots from viewew
plot(nnetFit_NS)
##Build predictor
prednet_NS <-compute(nnetFit_NS,testing_NS[,-ncol(testing_NS)])

##measure error prediction for both models
MSE.nn_NS <- sum((prednet_NS$net.result-testing_NS$TARGET)^2)/nrow(testing_NS)
MSE.svm_NS <- sum((predsvm_NS-testing_NS$TARGET)^2)/nrow(testing_NS)
#NOTE TO SELF -  CREATE PRESVM

##compare MSE
print(paste(MSE.nn_NS, MSE.svm_NS))

###FURTHER EVALUATION
actualTS_NS=testing_NS$TARGET
##1. Evaluation for TARGET prediction. Residual sum of squares
ssr.nn_NS =sum((prednet_NS$net.result-actualTS_NS)^2)
ssr.svm_NS = sum((predsvm-actualTS_NS)^2)
##Normalize Residual Mean Square Error (NRMSE)
nrmse.nn_NS = sqrt(ssr.nn_NS)-1*var(actualTS_NS)
nrmse.svm_NS = sqrt(ssr.svm_NS/((length(actualTS_NS)-1)*var(actualTS_NS)))
##percentage of outperforming direct sample mean (sample expected value)
pcorrect.nn_NS = (1-nrmse.nn_NS)*100;  
pcorrect.svm_NS = (1-nrmse.svm_NS)*100;  
##compare  
print(paste(pcorrect.nn_NS, pcorrect.svm_NS))


###### PROBLEM 2 ######

##Portfolio
symbols=c('BHP.AX','RIO.AX','ANZ.AX','CBA.AX','NAB.AX','WPL.AX', "FMG.AX")  
##retrieved ts from yahoo as google stopped provide access to the service in april of 2018
getSymbols(symbols,from="2012-01-01",to="2017-01-01")  

BHPad= BHP.AX$BHP.AX.Adjusted;RIOad=RIO.AX$RIO.AX.Adjusted;
ANZad=ANZ.AX$ANZ.AX.Adjusted; CBAad = CBA.AX$CBA.AX.Adjusted
NABad=NAB.AX$NAB.AX.Adjusted; WPLad = WPL.AX$WPL.AX.Adjusted
FMGad=FMG.AX$FMG.AX.Adjusted

## Compute daily returns for above AdjClose time series 
BHPRd= periodReturn(BHPad,period="daily")
RIORd= periodReturn(RIOad,period="daily")
ANZRd= periodReturn(ANZad,period="daily")
CBARd= periodReturn(CBAad,period="daily")
NABRd= periodReturn(NABad,period="daily")
WPLRd= periodReturn(WPLad,period="daily")
FMGRd= periodReturn(FMGad,period="daily")
## Creating returns matrix
returns=cbind(BHPRd, RIORd, ANZRd, CBARd, NABRd, WPLRd, FMGRd) 
colnames(returns) = c("BHP.ret","RIO.ret","ANZ.ret","CBA.ret","NAB.ret","WPL.ret", "FMG.ret") #"JPM.ret",
returns <- na.trim(returns)
##selecting the period of analysis
dI <- "2013-01-01"; dF <- "2017-01-01"
retP <- returns[paste(dI,"/",dF,sep="")]

# initialize portfolio
init.portfolio <- portfolio.spec(assets = symbols)
print.default(init.portfolio)

##Add some constraints: long-only 
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only")
init.portfolio <- add.constraint(init.portfolio, type="weight_sum",
                                 min_sum=0.99, max_sum=1.01)
##MinVar Portfolio
# Add objective for portfolio to minimize portfolio standard deviation
minSD.portfolio <- add.objective(portfolio=init.portfolio,
                                 type="risk",
                                 name="StdDev")
print(minSD.portfolio)

# Run the optimization for the minimum standard deviation portfolio
#optimize_method: "DEoptim"(differential evolution),"random",
#   "ROI"(R Optimization Infrastructure):Rglpk,quadprog,
#  "pso"(particle swarm optimization),"GenSA"(generalized simulated annealing) 

minSD.opt <- optimize.portfolio(R = returns, portfolio = minSD.portfolio,
                                optimize_method = "GenSA", trace = TRUE)

print(minSD.opt)

##HW: Plot Efficient Frontier
## Plot the optimal weights and the optimal portfolio in risk-return space
##to get xy-limits compute stdDev and mean for all assets
stDevAll=llply(returns,sd)
meanAll=llply(returns,mean)
d=1
ylb= min(unlist(meanAll))*d; yUb=max(unlist(meanAll))*d
xlb= 0; xUb=max(unlist(stDevAll)) 
plot(minSD.opt, risk.col="StdDev",chart.assets=TRUE,main="Min SD Optimization",
     ylim=c(ylb,yUb), xlim=c(xlb,xUb))


#Max Return per unit ES
##Create new portfolio object with same initial portfolio and constraints, 
## but different objective: To maximize mean per unit ES (Expected Shortfall)
meanES.portfolio <- add.objective(portfolio=init.portfolio, 
                                  type="return", 
                                  name="mean")
meanES.portfolio <- add.objective(portfolio=meanES.portfolio, 
                                  type="risk", 
                                  name="ES")
print(meanES.portfolio)

# Run the optimization for the maximize mean per unit ES

meanES.opt <- optimize.portfolio(R = returns, portfolio = meanES.portfolio, 
                                 optimize_method = "GenSA", trace = TRUE)

print(meanES.opt)

stDevAll=llply(returns,sd)
meanAll=llply(returns,mean)
d=2
ylb= min(unlist(meanAll))*d; yUb=max(unlist(meanAll))*d
xlb= 0; xUb=max(unlist(stDevAll)) 
plot(meanES.opt, chart.assets=TRUE,main="Mean ES Optimization",
     ylim=c(ylb,yUb), xlim=c(xlb,xUb))
