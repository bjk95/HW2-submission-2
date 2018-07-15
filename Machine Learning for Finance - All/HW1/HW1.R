#HW1 team pink sloth (Adrian Stefan and Bradley Kester)
#Loading required packages
library(quantmod)
library(xts)
require(downloader)
library('fBasics'); library("moments")
library(xtable)

#Fetching tickers
getSymbols("^AXJO", from = "1995-01-01", to = "2018-01-01")
getSymbols("ANZ.AX", from = "1995-01-01", to = "2018-01-01")
getSymbols("CBA.AX", from = "1995-01-01", to = "2018-01-01")
getSymbols("NAB.AX", from = "1995-01-01", to = "2018-01-01")
getSymbols("BHP.AX", from = "1995-01-01", to = "2018-01-01")
getSymbols("RIO.AX", from = "1995-01-01", to = "2018-01-01")
getSymbols("FMG.AX", from = "1995-01-01", to = "2018-01-01")

#Get adjusted returns

asx.adj <- AXJO$AXJO.Adjusted
bhp.adj <- BHP.AX$BHP.AX.Adjusted

#Define ASX returns
asx.d <- periodReturn(asx.adj, period = "daily")
asx.w <- periodReturn(asx.adj, period = "weekly")
asx.m <- periodReturn(asx.adj, period = "monthly")
asx.q <- periodReturn(asx.adj, period = "quarterly")
asx.y <- periodReturn(asx.adj, period = "yearly")


#Define BHP returns
bhp.d <- periodReturn(bhp.adj, period = "daily")
bhp.w <- periodReturn(bhp.adj, period = "weekly")
bhp.m <- periodReturn(bhp.adj, period = "monthly")
bhp.q <- periodReturn(bhp.adj, period = "quarterly")
bhp.y <- periodReturn(bhp.adj, period = "yearly")

#Exercise 1
freq = c("daily",
         "weekly",
         "monthly",
         "quarterly",
         "yearly")
#Basic statistics
tickers <- list("AXJO" = asx.adj, "BHP" = bhp.adj)

#Run preliminary loops to obtain basic stats table
returns = lapply(tickers, function(x) {
  
  # ticker <- x
  ticker <- x
  
  sapply(freq, function(f) { 
    
    ret <- suppressWarnings(periodReturn(ticker, f))
    
  })
  
})

returns <- sapply(returns, function(x) {
  
  out <- do.call(cbind, x)
  colnames(out) <- freq
  return(out)
  
})

tabLabs <- c("\\label{tab:AXJO} Basic descriptive statistics for FTSE",
            "\\label{tab:BHP.AX} Basic descriptive statistics for BATS")

for (x in 1:length(returns)) {
  
  # Adjust values to match slides more accurately:
  tab = basicStats(returns[[x]])
  tab["nobs",] = tab["nobs",]-tab["NAs",]
  tab["Kurtosis",] = tab["Kurtosis",] - 3
  rownames(tab)[which(rownames(tab) == "Kurtosis")] = "Exc. Kurtosis"
  tab = tab[-which(rownames(tab) == "NAs"),]
  
  # Add dates:
  start = as.Date(sapply(1:ncol(returns[[x]]), function(i) {
    
    index(returns[[x]])[which(!(is.na(returns[[x]][,i])))[1]]
    
  }))
  
  end = as.Date(sapply(1:ncol(returns[[x]]), function(i) {
    
    index(returns[[x]])[tail(which(!(is.na(returns[[x]][,i]))),1)]
    
  }))
  
  tab = rbind("Start" = as.character(start),
              "End" = as.character(end),
              tab)
  
  
  xtab = xtable(tab, digits = c(0,rep(5, ncol(tab))), caption = tabLabs[x])
  print(xtab)
  
}

save(AXJO, file = "ASX.RData")
save(BHP.AX, file = "BHP.RData")

#Normality test
(asxd.sp <- shapiro.test(as.vector(na.exclude(asx.d[813:5812])))) #Maximum of 5000 variables so we took the most recent
(asxw.sp <- shapiro.test(as.vector(na.exclude(asx.w)))) 
(asxm.sp <- shapiro.test(as.vector(na.exclude(asx.m)))) 
(asxq.sp <- shapiro.test(as.vector(na.exclude(asx.q)))) 
(asxy.sp <- shapiro.test(as.vector(na.exclude(asx.y)))) 

(bhpd.sp <- shapiro.test(as.vector(na.exclude(bhp.d))))
(bhpw.sp <- shapiro.test(as.vector(na.exclude(bhp.w)))) 
(bhpm.sp <- shapiro.test(as.vector(na.exclude(bhp.m)))) 
(bhpq.sp <- shapiro.test(as.vector(na.exclude(bhp.q)))) 
(bhpy.sp <- shapiro.test(as.vector(na.exclude(bhp.y)))) 

(asxd.jb <- jarque.test(as.vector(na.exclude(asx.d))))
(asxw.jb <- jarque.test(as.vector(na.exclude(asx.w))))
(asxm.jb <- jarque.test(as.vector(na.exclude(asx.m))))
(asxq.jb <- jarque.test(as.vector(na.exclude(asx.q))))
(asxy.jb <- jarque.test(as.vector(na.exclude(asx.y))))

(bhpd.jb <- jarque.test(as.vector(na.exclude(bhp.d))))
(bhpw.jb <- jarque.test(as.vector(na.exclude(bhp.w))))
(bhpm.jb <- jarque.test(as.vector(na.exclude(bhp.m))))
(bhpq.jb <- jarque.test(as.vector(na.exclude(bhp.q))))
(bhpy.jb <- jarque.test(as.vector(na.exclude(bhp.y))))

(asxd.ag <- agostino.test(as.vector(na.exclude(asx.d))))
(asxw.ag <- agostino.test(as.vector(na.exclude(asx.w))))
(asxm.ag <- agostino.test(as.vector(na.exclude(asx.m))))
(asxq.ag <- agostino.test(as.vector(na.exclude(asx.q))))
(asxy.ag <- agostino.test(as.vector(na.exclude(asx.y))))

(bhpd.ag <- agostino.test(as.vector(na.exclude(bhp.d))))
(bhpw.ag <- agostino.test(as.vector(na.exclude(bhp.w))))
(bhpm.ag <- agostino.test(as.vector(na.exclude(bhp.m))))
(bhpq.ag <- agostino.test(as.vector(na.exclude(bhp.q))))
(bhpy.ag <- agostino.test(as.vector(na.exclude(bhp.y))))

#Compiling test results
asxd.SPpval <- asxd.sp$p.value;asxw.SPpval <- asxw.sp$p.value; asxm.SPpval <- asxm.sp$p.value; asxq.SPpval <- asxq.sp$p.value; asxy.SPpval <- asxy.sp$p.value
asxd.JBpval <- asxd.jb$p.value;asxw.JBpval <- asxw.jb$p.value; asxm.JBpval <- asxm.jb$p.value; asxq.JBpval <- asxq.jb$p.value; asxy.JBpval <- asxy.jb$p.value
asxd.AGpval <- asxd.ag$p.value;asxw.AGpval <- asxw.ag$p.value; asxm.AGpval <- asxm.ag$p.value; asxq.AGpval <- asxq.ag$p.value; asxy.AGpval <- asxy.ag$p.value

bhpd.SPpval <- bhpd.sp$p.value; bhpw.SPpval <- bhpw.sp$p.value; bhpm.SPpval <- bhpm.sp$p.value; bhpq.SPpval <- bhpq.sp$p.value; bhpy.SPpval <- bhpy.sp$p.value
bhpd.JBpval <- bhpd.jb$p.value; bhpw.JBpval <- bhpw.jb$p.value; bhpm.JBpval <- bhpm.jb$p.value; bhpq.JBpval <- bhpq.jb$p.value; bhpy.JBpval <- bhpy.jb$p.value
bhpd.AGpval <- bhpd.ag$p.value; bhpw.AGpval <- bhpw.ag$p.value; bhpm.AGpval <- bhpm.ag$p.value; bhpq.AGpval <- bhpq.ag$p.value; bhpy.AGpval <- bhpy.ag$p.value

table.asx <- cbind(rbind(round(asxd.SPpval, digits = 5), round(asxd.JBpval, digits = 5), round(asxd.AGpval, digits = 5)),
                   rbind(round(asxw.SPpval, digits = 5), round(asxw.JBpval, digits = 5), round(asxw.AGpval, digits = 5)),
                   rbind(round(asxm.SPpval, digits = 5), round(asxm.JBpval, digits = 5), round(asxm.AGpval, digits = 5)),
                   rbind(round(asxq.SPpval, digits = 5), round(asxw.JBpval, digits = 5), round(asxq.AGpval, digits = 5)),
                   rbind(round(asxy.SPpval, digits = 5), round(asxy.JBpval, digits = 5), round(asxy.AGpval, digits = 5)))

rownames(table.asx) <- c("Shapiro Test", "Jarque-Bera Test", "Agostino Test")
colnames(table.asx) <- c("Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
table.asx

table.bhp <- cbind(rbind(round(bhpd.SPpval, digits = 5), round(bhpd.JBpval, digits = 5), round(bhpd.AGpval, digits = 5)),
                   rbind(round(bhpw.SPpval, digits = 5), round(bhpw.JBpval, digits = 5), round(bhpw.AGpval, digits = 5)),
                   rbind(round(bhpm.SPpval, digits = 5), round(bhpm.JBpval, digits = 5), round(bhpm.AGpval, digits = 5)),
                   rbind(round(bhpq.SPpval, digits = 5), round(bhpq.JBpval, digits = 5), round(bhpq.AGpval, digits = 5)),
                   rbind(round(bhpy.SPpval, digits = 5), round(bhpy.JBpval, digits = 5), round(bhpy.AGpval, digits = 5)))

rownames(table.bhp) <- c("Shapiro Test", "Jarque-Bera Test", "Agostino Test")
colnames(table.bhp) <- c("Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
table.bhp

#Density plots

#ASX

par(mfrow=c(5,2), mar=c(3,3,1.5,1))
dsdasx <- density(asx.d) ##estimating density using FFT
ylasxd <- c(min(dsdasx$y),max(dsdasx$y)) #set y limits
hist(asx.d,probability=T, main = " ASX Daily", xlab="ASX Daily returns",ylim=ylasxd)
lines(dsdasx)
bd1 <- seq(min(asx.d),max(asx.d),0.001)
lines(bd1,dnorm(bd1,mean(asx.d),sd(asx.d)),col="red") 

dswasx <- density(asx.w) ##estimating density using FFT
ylasxw <- c(min(dswasx$y),max(dswasx$y)) #set y limits
hist(asx.w,probability=T, main = " ASX Weekly", xlab="ASX Weekly returns",ylim=ylasxw)
lines(dswasx)
bw1 <- seq(min(asx.w),max(asx.w),0.001)
lines(bd1,dnorm(bd1,mean(asx.w),sd(asx.w)),col="red") 

dsmasx <- density(asx.m) ##estimating density using FFT
ylasxm <- c(min(dsmasx$y),max(dsmasx$y)) #set y limits
hist(asx.m,probability=T, main = " ASX Monthly", xlab="ASX Monthly returns",ylim=ylasxm)
lines(dsmasx)
bm1 <- seq(min(asx.m),max(asx.m),0.001)
lines(bd1,dnorm(bd1,mean(asx.m),sd(asx.m)),col="red") 

dsqasx <- density(asx.q) ##estimating density using FFT
ylasxq <- c(min(dsqasx$y),max(dsqasx$y)) #set y limits
hist(asx.q,probability=T, main = " ASX Quarterly", xlab="ASX Quarterly returns",ylim=ylasxq)
lines(dsqasx)
bq1 <- seq(min(asx.q),max(asx.q),0.001)
lines(bd1,dnorm(bd1,mean(asx.q),sd(asx.q)),col="red")  

dsyasx <- density(na.omit(asx.y)) ##estimating density using FFT
ylasxy <- c(min(dsyasx$y),max(dsyasx$y)) #set y limits
hist(asx.y,probability=T, main = " ASX Yearly", xlab="ASX Yearly returns",ylim=ylasxy)
lines(dsyasx)
by1 <- seq(min(na.exclude(asx.y)),max(na.exclude(asx.y)),0.001)
lines(by1,dnorm(by1,mean(asx.y),sd(asx.y)),col="red")  


#BHP

dsdbhp <- density(bhp.d) ##estimating density using FFT
ylbhpd <- c(min(dsdbhp$y),max(dsdbhp$y)) #set y limits
hist(bhp.d,probability=T, main = " BHP Daily", xlab="BHP Daily returns",ylim=ylbhpd)
lines(dsdbhp)
bd1 <- seq(min(bhp.d),max(bhp.d),0.001)
lines(bd1,dnorm(bd1,mean(bhp.d),sd(bhp.d)),col="red") 

dswbhp <- density(na.omit(bhp.w)) ##estimating density using FFT
ylbhpw <- c(min(dswbhp$y),max(dswbhp$y)) #set y limits
hist(bhp.w,probability=T, main = " BHP Weekly", xlab="BHP Weekly returns",ylim=ylbhpw)
lines(dswbhp)
bw1 <- seq(min(na.omit(bhp.w)),max(na.omit(bhp.w)),0.001)
lines(bd1,dnorm(bd1,mean(bhp.w),sd(bhp.w)),col="red") 

dsmbhp <- density(na.omit(bhp.m)) ##estimating density using FFT
ylbhpm <- c(min(dsmbhp$y),max(dsmbhp$y)) #set y limits
hist(bhp.m,probability=T, main = " BHP Monthly", xlab="BHP Monthly returns",ylim=ylbhpm)
lines(dsmbhp)
bm1 <- seq(min(na.omit(bhp.m)),max(na.omit(bhp.m)),0.001)
lines(bd1,dnorm(bd1,mean(bhp.m),sd(bhp.m)),col="red") 

dsqbhp <- density(bhp.q) ##estimating density using FFT
ylbhpq <- c(min(dsqbhp$y),max(dsqbhp$y)) #set y limits
hist(bhp.q,probability=T, main = " BHP Quarterly", xlab="BHP Quarterly returns",ylim=ylbhpq)
lines(dsqbhp)
bq1 <- seq(min(bhp.q),max(bhp.q),0.001)
lines(bd1,dnorm(bd1,mean(bhp.q),sd(bhp.q)),col="red")  

dsybhp <- density(na.exclude(bhp.y)) ##estimating density using FFT
ylbhpy <- c(min(dsybhp$y),max(dsybhp$y)) #set y limits
hist(bhp.y,probability=T, main = " BHP Yearly", xlab="BHP Yearly returns",ylim=ylbhpy)
lines(dsybhp)
by1 <- seq(min(na.exclude(bhp.y)),max(na.exclude(bhp.y)),0.001)
lines(by1,dnorm(by1,mean(bhp.y),sd(bhp.y)),col="red")  

#Exercise 2

par(mfrow=c(5,2), mar=c(3,3,1.5,1))
teffectPlot(asx.d)
teffectPlot(bhp.d)
teffectPlot(asx.w)
teffectPlot(na.exclude(bhp.w))
teffectPlot(asx.m)
teffectPlot(na.exclude(bhp.m))
teffectPlot(asx.q)
teffectPlot(na.exclude(bhp.q))
teffectPlot(na.exclude(asx.y))
teffectPlot(na.exclude(bhp.y))

#Exercise 3

#ASX

ASXD.EMA <- sqrt(mean(EMA(na.exclude(asx.d), n=1, ratio=0.94))) #too many observations
ASXW.EMA <- sqrt(mean(EMA(na.exclude(asx.w),  ratio=0.94))) #too many observations
ASXM.EMA <- sqrt(mean(EMA(asx.m,  ratio=0.94))) 
ASXQ.EMA <- sqrt(mean(EMA(asx.q,  ratio=0.94)))
ASXY.EMA <- sqrt(mean(EMA(na.exclude(asx.y),  ratio=0.94)))
ASXD.Std <- sd(asx.d) 
ASXW.Std <- sd(asx.w) 
ASXM.Std <- sd(asx.m) 
ASXQ.Std <- sd(asx.q)
ASXY.Std <- sd(na.exclude(asx.y))

asx.std.compare <- cbind(rbind(ASXD.Std, ASXW.Std, ASXM.Std, ASXQ.Std, ASXY.Std),
                         rbind(ASXD.EMA, ASXW.EMA, ASXM.EMA, ASXQ.EMA, ASXY.EMA))
colnames(asx.std.compare) = c("ASX Std","ASX EMA Std")
asx.std.compare

#BHP

BHPD.EMA <- sqrt(mean(EMA(na.exclude(bhp.d), n=1, ratio=0.94))) #too many observations
BHPW.EMA <- sqrt(mean(EMA(na.exclude(bhp.w),  ratio=0.94))) #too many observations
BHPM.EMA <- sqrt(mean(EMA(na.exclude(bhp.m),  ratio=0.94)))
BHPQ.EMA <- sqrt(mean(EMA(bhp.q,  ratio=0.94)))
BHPY.EMA <- sqrt(mean(EMA(na.exclude(bhp.y),  ratio=0.94)))
BHPD.Std <- sd(bhp.d) 
BHPW.Std <- sd(na.exclude(bhp.w)) 
BHPM.Std <- sd(na.exclude(bhp.m))
BHPQ.Std <- sd(bhp.q)
BHPY.Std <- sd(na.exclude(bhp.y))

bhp.std.compare <- cbind(rbind(BHPD.Std, BHPW.Std, BHPM.Std, BHPQ.Std, BHPY.Std),
                         rbind(BHPD.EMA, BHPW.EMA, BHPM.EMA, BHPQ.EMA, BHPY.EMA))
colnames(bhp.std.compare) = c("BHP Std","BHP: EMA Std")
bhp.std.compare
