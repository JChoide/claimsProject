clc <- function() cat(rep("\n",50))
# clc()
rm(list=ls())
cat("\014")
# search()

setwd("~/")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("xtable", lib.loc="~/R/win-library/3.2")
library("corrplot", lib.loc="~/R/win-library/3.2")

denied = read.csv("claimsDenied.csv")
posted = read.csv("claimsPosted.csv")

head(denied)
head(posted)
# names(denied)
# ncol(denied)
# nrow(denied)

# summary data of denied claims
print(xtable(summary(denied)),type="html")
# summary data of posted claims
print(xtable(summary(posted)),type="html")

# alpha = 0.05
# tests the null hypothesis that all months are equally represented
p1=table(denied$Month) # frequency of denials per month
chisq.test(p1) # goodness-of-fit test is performed
# reject the null hypothesis (2.2e-16<0.05)
# sort(table(denied$Month),decreasing=T)[1:5] # top 5 denial months

# tests the null hypothesis that all clients are equally represented
p2=table(denied$Practice.ID)
chisq.test(p2)
# reject the null hypothesis (2.2e-16<0.05)
# sort(table(denied$Practice.ID),decreasing=T)[1:5]  # Top 5 denied clients

# tests the null hypothesis that all denial codes are equally represented
p3=table(denied$Denial.Code)
chisq.test(p3)
# reject the null hypothesis (2.2e-16<0.05)
# sort(table(denied$Denial.Code),decreasing=T)[1:5]  # Top 5 denial codes

# tests the null hypothesis that all denial categories are equally represented
p4=table(denied$Denial.Reason.Category)
chisq.test(p4)
# reject the null hypothesis (2.2e-16<0.05)
# sort(table(denied$Denial.Reason.Category),decreasing=T)[1:5]  # Top 5 denial cateogries

# tests the null hypothesis that all denial messages are equally represented
p5=table(denied$Denial.Message)
chisq.test(p5)
# reject the null hypothesis (2.2e-16<0.05)
# sort(table(denied$Denial.Message),decreasing=T)[1:5]  # Top 5 denial messages

# tests the null hypothesis that all claim months are equally represented
# values obtained from excel
claimpermonth = c(13710, 12008, 14735, 13601, 15117, 13898, 14655, 14224, 14924, 15691, 
                  16192, 16695)
chisq.test(claimpermonth)
# reject the null hypothesis (2.2e-16<0.05)

# obtains count of denials per range
count1 = 0
count2 = 0
count3 = 0
count4 = 0
for(i in 1:nrow(denied)){
  if(denied$Practice.ID[i]<4000 && denied$Practice.ID[i]>=3000){
    count1 = count1 + 1
  }
  else if(denied$Practice.ID<3000 && denied$Practice.ID[i]>=2000){
    count2 = count2 + 1
  }
  else if(denied$Practice.ID<2000 && denied$Practice.ID[i]>=1000){
    count3 = count3 + 1
  }
  else{
    count4 = count4 + 1
  }
}
# client range statistics
p34 = count1/nrow(denied) # 0.1597138
p23 = count2/nrow(denied) # 0.367768
p12 = count3/nrow(denied) # 0.364799
p01 = count4/nrow(denied) # 0.1077192
range = data.frame(p34,p23,p12,p01)
print(xtable(range), type="html")

# Histogram of Clients, Normality Plots, Box Plots
par(mfrow = c(2, 3))
clientdist = sort(denied$Practice.ID)
hist(clientdist, xlab="Client ID",col="red", main="Histogram of Clients")
qqnorm(clientdist)
qqline(clientdist)
boxplot(clientdist)
# Histogram of Clients, Normality Plots, Box Plots for 2500-3500 range
clientdist = sort(denied$Practice.ID)[7704:13136]
hist(clientdist, xlab="Client ID",col="blue", main="Histogram of Clients")
qqnorm(clientdist)
qqline(clientdist)
boxplot(clientdist)
par(mfrow = c(1, 1))

# Box plot of client id per month
qplot(factor(Month),Practice.ID,xlab="Month",ylab="Client ID",
      main="Boxplot of Client ID's over Time",data=denied,geom=c("boxplot"))

# Frequency for different ranges of claim IDs
id=data.frame(table(cut(denied$Claim.ID,seq(998000,1013000,1000),include.lowest=TRUE)))
print(xtable(id),type="html")
summary(id$Freq) # summary data for frequency of ranges
# IQR Outlier Test
iqr=998-994
lowerbound=994-1.5*iqr
upperbound=998+1.5*iqr
# 1,2,15 outside of range. Therefore, drop those points
stdev=sd(id$Freq)
# Residual plot
plot(id$Freq-mean(id$Freq),ylim=c(-900,900),xlab="Range ID",ylab="Residual",
     main="Residual Plot for Claim ID")
abline(stdev,0,col="red",lty=2)
abline(-stdev,0,col="red",lty=2)
abline(2*stdev,0,col="red",lty=3)
abline(-2*stdev,0,col="red",lty=3)
abline(3*stdev,0,col="red",lty=4)
abline(-3*stdev,0,col="red",lty=4)
abline(0,0)
# Chi-squared Test
chisq.test(id[3:14,2])
# Uniform: p-value = 1

# Converts factor fields to numeric
denied2<-transform(denied,Month=as.numeric(Month),
                   Denial.Reason.Category=as.numeric(Denial.Reason.Category),
                   Denial.Code=as.numeric(Denial.Code),
                   Denial.Message=as.numeric(Denial.Message))
# Correlation
cor.mtest <- function(mat,conf.level=0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(denied2, 0.95)
res2 <- cor.mtest(denied2, 0.99)
# Correlation data
c<-cor(denied2) # correlation matrix
colnames(c)<-c("Month","Client ID","Claim ID","Category","Code","Message")
rownames(c)<-c("Month","Client ID","Claim ID","Category","Code","Message")
wb <- c("white", "black")
corrplot(c,p.mat = res1[[1]],sig.level=0.01,insig="p-value",order="hclust",addrect=2,
         col=wb,bg="gold2",method="square",tl.col="black")
print(xtable(c),type="html") # print table in html code

# Plots
# Denials per Month Divided by Denial Category
ggplot()+geom_bar(data=denied,aes(x=Month,fill=Denial.Reason.Category),position="fill")
# 100% Stacked Column Histogram
qplot(Month,data=denied,geom="histogram",fill=Denial.Reason.Category)
# Smoothing curves/Scatter Plots
qplot(Practice.ID,Denial.Reason.Category,data=denied2,alpha=I(1/10),
      geom=c("point", "smooth"),span=1,xlab="Practice ID",ylab="Denial Category",
      main="Smoothing Curve Between Practice ID and Denial Category")
qplot(Practice.ID,Claim.ID,data=denied,alpha=I(1/10),geom=c("point", "smooth"),span=1,
      xlab="Practice ID",ylab="Claim ID",
      main="Smoothing Curve Between Client ID and Claim ID")
