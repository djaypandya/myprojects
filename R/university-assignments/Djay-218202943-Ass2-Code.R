# Question 1

AIMSDataAirPres<- as.matrix(read.csv("AIMSNingalooReefAirPressure.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = ""))

# 1.1)
# Time Series Plot
AIMSDataTimeSeries <- ts(AIMSDataAirPres)
plot.ts(AIMSDataTimeSeries)

# Five Point Summary
summary(AIMSDataAirPres)

# Mean
mean(AIMSDataAirPres)

# Standard Deviation
sd(AIMSDataAirPres)

# 1.2)
hist(AIMSDataAirPres)

# 1.3)
library(MASS)
fit1 <- fitdistr(AIMSDataAirPres,"normal")
hist(AIMSDataAirPres,prob=TRUE)
curve(dnorm(x, fit1$estimate[1], fit1$estimate[2]), col="red", lwd=2, add=T)

# 1.4)
install.packages("mixtools")
library(mixtools)
mixmdl <- normalmixEM(AIMSDataAirPres) # fits two gaussians by default
print(summary(mixmdl)) #returns lambda value, mean and standard deviation of each distribution

# 1.5)
plot(mixmdl,which=2)
curve(dnorm(x, fit1$estimate[1], fit1$estimate[2]), col="blue", lwd=2, add=T)
legend(996, 0.08, legend=c("Mix-model Gaussian 1", "Mix-model Gaussian 2","Combined Density Distribution"),col=c("red", "green","blue"), lty=1, cex=0.7)

# 1.6) 
plot(mixmdl$all.loglik)

# Question 3

# 3.2)
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("gRain")
library(RBGL)
library(gRbase)
library(gRain)
lh <- c("low","high")
lhn <- c("low","high","normal")
e <- cptable(~eh,values=c(0.2,0.8),levels=lh)
o.e <- cptable(~oil|eh,values=c(0.9,0.1,0.05,0.95),levels=lh)
i.e.o <- cptable(~inf|eh:oil,values=c(0.9,0.1,0.2,0.8,0.1,0.9,0.02,0.98),levels=lh)
b.o <- cptable(~bp|oil,values=c(0.8,0.15,0.05,0.1,0.4,0.5),levels=lhn)
r.e.i <- cptable(~rt|eh:inf,values=c(0.6,0.3,0.1,0.1,0.2,0.7,0.2,0.2,0.6,0.05,0.1,0.85),levels=lhn)
cptlist <- compileCPT(list(e,o.e,i.e.o,b.o,r.e.i))
net1 <- grain(cptlist)
biocLite("Rgraphviz")
plot(net1)
summary(cptlist)

#3.3)
net12 <- setEvidence(net1,nodes=c("bp","rt"),states=c("high","normal"))
p.inf <- querygrain(net12,nodes=c("inf"))
print(p.inf$inf[2])

# Question 4

# 4.2)
library(igraph)
install.packages("ggm")
library(ggm)
dag <- DAG(g~c,c~a,f~c,f~d,d~a,d~b,f~e,h~f,h~e)
drawGraph(dag,adjust=FALSE)
dSep(dag,first="c",second="g",cond=NULL)
dSep(dag,first="c",second="h",cond=c("e"))
dSep(dag,first="g",second="e",cond=c("d"))
dSep(dag,first="c",second="h",cond=c("f"))
dSep(dag,first="b",second="g",cond=c("f"))
dSep(dag,first="b",second="g",cond=c("d","c","e"))
dSep(dag,first="a",second="h",cond=c("d","f"))
