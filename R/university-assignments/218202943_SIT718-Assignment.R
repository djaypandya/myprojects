linearTransform <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

theData <- read.table(as.matrix("ENB18data.txt"))
myData <- theData[sample(1:768,300),c(1:5,7)]

hist(myData[,1],xlab="Relative Compactness (fraction)")
hist(myData[,2],xlab="Surface Area (m^2)")
hist(myData[,3],xlab="Wall Area (m^2)")
hist(myData[,4],xlab="Roof Area (m^2)")
hist(myData[,5],xlab="Overall Height (m^2)")
hist(myData[,6],xlab="Cooling Load (kWh.m^-2)")

plot(myData[,1],myData[,6],xlab="Relative Compactness (fraction)",ylab="Cooling Load (kWh.m^-2)")
plot(myData[,2],myData[,6],xlab="Surface Area (m^2)",ylab="Cooling Load (kWh.m^-2)")
plot(myData[,3],myData[,6],xlab="Wall Area (m^2)",ylab="Cooling Load (kWh.m^-2)")
plot(myData[,4],myData[,6],xlab="Roof Area (m^2)",ylab="Cooling Load (kWh.m^-2)")
plot(myData[,5],myData[,6],xlab="Overall Height (m)",ylab="Cooling Load (kWh.m^-2)")

RC<-myData[,1]
SA<-myData[,2]
WA<-myData[,3]
RA<-myData[,4]
OH<-myData[,5]
CL<-myData[,6]

RCtrans<-linearTransform(RC)
SAtrans<-linearTransform(SA)
WAtrans<-linearTransform(WA)
RAtrans<-linearTransform(RA)
OHtrans<-linearTransform(OH)
CLtrans<-linearTransform(CL)

yourData<-matrix(c(SAtrans,WAtrans,RAtrans,OHtrans,CLtrans),nrow=300,ncol=5)
write.table(yourData,"djay-transformed.txt")

source("AggWaFit718.R")

fit.OWA(yourData,"outputOWA.txt","statsOWA.txt")
fit.choquet(yourData,"outputChoquet.txt","statsChoquet.txt")
fit.QAM(yourData,"outputQM.txt","statsQM.txt",g=QM,g.inv=invQM)
fit.QAM(yourData,"outputPM05.txt","statsPM05.txt",g=PM05,g.inv=invPM05)
fit.QAM(yourData,"outputWAM.txt","statsWAM.txt",g=AM,g.inv=invAM)

RCx<-0.82
SAx<-612.5
WAx<-318.5
RAx<-147
OHx<-7

RCxtrans<-(RCx-min(RC))/(max(RC)-min(RC))
SAxtrans<-(SAx-min(SA))/(max(SA)-min(SA))
WAxtrans<-(WAx-min(WA))/(max(WA)-min(WA))
RAxtrans<-(RAx-min(RA))/(max(RA)-min(RA))
OHxtrans<-(OHx-min(OH))/(max(OH)-min(OH))

Xvec<-c(SAxtrans,WAxtrans,RAxtrans,OHxtrans)
fuzzy<-c(0,0.175393380358087,0.175393380358087,0.101804123711335,0.101804123711335,0.175393380358101,0.175393380358101,0.404286489419351,0.404286489419351,0.741182854042256,0.741182854042256,0.404286489419351,0.663103635377247,0.741182854042201,1)
predictedCLtrans<-choquet(Xvec,fuzzy)
predictedCL<-predictedCLtrans*(max(CL)-min(CL))+min(CL)
predictedCL
