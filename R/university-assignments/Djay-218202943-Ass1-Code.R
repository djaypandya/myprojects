# Question 1)

# Loading the data
my.data <- as.matrix(read.table("Djay-218202943-BikeShareMyData.txt"))

# Part 1.1)
registeredUsers <- my.data[,9]
temperature <- my.data[,4]

hist(registeredUsers, main="Histogram of Registered Users",xlab="Registered Users",xlim=c(0,450))
hist(temperature, main="Histogram of Temperature",xlab="Temperature (degC)",xlim=c(0,45))

# Part 1.2)
casualUsers <- my.data[,8]

summary(casualUsers)
summary(registeredUsers)

# Part 1.3)
mat <- cbind(casualUsers,registeredUsers)
par(las=1)

boxplot(mat, main="Box Plots", range = 0)

# Part 1.4)
plot(temperature[1:200],casualUsers[1:200],xlab = "Temperature (degC)",ylab = "Casual Users (Units)",main = "Temperature Vs Casual Users")

# Part 1.5) 
abline(lm(casualUsers~temperature),col='red')
lm(casualUsers~temperature)
cor(temperature,casualUsers)
summary(lm(casualUsers~temperature))$r.squared

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 2)

vehicleSurveyResults <- rbind(c(1360,1140,810),c(260,190,240))
colnames(vehicleSurveyResults) = c("N","V","Q")
rownames(vehicleSurveyResults) = c("P","C")

# Part 2.1)
print((sum(vehicleSurveyResults[,"V"])/sum(vehicleSurveyResults))*100)

# Part 2.2)
print((sum(vehicleSurveyResults["C",])/sum(vehicleSurveyResults))*100)

# Part 2.3)
print((vehicleSurveyResults["P","N"]/sum(vehicleSurveyResults))*100)

# Part 2.4)
print((vehicleSurveyResults["C","Q"]/sum(vehicleSurveyResults[,"Q"]))*100)

# Part 2.5) 
print((vehicleSurveyResults["P","Q"]/sum(vehicleSurveyResults["P",]))*100)

# Part 2.6)
print(((sum(vehicleSurveyResults["P",])+sum(vehicleSurveyResults[,"V"])-vehicleSurveyResults["P","V"])/sum(vehicleSurveyResults))*100)

# Part 2.7)
print(sum(vehicleSurveyResults["P",]/sum(vehicleSurveyResults)))
print(sum(vehicleSurveyResults["C",]/sum(vehicleSurveyResults)))

# Part 2.8)
print(sum(vehicleSurveyResults[,"N"]/sum(vehicleSurveyResults)))
print(sum(vehicleSurveyResults[,"V"]/sum(vehicleSurveyResults)))
print(sum(vehicleSurveyResults[,"Q"]/sum(vehicleSurveyResults)))

# Part 2.9)
print(vehicleSurveyResults["P","N"]/sum(vehicleSurveyResults[,"N"]))
print(vehicleSurveyResults["C","N"]/sum(vehicleSurveyResults[,"N"]))
print(vehicleSurveyResults["P","V"]/sum(vehicleSurveyResults[,"V"]))
print(vehicleSurveyResults["C","V"]/sum(vehicleSurveyResults[,"V"]))
print(vehicleSurveyResults["P","Q"]/sum(vehicleSurveyResults[,"Q"]))
print(vehicleSurveyResults["C","Q"]/sum(vehicleSurveyResults[,"Q"]))

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 3)

p_smokers <- 0.2
p_smokers_lungCond <- 0.6
p_nonSmokers_lungCond <- 0.15

p_lungCond_smoker <- (p_smokers_lungCond*p_nonSmokers_lungCond)/(p_smokers)
print(p_lungCond_smoker)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 4

# Part d)

MLE<-(100+60+70)/3
print(MLE)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 5

# Part 5.4 b)
posteriorSD <- 1/((3/200)+(1/100))
print(posteriorSD)

posteriorMean <- posteriorSD*(((3*1100)/200)+(800/100))
print(posteriorMean)

# Part 5.4 3)
posteriorSD1 <- 1/((15/200)+(1/100))
print(posteriorSD1)

posteriorMean1 <- posteriorSD1*(((15*1100)/200)+(800/100))
print(posteriorMean1)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 6

# Part 6.1) 
selData <- as.matrix(read.table("Djay-218202943-PCASelData.txt"))
pZ <- prcomp(selData,tol=0.01,scale=TRUE)
pZ
summary(pZ)
biplot(pZ)

# Part 6.2)
par(mfrow=c(1,2))
plot(pZ)
plot(pZ,type='l')

# Part 6.3)
selData.dist <- dist(selData)
mds <- cmdscale(selData.dist)
plot(mds)

# Part 6.4)
library(MASS)
fit<-isoMDS(selData.dist,k=2)
plot(fit$points)

# Part 6.5)
fit<-isoMDS(selData.dist,k=2)
selData.sh <- Shepard(selData.dist,fit$points)
plot(selData.sh, pch= ".")
lines(selData.sh$x, selData.sh$yf, type = "S", col="red")

# Part 6.6)
fit1<-isoMDS(selData.dist,k=4)
selData.sh1 <- Shepard(selData.dist,fit1$points)
plot(selData.sh1, pch= ".")
lines(selData.sh1$x, selData.sh1$yf, type = "S", col="red")

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Question 7

# Part 7.1)

# Part a)
zz <- read.table("SITdata2018.txt")
zz <- as.matrix(zz)
plot(zz, main = "Plot of SITdata2018")

# Part c)
zz <- read.table("SITdata2018.txt")
zz <- as.matrix(zz)
cl <- kmeans(zz,4,nstart = 25)

# Part d)
totwss = array(,c(20,1))
for (i in 2:20)
{
  print(i)
  totwss[i,1] = (kmeans(zz,centers=i))$tot.withinss
  print(totwss[i])
} 
plot(totwss, main="Total Within Sum of Squres (TOTWSS) with different K values", xlab = "K Values", ylab = "TOTWSS")
totwss

# Part 7.2)
library(kernlab)
sc <- specc(zz,centers=4)
plot(zz,col=sc)
