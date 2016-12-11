#### Input nfl data into Data Frame "nflData"
#install.packages("rpart")
#install.packages("e1071")
#install.packages("arules")
install.packages("dismo")
library(arules)
library(rpart)
library(cluster)
library(fpc)
library(dismo)

nflData <- read.csv(file = "C:/Users/maryjoyce/Desktop/NFL-All.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
testData <- read.csv(file = "C:/Users/maryjoyce/Desktop/NFL-2016.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

#nflDataOrig[1:20,]
#testData[1:20,1:15]

BinningTeamData <- function(nflData){
    # Bin team data
  nflData$Team[nflData$Team=="Arizona Cardinals"]<-"1"
  nflData$Team[nflData$Team=="Atlanta Falcons"]<-"2"
  nflData$Team[nflData$Team=="Baltimore Ravens"]<-"3"
  nflData$Team[nflData$Team=="Buffalo Bills"]<-"4"
  nflData$Team[nflData$Team=="Carolina Panthers"]<-"5"
  nflData$Team[nflData$Team=="Chicago Bears"]<-"6"
  nflData$Team[nflData$Team=="Cincinnati Bengals"]<-"7"
  nflData$Team[nflData$Team=="Cleveland Browns"]<-"8"
  nflData$Team[nflData$Team=="Dallas Cowboys"]<-"9"
  nflData$Team[nflData$Team=="Denver Broncos"]<-"10"
  nflData$Team[nflData$Team=="Detroit Lions"]<-"11"
  nflData$Team[nflData$Team=="Green Bay Packers"]<-"12"
  nflData$Team[nflData$Team=="Houston Texans"]<-"13"
  nflData$Team[nflData$Team=="Indianapolis Colts"]<-"14"
  nflData$Team[nflData$Team=="Jacksonville Jaguars"]<-"15"
  nflData$Team[nflData$Team=="Kansas City Chiefs"]<-"16"
  nflData$Team[nflData$Team=="Miami Dolphins"]<-"17"
  nflData$Team[nflData$Team=="Minnesota Vikings"]<-"18"
  nflData$Team[nflData$Team=="New England Patriots"]<-"19"
  nflData$Team[nflData$Team=="New Orleans Saints"]<-"20"
  nflData$Team[nflData$Team=="New York Giants"]<-"21"
  nflData$Team[nflData$Team=="New York Jets"]<-"22"
  nflData$Team[nflData$Team=="Oakland Raiders"]<-"23"
  nflData$Team[nflData$Team=="Philadelphia Eagles"]<-"24"
  nflData$Team[nflData$Team=="Pittsburgh Steelers"]<-"25"
  nflData$Team[nflData$Team=="St. Louis Rams"]<-"26"
  nflData$Team[nflData$Team=="San Diego Chargers"]<-"27"
  nflData$Team[nflData$Team=="San Francisco 49ers"]<-"28"
  nflData$Team[nflData$Team=="Seattle Seahawks"]<-"29"
  nflData$Team[nflData$Team=="Tampa Bay Buccaneers"]<-"30"
  nflData$Team[nflData$Team=="Tennessee Titans"]<-"31"
  nflData$Team[nflData$Team=="Washington Redskins"]<-"32"
  nflData$Team <- as.numeric(nflData$Team)
  return(nflData)
}

BinningData <- function(nflData){
  nflData$Upset[nflData$Upset=="N"]<-"0"
  nflData$Upset[nflData$Upset=="Y"]<-"1"
  nflData$AorH[nflData$AorH=="A"]<-"0"
  nflData$AorH[nflData$AorH=="A*"]<-"0"
  nflData$AorH[nflData$AorH=="H"]<-"1"
  nflData$AorH[nflData$AorH=="H*"]<-"1"
#  nflData$QB[nflData$QB=="P"]<-"1"
#  nflData$QB[nflData$QB=="O"]<-"2"
#  nflData$QB[nflData$QB=='']<-"0"
#  nflData$RB[nflData$RB=="P"]<-"1"
#  nflData$RB[nflData$RB=="O"]<-"2"
#  nflData$RB[nflData$RB=='']<-"0"
  nflData$Upset <- as.numeric(nflData$Upset)
  nflData$AorH <- as.numeric(nflData$AorH)
#  nflData$QB <- as.numeric(nflData$QB)
#  nflData$RB <- as.numeric(nflData$RB)
  nflData$AvgPF <- as.numeric(nflData$AvgPF)
  nflData$AvgPA <- as.numeric(nflData$AvgPA)
  return(nflData)
}

ViewBinnedData <- function(nflData) {
#  nflData$Team
  nflData$Upset
  nflData$AorH
  nflData$QB
  nflData$RB
  
 # unique(nflData$Team)
  unique(nflData$Upset)
  unique(nflData$AorH)
  unique(nflData$QB)
}

WeatherBin <- function(nflData) {
  nflData$Weather[nflData$Weather>=85 & nflData$Weather!="Dome"]<-"Hot"
  nflData$Weather[nflData$Weather>=70 & nflData$Weather<85 & nflData$Weather!="Dome"]<-"Warm"
  nflData$Weather[nflData$Weather>=55 & nflData$Weather<70 & nflData$Weather!="Dome"]<-"Mild"
  nflData$Weather[nflData$Weather>=40 & nflData$Weather<55 & nflData$Weather!="Dome"]<-"Cool"
  nflData$Weather[nflData$Weather<40 & nflData$Weather!="Dome"]<-"Cold"
  nflData$Weather <- as.factor(nflData$Weather)
  return(nflData)
}

WeatherNumBin <- function(nflData) {
  nflData$Weather[nflData$Weather=="Dome"]<-"0"
  nflData$Weather <- as.numeric(nflData$Weather)
  return(nflData)
}

UpsetAmtBin <- function(curData) {
  curData <- as.numeric(curData)
  #curData[curData>=1 & curData<8]<-"Close"
  curData[curData>=1 & curData<15]<-"Low"
  curData[curData>=15 & curData<24]<-"Medium"
  curData[curData>=24 & curData<=45]<-"High"
  curData <- as.factor(curData)
  return(curData)
}

decisionTreeClassifyData <- function(trainData, testData) {
  nflDataUpsetWins_rpart <- rpart(Upset ~ AorH + Time + Weather, data = trainData, method = "class")
  printcp(nflDataUpsetWins_rpart)
  plotcp(nflDataUpsetWins_rpart)
  plot(nflDataUpsetWins_rpart)
  text(nflDataUpsetWins_rpart, use.n=TRUE)
  #nflDataUpsetWins_pred <- predict(nflDataUpsetWins_rpart, newData = testData, type="class")
  nflDataUpsetWins_pred <- predict(nflDataUpsetWins_rpart, testData[,-6], type="class")
  #balanceScale_pred <- predict(balanceScale_rpart, newData = testData, type="class")
  nflDataUpsetWins_pred
  table(nflDataUpsetWins_pred, testData$Upset)
}

nflData <- BinningData(nflData)
nflData <- WeatherNumBin(nflData)
testData <- BinningData(testData)
testData <- WeatherNumBin(testData)

nflDataTies <- subset(nflData, nflData$Outcome == "T")
#nflDataTies[1:20,]
nflDataWins <- subset(nflData, nflData$Outcome == "W")
#nflDataWins[1:20,]
nflDataLoses <- subset(nflData, nflData$Outcome == "L")
#nflDataLoses[1:20,]

nflDataUpsetWins <- subset(nflDataWins, nflDataWins$Upset == "1")
#nflDataUpsetWins[1:20,]
nflDataUpsetLoses <- subset(nflDataLoses, nflDataLoses$Upset == "1")
#nflDataUpsetLoses[1:20,]

#### Magnitude of Upset
### MofU - Clustering
newDataUpsetWins <- cbind(nflDataUpsetWins$Odds, nflDataUpsetWins$UpsetAmt)
#newDataUpsetWins$Odds <- lapply(newDataUpsetWins$Odds, abs);
newDataUpsetWins <- abs(newDataUpsetWins)
plot(newDataUpsetWins)

## Density-based approach: DBSCAN
#Run dbscan with eps = 2 and MinPts = 5
dbr <- dbscan(newDataUpsetWins, eps=2, MinPts=5)
str(dbr)
plot(newDataUpsetWins, col=dbr$cluster+1L)
# Silhouette plot
d <- dist(newDataUpsetWins)
sil <- silhouette(dbr$cluster,d)
plot(sil)

## Partitioning approach: K-means
myData <- newDataUpsetWins
wss<-(nrow(myData)-1)*sum(apply(myData,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(myData,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups")
#run k-means with k = 4
km4 <- kmeans(newDataUpsetWins, centers = 4)
km4
#visualize results colored by cluster
plot(newDataUpsetWins, col=km4$cluster)
#plot cluster centers
points(km4$centers,pch='x',cex=1.5)
#run k-means with k = 3
km3 <- kmeans(newDataUpsetWins, centers = 3)
km3
#visualize results colored by cluster
plot(newDataUpsetWins, col=km3$cluster)
#plot cluster centers
points(km3$centers,pch='x',cex=1.5)
#silhouette plots
d <- dist(newDataUpsetWins)
sil <- silhouette(km4$cluster,d)
plot(sil)
d <- dist(newDataUpsetWins)
sil <- silhouette(km3$cluster,d)
plot(sil)


### MofU Discretize
nflDataUpsetWins$UpsetAmt
table(discretize(nflDataUpsetWins$UpsetAmt, method="cluster", categories=4))

nflDataUpsetWins$UpsetAmt <- UpsetAmtBin(nflDataUpsetWins$UpsetAmt)
testData$UpsetAmt <- UpsetAmtBin(testData$UpsetAmt)

### MofU Classification
## Decision Tree Classification
classTestData <- subset(testData, testData$Upset!=0 & testData$Outcome!="T")
nflDataUpsetWinsFiltered <- subset(nflDataUpsetWins, complete.cases(nflDataUpsetWins))
classTestDataFiltered <- subset(classTestData, complete.cases(classTestData))
#classTestDataFiltered$UpsetAmt <- UpsetAmtBin(classTestDataFiltered$UpsetAmt)


#nflDataUpsetWins$Odds <- abs(nflDataUpsetWins$Odds)
#nflDataUpsetWinsMedHigh <- subset(nflDataUpsetWins, nflDataUpsetWins$UpsetAmt!="Low")
nflDataUpsetAmtWins_rpart <- rpart(UpsetAmt ~ AorH + Time + Weather, data = nflDataUpsetWins, method = "class")
printcp(nflDataUpsetAmtWins_rpart)
plotcp(nflDataUpsetAmtWins_rpart)
plot(nflDataUpsetAmtWins_rpart)
text(nflDataUpsetAmtWins_rpart, use.n=TRUE)
#testData$Odds <- abs(testData$Odds)
#classTestData$UpsetAmt <- as.factor(classTestData$UpsetAmt)
nflDataUpsetAmtWins_pred <- predict(nflDataUpsetAmtWins_rpart, newData = classTestDataFiltered, type="class")
nflDataUpsetAmtWins_pred <- predict(nflDataUpsetAmtWins_rpart, classTestDataFiltered[,-6], type="class")
nflDataUpsetAmtWins_pred
table(nflDataUpsetAmtWins_pred,droplevels(classTestDataFiltered)$UpsetAmt)

## Naive Bayes Classification
classifier <- naiveBayes(Upset ~ AorH + Time + Weather, data = trainData, method = "class")
pred <- predict(classifier, testData[,-5])
table(pred)
table(testData$class)
table(pred,testData$class)





nflDataSubset <- subset(nflData, nflData$Upset > 0)
nflDataSubset <- subset(nflDataSubset, nflDataSubset$Outcome =="L")

# Assumed to matter
pairs(~nflDataWins$Upset+
        nflDataWins$Weather)
pairs(~nflDataLoses$Upset+
        nflDataLoses$Weather)
pairs(~nflDataWins$Upset+
        nflDataWins$AvgPF)
pairs(~nflDataWins$Upset+
        nflDataWins$AvgPA)
pairs(~nflDataLoses$Upset+
        nflDataLoses$AvgPF)
pairs(~nflDataLoses$Upset+
        nflDataLoses$AvgPA)

hist(nflDataUpsetWins$AvgPF,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "AvgPF",
     main = "Histogram of Upset Wins vs. Avgerage Points For")
hist(nflDataUpsetWins$AvgPA,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "AvgPA",
     main = "Histogram of Upset Wins vs. Avgerage Points Against")
hist(nflDataUpsetLoses$AvgPF,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "AvgPF",
     main = "Histogram of Upset Loses vs. Average Points For")
hist(nflDataUpsetLoses$AvgPA,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "AvgPA",
     main = "Histogram of Upset Loses vs. Average Points Against")

hist(nflDataUpsetWins$Weather,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "Weather",
     main = "Histogram of Upset Wins Weather")
hist(nflDataUpsetLoses$Weather,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/3),
     xlim = c(0,100),
     col = "lightblue",
     ylab = "Count",
     xlab = "Weather",
     main = "Histogram of Upset Loses Weather")

hist(nflDataUpsetWins$Defense,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/4),
     xlim = c(0,12),
     col = "lightblue",
     ylab = "Count",
     xlab = "Defense",
     main = "Histogram of Upset Wins Defense Injuries")
hist(nflDataUpsetLoses$Defense,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/4),
     xlim = c(0,12),
     col = "lightblue",
     ylab = "Count",
     xlab = "Defense",
     main = "Histogram of Upset Loses Defense Injuries")

hist(nflDataUpsetWins$Offense,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/4),
     xlim = c(0,10),
     col = "lightblue",
     ylab = "Count",
     xlab = "Offense",
     main = "Histogram of Upset Wins Offense Injuries")
hist(nflDataUpsetLoses$Offense,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/3),
     xlim = c(0,10),
     col = "lightblue",
     ylab = "Count",
     xlab = "Offense",
     main = "Histogram of Upset Loses Offense Injuries")

hist(nflDataUpsetWins$DaysRest,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetWins)/1.5),
     xlim = c(2,14),
     col = "lightblue",
     ylab = "Count",
     xlab = "DaysRest",
     main = "Histogram of Upset Wins Days Rest")
hist(nflDataUpsetLoses$DaysRest,
     breaks = 20,
     ylim = c(0,nrow(nflDataUpsetLoses)/1.5),
     xlim = c(2,14),
     col = "lightblue",
     ylab = "Count",
     xlab = "DaysRest",
     main = "Histogram of Upset Loses Days Rest")

hist(nflDataUpsetWins$Time,
     breaks = 30,
     ylim = c(0,nrow(nflDataUpsetWins)/1.5),
     xlim = c(1200,2200),
     col = "lightblue",
     ylab = "Count",
     xlab = "Game Time",
     main = "Histogram of Upset Wins Game Time")
hist(nflDataUpsetLoses$Time,
     breaks = 30,
     ylim = c(0,nrow(nflDataUpsetLoses)/1.5),
     xlim = c(1200,2200),
     col = "lightblue",
     ylab = "Count",
     xlab = "Game Time",
     main = "Histogram of Upset Loses Game Time")




#### Classification
### Train & Test

## Decision Tree Classification
decisionTreeClassifyData <- function(trainData, testData) {
nflDataUpsetWins_rpart <- rpart(Upset ~ Odds + Time + Weather, data = nflDataWins, method = "class")
printcp(nflDataUpsetWins_rpart)
plotcp(nflDataUpsetWins_rpart)
plot(nflDataUpsetWins_rpart)
text(nflDataUpsetWins_rpart, use.n=TRUE)
#nflDataUpsetWins_pred <- predict(nflDataUpsetWins_rpart, newData = testData, type="class")
nflDataUpsetWins_pred <- predict(nflDataUpsetWins_rpart, testData[,-6], type="class")
#balanceScale_pred <- predict(balanceScale_rpart, newData = testData, type="class")
nflDataUpsetWins_pred
table(nflDataUpsetWins_pred, testData$Upset)
}
decisionTreeClassifyData(nflDataWins, testData)
  

classifyData(nflDataWins, nflDataUndetermined)
classifyData(nflDataLoses, nflDataUndetermined)

## Naive Bayes Classification
# library(class)
# library(e1071)
# classifier <- naiveBayes(class ~ balanceData, data = trainData)
classifier <- naiveBayes(Upset ~ Time + Weather + Offense + Defense, data = nflDataWins, method = "class")
classifier
pred <- predict(classifier, testData[,-5])
table(pred)
table(nflDataWins$Upset)
table(pred,nflDataWins$Upset)

## Random Forest?
# install.packages("randomForest")
# library(randomForest)
# fit <- randomForest(class ~ balanceData, data = trainData)
fit <- randomForest(Upset ~ Time + DaysRest + Offense + Defense, data = nflData, method = "class")
print(fit)
importance(fit)



