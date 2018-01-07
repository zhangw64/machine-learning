rm(list = ls(all = TRUE))
library(data.table)
library(randomForest)
library(glmnet)

# read in training and test data
dat <- fread("Sample100000.csv")
testdata <- fread("ProjectTestData.csv")
colnames(testdata)<-c(colnames(dat)[1],colnames(dat)[3:24])
## get the training and validation data ready----

colnames(dat)
# change the hour column to a weekday column and a hour only column
dat[substr(dat$hour,6,6) %in% c(1,8),weekday:="Tuesday"]
dat[substr(dat$hour,6,6) %in% c(2,9),weekday:="Wednesday"]
dat[substr(dat$hour,6,6) == 3 ,weekday:="Thursday"]
dat[substr(dat$hour,6,6) == 4 ,weekday:="Friday"]
dat[substr(dat$hour,6,6) == 5 ,weekday:="Saturday"]
dat[substr(dat$hour,6,6) == 6 ,weekday:="Sunday"]
dat[substr(dat$hour,6,6) == 7 ,weekday:="Monday"]
dat$hour <- substr(dat$hour,7,8)

# check the length of unique values of the columns
for (i in 1:ncol(dat)){
  tmp <- sort(table(dat[[i]]),decreasing = T)
  barplot(tmp)
  numoflevel = length(unique(dat[[i]]))
  colname = names(dat)[[i]]
  title(paste0(colname, "(col ",i,") has ", numoflevel, " levels"))
}

# dropped some columns that are distinct ids (levels > 1/10 of data rows) or repetitive (correlation from histogram seemed too high)
dat <- subset(dat,select=c(2:5,7:11,14:25))
dat[,1:ncol(dat)] <- lapply(dat[,1:ncol(dat)], as.character)


# get testdata ready step1
testdata[substr(testdata$hour,5,5) %in% c(1,8),weekday:="Tuesday"]
testdata[substr(testdata$hour,5,5) %in% c(2,9),weekday:="Wednesday"]
testdata[substr(testdata$hour,5,5) == 3 ,weekday:="Thursday"]
testdata[substr(testdata$hour,5,5) == 4 ,weekday:="Friday"]
testdata[substr(testdata$hour,5,5) == 5 ,weekday:="Saturday"]
testdata[substr(testdata$hour,5,5) == 6 ,weekday:="Sunday"]
testdata[substr(testdata$hour,5,5) == 7 ,weekday:="Monday"]
testdata$hour <- substr(testdata$hour,7,8)
head(testdata)

testdata <- subset(testdata,select=c(2:4,6:10,13:24))
testdata[,1:ncol(testdata)] <- lapply(testdata[,1:ncol(testdata)], as.character)
# for categories with levels > 30, assign the values other than the top 30 frequent ones to the most frequent value

name_level30 = list()
datname = names(dat)
for (i in 1:ncol(dat)){
  tmp <- sort(table(dat[[i]]),decreasing = T)
  name_level30[[i]] <- ifelse(length(tmp)>30,datname[[i]],NA)
  
}
datname
name_level30
dat <- as.data.frame(dat)

site_domain<- sort(table(dat[[5]]),decreasing = T)
site_domain <- as.vector(names(site_domain[1:30]))
dat$site_domain[!dat$site_domain %in% site_domain] <- tail(names(sort(table(dat[[5]]))), 1)

app_id<- sort(table(dat[[7]]),decreasing = T)
app_id <- as.vector(names(app_id[1:30]))
dat$app_id[!dat$app_id %in% app_id] <- tail(names(sort(table(dat[[7]]))), 1)

app_domain<- sort(table(dat[[8]]),decreasing = T)
app_domain <- as.vector(names(app_domain[1:30]))
dat$app_domain[!dat$app_domain %in% app_domain] <- tail(names(sort(table(dat[[8]]))), 1)

device_model <- sort(table(dat[[10]]),decreasing = T)
device_model <- as.vector(names(device_model[1:30]))
dat$device_model[!dat$device_model %in% device_model] <- tail(names(sort(table(dat[[10]]))), 1)

c14 <- sort(table(dat[[13]]),decreasing = T)
c14 <- as.vector(names(c14[1:30]))
dat$C14[!dat$C14 %in% c14] <- tail(names(sort(table(dat[[13]]))), 1)

c17 <- sort(table(dat[[16]]),decreasing = T)
c17 <- as.vector(names(c17[1:30]))
dat$C17[!dat$C17 %in% c17] <- tail(names(sort(table(dat[[16]]))), 1)

c19 <- sort(table(dat[[18]]),decreasing = T)
c19 <- as.vector(names(c19[1:30]))
dat$C19[!dat$C19 %in% c19] <- tail(names(sort(table(dat[[18]]))), 1)

c20 <- sort(table(dat[[19]]),decreasing = T)
c20 <- as.vector(names(c20[1:30]))
dat$C20[!dat$C20 %in% c20] <- tail(names(sort(table(dat[[19]]))), 1)

c21 <- sort(table(dat[[20]]),decreasing = T)
c21 <- as.vector(names(c21[1:30]))
dat$C21[!dat$C21 %in% c21] <- tail(names(sort(table(dat[[20]]))), 1)


# this gives frequency of each category in one x variable
for (i in 1:ncol(dat)){
  tmp <- sort(table(dat[[i]]),decreasing = T)
  barplot(tmp)
  numoflevel = length(unique(dat[[i]]))
  colname = names(dat)[[i]]
  title(paste0(colname, "(col ",i,") has ", numoflevel, " levels"))
}

## split training and two validation data, use this for model building and final performance check
trainind <- ceiling(nrow(dat)*0.6)
traindata <- dat[1:trainind,]
valdata <- dat[(trainind+1):ceiling(nrow(dat)),]


# the unique values for each x variable in the data set
traindata_unique <- lapply(traindata, FUN = unique)
valdata_unique <- lapply(valdata, FUN = unique)


# this for loop checks if categories in valdata is in traindata
# it saves the new categories in a list
newcategory <- list(NULL)
for (i in 1:ncol(valdata)){
  wh <- !(valdata_unique[[i]] %in% traindata_unique[[i]])
  cat("i=",i,"number of new categories", sum(wh),"\n")
  cat("percentage of similarity",round(sum(!wh)/length(wh)*100,1),"%\n")
  newcategory[[i]] <- valdata_unique[[i]][wh]
}

# match the new category with the most frequent level in the column
names(valdata)
valdata$app_category[valdata$app_category %in% newcategory[[9]]] <-tail(names(sort(table(dat[[9]]))), 1)

# check it they match

traindata_uniquecheck <- lapply(traindata, FUN = unique)
valdata_uniquecheck <- lapply(valdata, FUN = unique)
newcategorycheck <- list(NULL)
for (i in 1:ncol(valdata)){
  wh <- !(valdata_uniquecheck[[i]] %in% traindata_uniquecheck[[i]])
  cat("i=",i,"number of new categories", sum(wh),"\n")
  cat("percentage of similarity",round(sum(!wh)/length(wh)*100,1),"%\n")
  newcategorycheck[[i]] <- valdata_uniquecheck[[i]][wh]
}


# now that the valdata and traindata have same levels for all columns, set the levels equal
valdata[,2:ncol(valdata)] <- lapply(valdata[,2:ncol(valdata)], factor)
traindata[,2:ncol(traindata)] <- lapply(traindata[,2:ncol(traindata)], factor)
valdata[,1] <- as.numeric(valdata[,1])
traindata[,1] <- as.numeric(traindata[,1])
common <- intersect(names(traindata), names(valdata)) 
for (p in common) { 
  if (class(traindata[[p]]) == "factor") { 
    levels(valdata[[p]]) <- levels(traindata[[p]]) 
  } 
}

# log loss function-----
LLfn <- function(PHat,YVal) {
  tmp <- rep(NA,length(PHat))
  tmp[YVal==1] <- log(PHat[YVal==1])
  tmp[YVal==0] <- log(1-PHat[YVal==0])
  mean(-tmp)
}

## random forest----

# tried different parameters in ntree and moxnodes, below is the best model
# we are setting the parameters high because we have many categorical variables and many data
out <- randomForest(as.numeric(click)~.,data=traindata,mtry=6,ntree=500,maxnodes=500)
ypred <- predict(out,newdata= valdata[,-1], type = "response")
LL1 <- LLfn(ypred,as.numeric(as.character(valdata[,1])))

## lasso----

library(glmnet)
library(boot)
xtrain <- model.matrix(click ~ .,traindata)

xval <- model.matrix(click ~ .,valdata)

ytrain <- as.numeric(traindata$click)

# started with default, then grid
grid1 <- seq(0,0.07,length=100) # good log loss = 0.4204 when lamda = 0.005
grid2 <- seq(0.004,0.01,length=50) # good log loss = 0.42039
grid3 <- seq(0.005, 0.006,length=50) # good log loss = 0.4485
# got the inverse bell shape curve on this grid

outR <- glmnet(xtrain,ytrain,family = "binomial", lambda = grid3, alpha=1,thresh=1e-12)
YHat <- predict.glmnet(outR,xval, type="response")

YHatlogit<-inv.logit(YHat)
LL2 <- apply(YHatlogit,2, FUN=LLfn, as.numeric(valdata[,1]))
min(LL2)
which.min(LL2)
lassoind = which.min(LL2)
outR$lambda[lassoind]
plot(outR$lambda,LL2)

# lower than random forest
# so we will use random forest


## testdata, clean and match the levels of columns in the testdata with the traindata----



head(testdata)
head(traindata)
# for categories with levels > 30, assign the values other than the top 30 frequent ones to the most frequent value

testdata <- as.data.frame(testdata)

# the unique values for each x variable in the data set

testdata_unique <- lapply(testdata, FUN = unique)

# this for loop checks if categories in valdata is in traindata
# it saves the new categories in a list
newcategory <- list(NULL)
for (i in 1:ncol(testdata)){
  wh <- !(testdata_unique[[i]] %in% traindata_unique[[i+1]])
  cat("i=",i,"number of new categories", sum(wh),"\n")
  cat("percentage of similarity",round(sum(!wh)/length(wh)*100,1),"%\n")
  newcategory[[i]] <- testdata_unique[[i]][wh]
}

# match the new category with the most frequent level in the column
names(testdata)
names(traindata)
head(testdata)
head(traindata)

testdata$site_domain[testdata$site_domain %in% newcategory[[4]]] <-tail(names(sort(table(traindata[[5]]))), 1)

testdata$site_category[testdata$site_category %in% newcategory[[5]]] <-tail(names(sort(table(traindata[[6]]))), 1)

testdata$app_id[testdata$app_id %in% newcategory[[6]]] <-tail(names(sort(table(traindata[[7]]))), 1)

testdata$app_domain[testdata$app_domain %in% newcategory[[7]]] <-tail(names(sort(table(traindata[[8]]))), 1)

testdata$app_category[testdata$app_category %in% newcategory[[8]]] <-tail(names(sort(table(traindata[[9]]))), 1)

testdata$device_model[testdata$device_model %in% newcategory[[9]]] <-tail(names(sort(table(traindata[[10]]))), 1)

testdata$device_type[testdata$device_type %in% newcategory[[10]]] <-tail(names(sort(table(traindata[[11]]))), 1)

testdata$C14[testdata$C14 %in% newcategory[[12]]] <-tail(names(sort(table(traindata[[13]]))), 1)

testdata$C17[testdata$C17 %in% newcategory[[15]]] <-tail(names(sort(table(traindata[[16]]))), 1)

testdata$C19[testdata$C19 %in% newcategory[[17]]] <-tail(names(sort(table(traindata[[18]]))), 1)

testdata$C20[testdata$C20 %in% newcategory[[18]]] <-tail(names(sort(table(traindata[[19]]))), 1)

testdata$C21[testdata$C21 %in% newcategory[[19]]] <-tail(names(sort(table(traindata[[20]]))), 1)

# check if they match

testdata_uniquecheck <- lapply(testdata, FUN = unique)
newcategorycheck <- list(NULL)
for (i in 1:ncol(testdata)){
  wh <- !(testdata_uniquecheck[[i]] %in% traindata_uniquecheck[[i+1]])
  cat("i=",i,"number of new categories", sum(wh),"\n")
  cat("percentage of similarity",round(sum(!wh)/length(wh)*100,1),"%\n")
  newcategorycheck[[i]] <- testdata_uniquecheck[[i]][wh]
}


# now that the testdata and traindata have same levels for all columns, set the levels equal
testdata[,1:ncol(testdata)] <- lapply(testdata[,1:ncol(testdata)], factor)
traindata[,2:ncol(traindata)] <- lapply(traindata[,2:ncol(traindata)], factor)
traindata[,1] <- as.numeric(traindata[,1])
common <- intersect(names(traindata[,2:ncol(traindata)]), names(testdata)) 
for (p in common) { 
  if (class(traindata[[p]]) == "factor") { 
    levels(testdata[[p]]) <- levels(traindata[[p]]) 
  } 
}

# make prediction----

ypredR <- predict(out,newdata= testdata, type = "response")


# submission file
submission <- fread("ProjectSubmission-Team7tmp.csv", header = T)
submissionname <- names(submission)
submissionname
submission[[2]] <- ypredR
head(submission)
write.csv(submission, "ProjectSubmission-Team7.csv")

