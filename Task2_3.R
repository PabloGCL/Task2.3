################### Libraries #######################

rm(list=ls(all=TRUE))

pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, ddalpha, DEoptimR, 
               dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, corrplot, dplyr, randomForest, MASS, forestFloor)


################## Calling Documents #####################

setwd("C:/Users/Usuario/Desktop/Big Data/2.3") 

existprod <- read_delim("existingproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)

newprod <- read_delim("newproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)


################ Product Size #####################

existprod$ProductSize <- existprod$ProductHeight*existprod$ProductWidth*existprod$ProductDepth
newprod$ProductSize <- newprod$ProductHeight*newprod$ProductWidth*newprod$ProductDepth

existprod$ProductHeight <- NULL
existprod$ProductWidth <- NULL
existprod$ProductDepth <- NULL

newprod$ProductHeight <- NULL
newprod$ProductWidth <- NULL
newprod$ProductDepth <- NULL


################ Warranty & Rest ####################

Warr <- filter(existprod, ProductType=="ExtendedWarranty")
NoWarr <- filter(existprod, ProductType!="ExtendedWarranty")

NewWarr <- filter(newprod, ProductType=="ExtendedWarranty")
NewNoWarr <- filter(newprod, ProductType!="ExtendedWarranty")

############### Outliers ##############

NoWarr_NoOut <- filter(existprod, PositiveServiceReview<=250)
NoWarr_NoOut <- filter(existprod, x2starReviews<=200)
NoWarr_NoOut <- filter(existprod, Volume<=3000)

################## Dummify the data ######################

NoWarrDummy <- dummyVars(" ~ .", data = NoWarr)
NoWarrDummy <- data.frame(predict(NoWarrDummy, newdata = NoWarr))

newDummy <- dummyVars(" ~ .", data = NewNoWarr)
newDummy <- data.frame(predict(newDummy, newdata = NewNoWarr))


################## Correlation ###################

  ## Dummy Variables
NoWarrDummy$BestSellersRank <- NULL
NoWarrDummy$x5StarReviews <- NULL
NoWarrDummy$ProductNum <- NULL


newDummy$BestSellersRank <- NULL
newDummy$x5StarReviews <- NULL
newDummy$ProductNum <- NULL


corrNoWarrDummy <- cor(NoWarrDummy) 
corrplot(corrNoWarrDummy)

corrNewDummy <- cor(newDummy) 
corrplot(corrNewDummy)

  ## Before Dummy 
existprod$BestSellersRank <- NULL
existprod$x5StarReviews <- NULL
existprod$ProductType <- NULL
existprod$ProductNum <- NULL

NewNoWarr$BestSellersRank <- NULL
NewNoWarr$x5StarReviews <- NULL
NewNoWarr$ProductType <- NULL
NewNoWarr$ProductNum <- NULL

NoWarr$BestSellersRank <- NULL
NoWarr$x5StarReviews <- NULL
NoWarr$ProductType <- NULL
NoWarr$ProductNum <- NULL

corrOGExist <- cor(existprod) 
corrplot(corrOGExist)

corrNew <- cor(NewNoWarr) 
corrplot(corrNew)

corrNoWarr <- cor(NoWarr) 
corrplot(corrNoWarr)

#################### Decision Tree ##########################

set.seed(123)
DecisionTree <- rpart(
  Volume ~ ., 
  data = NoWarr, 
  control = rpart.control(minsplit = 2)
  )

rpart.plot(DecisionTree) #, box.palette="RdBu")


set.seed(123)
DTDummy <- rpart(
  Volume ~ ., 
  data = NoWarrDummy, 
  control = rpart.control(minsplit = 2)
)

rpart.plot(DTDummy) #, box.palette="RdBu")


#################### Variable Weight ###############

set.seed(123)
train=sample(1:nrow(NoWarr), nrow(NoWarr)*0.75)
NoWarr.rf=randomForest(Volume ~ . , data = NoWarr, subset = train , importance=TRUE, ntree=500)

test <- NoWarr[-train,]
NoWarrpred=predict(NoWarr.rf, test)

importance(NoWarr.rf)
varImpPlot(NoWarr.rf)


#################### Linear Model ##################

set.seed(123)
inTrain <- createDataPartition(
  y = NoWarrDummy$Volume,
  p = .75,
  list = FALSE)

training <- NoWarrDummy[ inTrain,]
testing <- NoWarrDummy[-inTrain,]


control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        classProbs=TRUE)

    
  # One Variable #  
temp <- names(training[,1:(ncol(training)-1)]) 
metrics.acumLM1 <-  c()   

for(i in 12:(ncol(training)-1)){
  if(is.numeric(training[,i])){
    LinearModel1 <- lm(as.formula(paste(c("Volume ~ 0+", temp[i]))), training)
    PredLM1 <- predict(LinearModel1, testing)
    metricsLM1<-postResample(PredLM1, testing$Volume)
    metrics.acumLM1<-cbind(metrics.acumLM1, metricsLM1)
  }
}

metrics.acumLM1
#names(metrics.acumLM1)[,1:ncol(metrics.acumLM1)] <- colnames(training[1:(ncol(training)-1)])


# Two Variables #

temp <- names(training[,1:(ncol(training)-1)]) 
metrics.acumLM2 <-  c()   
set.seed(123)
LMmodel2 <- list()
for(i in 12:ncol(training)){
  for(j in i:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel2 <- lm(as.formula(paste(c("Volume ~ 0+", temp[i],temp[j]))), training)
    PredLM2 <- predict(LinearModel2, testing)
    metricsLM2<-postResample(PredLM2, testing$Volume)
    metrics.acumLM2<-cbind(metrics.acumLM2, metricsLM2)
  }
 }
}



metrics.acumRF <-  c()
#for(i in 12:(ncol(training)-1)){
#if(is.numeric(training[,i])){
RFModel <- train(Volume ~., 
                 data=training, method="rf", 
                 ntrees=100, tuneLength=10, trControl=control, importance= TRUE)
PredRF <- predict(RFModel, testing)
metricsRF<-postResample(PredRF, testing$Volume)
metrics.acumRF<-cbind(metrics.acumRF, metricsRF)
importance(PredRF)
#  }
#}



#RFmodel <- list()
#for(i in 12:ncol(training)){
# if(is.numeric(training[,i])){
RFmodel <- train(Volume ~ x4StarReviews+PositiveServiceReview, 
                 data=training,
                 method="rf",
                 tuneLength=10,
                 trControl=control
)
PredRF <- predict(RFmodel, testing)
RFmetrics <- postResample(PredRF, testing)
RFmetrics.acum <- cbind(metrics.acum, metrics)
#}
#}





summary(LinearModel)
coefficients(LinearModel) # model coefficients
confint(LinearModel, level=0.95) # CIs for model parameters 
fitted(LinearModel) # predicted values
residuals(LinearModel) # residuals
anova(LinearModel) # anova table 
vcov(LinearModel) # covariance matrix for model parameters 
influence(LinearModel) # regression diagnostics

ggplot(existing, aes(x=x4StarReviews, y=Volume)) +geom_point() + geom_smooth(method='lm', se=FALSE)


