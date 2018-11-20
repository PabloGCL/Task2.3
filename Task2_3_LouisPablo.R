pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, ddalpha, 
               DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, corrplot, dplyr, 
               randomForest, MASS, forestFloor, processx, e1071)

#if (!require("processx")) install.packages("processx")
setwd("C:/Users/Usuario/Desktop/Big Data/2.3")
existprod <- read.csv('existingproductattributes2017.2.csv')
newprod <- read.csv('newproductattributes2017.2.csv')

#Anova Decision tree before dummying to look at potential impact of product type#----
DTvol <- rpart(Volume~.,data=existprod, method='anova')
rpart.plot(DTvol)
existprod$x5StarReviews <- NULL
newprod$x5StarReviews <- NULL

p2 <- plot_ly(existprod, x = ~PositiveServiceReview, y = ~x4StarReviews, z = ~Volume, color = ~ProductType) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Positive Service Review'),
                      yaxis = list(title = '4 Star Reviews'),
                      zaxis = list(title = 'Volume')))
         
p2

#----PreProcessing----
#Dummifying the data
newDataframe <- dummyVars('~ .', data = newprod)
newpro_dummified <- data.frame(predict(newDataframe, newdata = newprod))

newDataframe <- dummyVars('~ .', data=existprod)
existprod_dummified <- data.frame(predict(newDataframe, newdata = existprod))

#Getting rid of BSR (maybe we can do soemthing different with this)
existprod_dum_NAs <- existprod_dummified
existprod_dum_NAs$BestSellersRank <- NULL

newprod_dum_NAs <- newpro_dummified
newprod_dum_NAs$BestSellersRank <- NULL

#Getting rid of illogical or irrelevant variables: Product# & Profit margin
existprod_dum_NAs$ProductNum <- NULL
#existprod_dum_NAs$ProfitMargin <- NULL

newprod_dum_NAs$ProductNum <- NULL
#newprod_dum_NAs$ProfitMargin <-NULL

#Correlation of variables
Correxist <- cor(existprod_dum_NAs)
corrplot(Correxist)

#Correlation of Ratings, Service Reviews and Volume
Volume <- existprod_dum_NAs[,25]
exist_CustRev_ServRev_vol <- cbind(existprod_dum_NAs[,14:20],Volume)
Cor_CustRev_ServRev_vol <- cor(exist_CustRev_ServRev_vol)
corrplot(Cor_CustRev_ServRev_vol)
Cor_CustRev_ServRev_vol

#Getting rid of highly correlated features
existprod_dum_NAs_xcor <- existprod_dum_NAs
existprod_dum_NAs_xcor$x3StarReviews <- NULL
existprod_dum_NAs_xcor$x1StarReviews <- NULL
existprod_dum_NAs_xcor$NegativeServiceReview <- NULL

newprod_dum_NAs_xcor <- newprod_dum_NAs
newprod_dum_NAs_xcor$x3StarReviews <- NULL
newprod_dum_NAs_xcor$x1StarReviews <- NULL
newprod_dum_NAs_xcor$NegativeServiceReview <- NULL

#Feature engineering width, length and depth into one variable
Product_size <- existprod_dum_NAs_xcor$ProductDepth*existprod_dum_NAs_xcor$ProductWidth*existprod_dum_NAs_xcor$ProductHeight
existprod_dum_NAs_xcor_size <- existprod_dum_NAs_xcor
existprod_dum_NAs_xcor_size$ProductSize <- Product_size

Product_size <- newprod_dum_NAs_xcor$ProductDepth * newprod_dum_NAs_xcor$ProductWidth * newprod_dum_NAs_xcor$ProductHeight
newprod_dum_NAs_xcor_size <- newprod_dum_NAs_xcor
newprod_dum_NAs_xcor_size$ProductSize <- Product_size

# Delete Width/Height/Depth
existprod_dum_NAs_xcor_size[,19:21] <- NULL
newprod_dum_NAs_xcor_size [,19:21] <- NULL

#Decision tree to have a quick look at ProductType impact
DTclass <- rpart(Volume~., data=existprod_dum_NAs_xcor_size,method='anova', 
               control=rpart.control(minsplit=3))
rpart.plot(DTclass)


----# Series of plots for initial look #----
# A loop for scatterplots of existing data
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
(plot(existprod_dum_NAs_xcor_size[,i],existprod_dum_NAs_xcor_size$Volume,main=paste('Scatterplot of Volume and',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers of existing data
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
(boxplot(existprod_dum_NAs_xcor[,i],main=paste('Boxplot of',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers the new_prod data
for (i in 13:ncol(newpro_dummified)){
(boxplot(newpro_dummified[,i],main=paste('Boxplot of',colnames(newpro_dummified[i]))))}

#for loop to graph box plot of each feature for existing and new products
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
plot_ly(y=existprod_dum_NAs_xcor_size[,i], type="box", name='Existing products') %>% 
  add_boxplot(y=newprod_dum_NAs_xcor_size[,i], type="box", name='New products') %>%
  layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[i]),'for Existing and New Products'))
}
# A simple example
plot_ly(y=existprod_dum_NAs_xcor_size[,20], type="box", name='Existing products') %>% 
add_boxplot(y=newprod_dum_NAs_xcor_size[,20], type="box", name='New products') %>%
layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[20]),'for Existing and New Products'))


#----Deletion of Outliers----
#Delete Warranties
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size[!existprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty == 1,] #getting rid of warranty rows
rownames(existprod_dum_NAs_xcor_size_warr) <- 1:nrow(existprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

newprod_dum_NAs_xcor_size_warr <- newprod_dum_NAs_xcor_size[!newprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty ==1,] #gettint rid of warranty rows
rownames(newprod_dum_NAs_xcor_size_warr) <- 1:nrow(newprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

existprod_dum_NAs_xcor_size_warr$ProductType.ExtendedWarranty <- NULL
newprod_dum_NAs_xcor_size_warr$ProductType.ExtendedWarranty <- NULL

#Deletion of specific outliers
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$PositiveServiceReview > 250,]
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$x2StarReviews > 200,]
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$Volume > 3000,]

#----Look at predicting Best Seller Rank----

BSRdataset <- existprod
BSRdataset$ProductNum <- NULL

#creating a vector to then partition the data
vec <- vector() 
for (i in 1:nrow(BSRdataset)){
if (is.na(BSRdataset[i,11])){
  vec <- c(vec,i)
}
}

BSRpredict <- BSRdataset[vec,] #Using vector to partition data
BSRmodel <- BSRdataset[-vec,]

rownames(BSRpredict) <- 1:nrow(BSRpredict) #reindexing the subsequent dataframes
rownames(BSRmodel) <- 1:nrow(BSRmodel)

#creating quick DT to look at possible predictions
DT_BSR <- rpart(BestSellersRank~.,data=BSRmodel,method='anova', 
              control=rpart.control(minsplit=2))
rpart.plot(DT_BSR)

#Dummyfying Product type
newDataframe <- dummyVars('~ .', data = BSRmodel)
BSRmodel_dumm <- data.frame(predict(newDataframe, newdata = BSRmodel))

newDataframe <- dummyVars('~ .', data = BSRpredict)
BSRmpredict_dumm <- data.frame(predict(newDataframe, newdata = BSRpredict))

#Creating correlation matrix
CORR <- cor(BSRmodel_dumm)
corrplot(CORR)


#----Create Models----

#look at correlation to look at features to start predicting with
corrplot(cor((existprod_dum_NAs_xcor_size_warr)))

set.seed(107)
intrain <- createDataPartition(
y = existprod_dum_NAs_xcor_size_warr$Volume,
p=.75,
list = FALSE
)

exist_train <- existprod_dum_NAs_xcor_size_warr[intrain,]
exist_test <- existprod_dum_NAs_xcor_size_warr[-intrain,]
BSRmodel <- BSRdataset[-vec,]

rownames(BSRpredict) <- 1:nrow(BSRpredict) #reindexing the subsequent dataframes
rownames(BSRmodel) <- 1:nrow(BSRmodel)

#creating quick DT to look at possible predictions
DT_BSR <- rpart(BestSellersRank~.,data=BSRmodel,method='anova', 
                control=rpart.control(minsplit=2))
rpart.plot(DT_BSR)

#Dummyfying Product type
newDataframe <- dummyVars('~ .', data = BSRmodel)
BSRmodel_dumm <- data.frame(predict(newDataframe, newdata = BSRmodel))

newDataframe <- dummyVars('~ .', data = BSRpredict)
BSRmpredict_dumm <- data.frame(predict(newDataframe, newdata = BSRpredict))

#Creating correlation matrix
CORR <- cor(BSRmodel_dumm)
corrplot(CORR)


#----Create Models----

#look at correlation to look at features to start predicting with
corrplot(cor((existprod_dum_NAs_xcor_size_warr)))

set.seed(107)
intrain <- createDataPartition(
  y = existprod_dum_NAs_xcor_size_warr$Volume,
  p=.75,
  list = FALSE
)

exist_train <- existprod_dum_NAs_xcor_size_warr[intrain,]
exist_test <- existprod_dum_NAs_xcor_size_warr[-intrain,]
BSRmodel <- BSRdataset[-vec,]

rownames(BSRpredict) <- 1:nrow(BSRpredict) #reindexing the subsequent dataframes
rownames(BSRmodel) <- 1:nrow(BSRmodel)

#creating quick DT to look at possible predictions
DT_BSR <- rpart(BestSellersRank~.,data=BSRmodel,method='anova', 
                control=rpart.control(minsplit=2))
rpart.plot(DT_BSR)

#Dummyfying Product type
newDataframe <- dummyVars('~ .', data = BSRmodel)
BSRmodel_dumm <- data.frame(predict(newDataframe, newdata = BSRmodel))

newDataframe <- dummyVars('~ .', data = BSRpredict)
BSRmpredict_dumm <- data.frame(predict(newDataframe, newdata = BSRpredict))

#Creating correlation matrix
CORR <- cor(BSRmodel_dumm)
corrplot(CORR)


#----Create Models----

#look at correlation to look at features to start predicting with
corrplot(cor((existprod_dum_NAs_xcor_size_warr)))

set.seed(107)
intrain <- createDataPartition(
  y = existprod_dum_NAs_xcor_size_warr$Volume,
  p=.75,
  list = FALSE
)

exist_train <- existprod_dum_NAs_xcor_size_warr[intrain,]
exist_test <- existprod_dum_NAs_xcor_size_warr[-intrain,]
nrow(exist_train)
nrow(exist_test)

set.seed(107)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3, search='random')
SVMmod <- train(
  Volume ~ PositiveServiceReview,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod2 <- train(
  Volume ~ PositiveServiceReview + x4StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod3 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod4 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod5 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod6 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

SVMmod7 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

SVMmod8 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

SVMmodAll <- train(
  Volume ~ .,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)
#-----For loop to check results----
results <-c()
SVM_prediction_results <- c()
SVMmodels = list(SVMmod,SVMmod2,SVMmod3,SVMmod4,SVMmod5,SVMmod6, SVMmod7, SVMmod8, SVMmodAll)
for (i in 1:9){
  if (i==1){
    results <- c()
  }
  placeholder <- predict(SVMmodels[i], newdata = exist_test)
  placeholder <- unlist(placeholder, use.names=FALSE)
  perf <- postResample(placeholder,exist_test$Volume)
  results <- cbind(results,perf)
  colnames(results)[i] <- paste('SVM',i)
}

results
####### Random Forest ##########

set.seed(107)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3, search='random')
RFmod <- train(
  Volume ~ PositiveServiceReview,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

RFmod2 <- train(
  Volume ~ PositiveServiceReview + x4StarReviews,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

RFmod3 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

RFmod4 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

RFmod5 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

RFmod6 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

RFmod7 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

RFmod8 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

RFmod8 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

RFmodAll <- train(
  Volume ~ .,
  data=exist_train,
  method='rf',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100,
  importance=TRUE
)


#-----For loop to check results----
resultsRF <-c()
RF_prediction_results <- c()
RFmodels = list(RFmod,RFmod2,RFmod3,RFmod4,RFmod5,RFmod6, RFmod7, RFmod8, RFmodAll)
for (i in 1:9){
  if (i==1){
    results <- c()
  }
  placeholderRF <- predict(RFmodels[i], newdata = exist_test)
  placeholderRF <- unlist(placeholderRF, use.names=FALSE)
  perfRF <- postResample(placeholderRF,exist_test$Volume)
  resultsRF <- cbind(resultsRF,perfRF)
  colnames(resultsRF)[i] <- paste('RF',i)
}

resultsRF

####### XGBT #######

set.seed(107)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3, search='random')

xgbTreemodAll <- train(
  Volume ~ .,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
  #importance=TRUE
)

xgbTreemod <- train(
  Volume ~ PositiveServiceReview,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
)

xgbTreemod2 <- train(
  Volume ~ PositiveServiceReview + x4StarReviews,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
)

xgbTreemod3 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
)

xgbTreemod4 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
)

xgbTreemod5 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100,
  ntrees=100
)

xgbTreemod6 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100,
  ntrees=100
)

xgbTreemod7 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100,
  ntrees=100
)

xgbTreemod8 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='xgbTree',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100,
  ntrees=100
)


#-----For loop to check results----
resultsxgbTree <-c()
xgbTree_prediction_results <- c()
RME <- c()
xgbTreemodels = list(xgbTreemod,xgbTreemod2,xgbTreemod3,xgbTreemod4,
                     xgbTreemod5,xgbTreemod6, xgbTreemod7, xgbTreemod8, xgbTreemodAll)
for (i in 1:9){
  if (i==1){
    results <- c()
  }
  placeholderxgbTree <- predict(xgbTreemodels[i], newdata = exist_test)
  placeholderxgbTree <- unlist(placeholderxgbTree, use.names=FALSE)
  perfxgbTree <- postResample(placeholderxgbTree,exist_test$Volume)
  resultsxgbTree <- cbind(resultsxgbTree,perfxgbTree)
  colnames(resultsxgbTree)[i] <- paste('xgbTree',i)
}

Predictions_xgbTree6 <- predict(xgbTreemod6, new_data = newprod_dum_NAs_xcor_size_warr)
resultsxgbTree

Predictions_xgbTree6 

newprod_dum_NAs_xcor_size_warr$PredVolumeSVM <- predict(SVMmod7, newdata=newprod_dum_NAs_xcor_size_warr)
newprod_dum_NAs_xcor_size_warr$PredVolumeXGBT <- predict(xgbTreemod7, newdata=newprod_dum_NAs_xcor_size_warr)
newprod_dum_NAs_xcor_size_warr$PredVolumeRF <- predict(RFmodAll, newdata=newprod_dum_NAs_xcor_size_warr)


newprod_dum_NAs_xcor_size_warr$Volume <- predict(RFmodAll, newdata=newprod_dum_NAs_xcor_size_warr)
importance(RFmodAll)
varImp(RFmodAll)

# Boxplot of Predicted Volumes
plot_ly(y=existprod_dum_NAs_xcor_size_warr[,19], type="box", name='Existing products') %>% 
  add_boxplot(y=newprod_dum_NAs_xcor_size_warr[,19], type="box", name='New products') %>%
  layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size_warr[19]),'for Existing and New Products'))


  
write.csv(results,  file="SVM.csv")
write.csv(resultsRF,  file="RF.csv")
write.csv(resultsxgbTree,  file="xgbTree.csv")
write.csv(newprod_dum_NAs_xcor_size_warr, file="NewProd.csv")
