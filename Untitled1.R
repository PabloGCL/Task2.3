pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, ddalpha, 
               DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, corrplot)


################## Calling Documents #####################

setwd("C:/Users/Usuario/Desktop/Big Data/2.3") 

OGexisting <- read_delim("existingproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)
View(OGexisting)

OGnew <- read_delim("newproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)
View(OGnew)


################## Dummify the data ######################

existing <- dummyVars(" ~ .", data = OGexisting)
existing <- data.frame(predict(existing, newdata = OGexisting))

newDummy <- dummyVars(" ~ .", data = OGnew)
newDummy <- data.frame(predict(newDummy, newdata = OGnew))

str(existing)
str(newDummy)

existing$BestSellersRank <- NULL
existing$x5StarReviews <- NULL

newDummy$BestSellersRank <- NULL
newDummy$x5StarReviews <- NULL


################## Correlation ###################

corrExist <- cor(existing) 
corrplot(corrExist)


#################### Decision Tree ##########################

set.seed(123)
DecisionTree <- rpart(
  Volume ~ ., 
  data = existing, 
  control = rpart.control(minsplit = 2)
)

par(xpd = NA, mar = rep(0.7, 4)) 
plot(DecisionTree, compress = TRUE)
text(DecisionTree, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)

rpart.plot(DecisionTree) #, box.palette="RdBu")


#################### Linear Model ##################

set.seed(123)
inTrain <- createDataPartition(
  y = existing$Volume,
  p = .75,
  list = FALSE)

str(inTrain)

training <- existing[ inTrain,]
testing  <- existing[-inTrain,]

nrow(training)
nrow(testing)


LMmodel <- list()
for(i in 12:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel <- lm(Volume ~ 0+training[,i], training)
    #fit <- train(brand~salary_bin+age_bin, data=training, method="rf", 
    #            metric="Kappa", tuneLength=10, trControl=fitControl, 
    #           ntree=ntree)
    #key <- toString(ntree) 
    
    LMmodel[[i]] <- LinearModel
    names(LMmodel)[i] <- colnames(existing[i])
  }
}

LMmodel