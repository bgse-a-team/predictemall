# Adding pokemon type and name
#load("~/Desktop/predictemall/R_Environment.RData")
#Pokemon <- read.csv("~/Desktop/predictemall/Pokemon.csv")
#load("~/Desktop/R_Environment_Predictions.RData")
#colnames(Pokemon)[1] <- "class"
#Pokemon[,3] <- as.character(Pokemon[,3])
#data <- merge(data,Pokemon[,1:3],by="class",all=F)
#data[,"class"] <- as.factor(data[,"class"])
#data[,"Type.1"] <- as.factor(data[,"Type.1"])
#data$terrainType <- as.factor(data$terrainType)
#data$pokestopDistanceKm <- as.numeric(data$pokestopDistanceKm)
# Load R Environment and variable list for Random Forests (basically removing variables like id, coordinates, country, etc)
load("./R_Environment.RData")
vars <- read.csv("./vars.csv")
vars <- as.character(vars[,2])

# Fitting the KNN model
  library(class)
  
  ## Create indices for making 5 random sample from the data set
  folds <- sample(cut(seq(1:nrow(data)),breaks=5,labels=F))
  
  ## Initialize error matrix that will contain the error rate for each sample (5 columns) and for each K (from 3 to 15; rows)
  error <- matrix(NA,ncol=5,nrow=13)
  
  ## Cross-valitation: create 5 samples
  for (i in 1:5){
    trainData <- data[-which(folds == i),]
    testData <- data[which(folds == i),]
    
    ## Fit a KNN model for K ranging from 3 to 15
    for (j in 3:15){
      model_knn <- knn(train = trainData[,c(2,3)], test = testData[,c(2,3)], cl = trainData[,"type1"], k = j)
      
      ## Compute the error rate for each of the K
      x <- cbind(as.character(model_knn),as.character(testData[,"type1"]))
      z <- 0
      for (h in 1:nrow(testData)){
        y <- x[h,1] == x[h,2]
        z <- y + z
      }
      error[j-2,i] <- (nrow(testData)-z)/nrow(testData)*100
    }
  }
  
  ## Error matrix in percentage
  round(error*100,1)
  
  ## Compute the mean error rate for each K
  round(apply(error,1,mean),3)*100 # Best perfomance when K = 3
  
  ## Fit again the model for K = 3 to abtain the exact prediction
  data$type1 <- as.factor(data$type1)
  model_knn3 <- knn(train = data[,c(2,3)], test = data[,c(2,3)], cl = data[,"type1"], k = 3)
  



# Fitting Random forest
  library(randomForest)
  ## Selecting variables for fitting the model
 # used_data <- data[,-which(colnames(data) %in% c("class","region","country","pokemonId","latitude",
 #                                                 "longitude","X_id","cellId_90m","cellId_180m",
 #                                                 "cellId_370m","cellId_730m","cellId_1460m",
 #                                                 "cellId_2920m",  "cellId_5850m","appearedLocalTime",
 #                                                 "Name"))]
  used_data <- data[,vars]
 # used_data <- used_data[,-which(colnames(used_data) %in% c("cellId_90m","cellId_180m",
 #                                                           "cellId_370m","cellId_730m","cellId_1460m",
 #                                                           "cellId_2920m",  "cellId_5850m"))]
  used_data$terrainType <- as.factor(used_data$terrainType)
  used_data$pokestopDistanceKm <- as.numeric(used_data$pokestopDistanceKm)

  ## Creating 15 dummies for each of the levels of the outcome (pokemon type)
  levels <- as.data.frame(matrix(NA,ncol=1,nrow=nrow(used_data)))
  colnames(levels) <- "L"
  for (i in 1:length(levels(used_data$type1))){
    levels$L <- rep(FALSE,nrow(used_data))
    levels$L[used_data$type1 == levels(used_data$type1)[i]] <- TRUE
    levels$L <- as.factor(levels$L)
    colnames(levels)[ncol(levels)] <- paste("L",i,sep = "")
  }
  
  ## Fitting the random forest model for each of the categories
  rf1 <- randomForest(levels$L1 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf2 <- randomForest(levels$L2 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf3 <- randomForest(levels$L3 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf4 <- randomForest(levels$L4 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf5 <- randomForest(levels$L5 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf6 <- randomForest(levels$L6 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf7 <- randomForest(levels$L7 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf8 <- randomForest(levels$L8 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf9 <- randomForest(levels$L9 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf10 <- randomForest(levels$L10 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf11 <- randomForest(levels$L11 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf12 <- randomForest(levels$L12 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf13 <- randomForest(levels$L13 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf14 <- randomForest(levels$L14 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  rf15 <- randomForest(levels$L15 ~ .,data=used_data[,-which(names(used_data) == "type1")],na.action=na.omit,ntree=100,importance=T)
  
  ## 4 most impotant predictors for each level (if type = 1, mean decrease in accuracy; if = 2 mean decrease in node impurity)
  importance_predictors <- as.data.frame(rbind(rownames(importance(rf1,type=2))[order(importance(rf1,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf2,type=2))[order(importance(rf2,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf3,type=2))[order(importance(rf3,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf4,type=2))[order(importance(rf4,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf5,type=2))[order(importance(rf5,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf6,type=2))[order(importance(rf6,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf7,type=2))[order(importance(rf7,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf8,type=2))[order(importance(rf8,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf9,type=2))[order(importance(rf9,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf10,type=2))[order(importance(rf10,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf11,type=2))[order(importance(rf11,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf12,type=2))[order(importance(rf12,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf13,type=2))[order(importance(rf13,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf14,type=2))[order(importance(rf14,type=2),decreasing=T)[1:4]],
                                               rownames(importance(rf15,type=2))[order(importance(rf15,type=2),decreasing=T)[1:4]]))
  colnames(importance_predictors) <- c("Importance 1","Importance 2","Importance 3","Importance 4")
  rownames(importance_predictors) <- levels(used_data$type1)
  importance_predictors
  
  ## Error rate of all models given that the class is TRUE
  error_per_type <- as.data.frame(c(round(rf1$confusion[2,3]*100,1),round(rf2$confusion[2,3]*100,1),
                      round(rf3$confusion[2,3]*100,1),round(rf4$confusion[2,3]*100,1),
                      round(rf5$confusion[2,3]*100,1),round(rf6$confusion[2,3]*100,1),
                      round(rf7$confusion[2,3]*100,1),round(rf8$confusion[2,3]*100,1),
                      round(rf9$confusion[2,3]*100,1),round(rf10$confusion[2,3]*100,1),
                      round(rf11$confusion[2,3]*100,1),round(rf12$confusion[2,3]*100,1),
                      round(rf13$confusion[2,3]*100,1),round(rf14$confusion[2,3]*100,1),
                      round(rf15$confusion[2,3]*100,1)))
  rownames(error_per_type) <- levels(used_data$type1)
  colnames(error_per_type) <- "error"
  error_per_type
  
