
#Install Packages
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tibble)) install.packages("tibble")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(partitions)) install.packages("partitions")
if(!require(iterpc)) install.packages("iterpc")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(car)) install.packages("car")
if(!require(rpart)) install.packages("rpart")
if(!require(performanceEstimation)) install.packages("performanceEstimation")
if(!require(randomForest)) install.packages("randomForest")
if(!require(MASS)) install.packages("MASS")
if(!require(mgcv)) install.packages("mgcv")
if(!require(mlbench)) install.packages("mlbench")
if(!require(corrplot)) install.packages("corrplot")
if(!require(Hmisc)) install.packages("Hmisc")
if(!require(formattable)) install.packages("formattable")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(varImp)) install.packages("varImp")
if(!require(plotmo)) install.packages("plotmo")
if(!require(ggiraph)) install.packages("ggiraph")
if(!require(ggiraphExtra)) install.packages("ggiraphExtra")
if(!require(ggiraphExtra)) install.packages("formatR")



# Library Packages 
library(formatR)
library(PerformanceAnalytics)
library(lubridate)
library(dplyr)
library(tibble)
library(stringr)
library(caret)
library(tidyr)
library(tidyverse)
library(partitions)
library(iterpc)
library(gridExtra)
library(ggplot2)
library(car)
library(rpart)
library(performanceEstimation)
library(randomForest)
library(caret)
library(MASS)
library(mgcv) # GAM
library(mlbench)
library(corrplot)
library(Hmisc)
library(formattable)
library(rpart.plot)
library(varImp)
library(plotmo)
library(ggiraph)
library(ggiraphExtra)
library(formattable)

# Define the RMSE function to test the result
RMSE <- function(True_Rings, Predicted_Rings){
   sqrt(mean((True_Rings - Predicted_Rings)^2))}

#####################################################################################
# Note the code assumes the Data is downloaded and saved to the working directory
#####################################################################################
# Read in the data 
full_data <- read.csv("abalone.csv")


# Create the training and testing subsets
set.seed(5)
test_index <- createDataPartition(full_data$Rings, times = 1, p = 0.1, list = FALSE)
train_set <- full_data %>% slice(-test_index)
test_set <- full_data %>% slice(test_index)


######################
# Data Exploration
######################
# Explore the data
formattable(as.table(summary(full_data)),format = "markdown")

# Row and Column Counts
dim(full_data)

# Classes of each Column
as.matrix(sapply(full_data,class))

# Column Names 
names(full_data)

# Check for N/A's  
as.matrix(apply(full_data, 2, function(x) any(is.na(x))))

# Examine the first 5 rows  
formattable(full_data[1:5,] , format = "markdown")



# Density Plot of Rings
S1 <- full_data %>% ggplot(aes(Rings, color = Sex)) %>% 
   + geom_density(size=1.5) %>%
   + xlab("Ring Count") %>%
   + ylab("Density") %>%
   + ggtitle("Density Plot Rings by Sex") %>%
   + scale_color_hue(labels = c("Female", "Infant","Male")) %>%
   + scale_colour_brewer(palette = "RdBu") %>%
   + theme_dark()

# Boxplot of Rings by Sex
S2 <-  full_data %>% ggplot(aes(x=Sex,y=Rings, color = Sex))  %>% 
   + geom_boxplot(size = 1.2) %>%
   + xlab("Sex") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Boxplots of Rings by Sex") %>%  
   + geom_hline(aes(yintercept=median(Rings, na.rm = FALSE))
                , linetype = 5, color = "black",size = .9) %>%
   + geom_hline(aes(yintercept=mean(Rings, na.rm = FALSE))
                , linetype = 1, color = "black",size = .9) %>%
   + scale_colour_brewer(palette = "RdBu") %>%
   + theme_dark()  

# Histogram by Rings
S3 <-  full_data %>% ggplot(aes(Rings,color = Sex))  %>% 
   + geom_histogram(size = 1.2 , alpha=0.5,binwidth=1) %>%
   + xlab("Sex") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Histogram of Rings") %>%  
   + scale_colour_brewer(palette = "RdBu") %>%
   + scale_x_continuous(breaks = seq(0, 30, by = 1)) %>%
   + theme_dark()  

# Arrange the Plots
grid.arrange(S1, S2, S3, ncol = 2)


# Length plot
L <-  full_data %>% ggplot(aes(Length, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Shell Length") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Ring Count vs Length") %>%
   + scale_colour_brewer(palette = "RdBu",guide=FALSE) %>%  
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)
# Diameter plot
D <- full_data %>% ggplot(aes(Diameter, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Shell Diameter") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Ring Count vs Diameter") %>%
   + scale_colour_brewer(palette = "RdBu",guide=FALSE) %>%
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)
# Height plot
H <- full_data %>% ggplot(aes(Height, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Shell Height") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Ring Count vs Height") %>%
   + scale_color_hue(labels = c("Female", "Infant", "Male")) %>%
   + scale_colour_brewer(palette = "RdBu") %>%
   + theme_dark() %>%
   +  stat_smooth(method = "lm", se = FALSE)

# Arrange the plots
grid.arrange(L, D,H, widths = c(1.65, 1.65,2))


# Whole Weight plot
WW <-  full_data %>% ggplot(aes(Whole.weight, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Whole.weight") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Whole.weight") %>%
   + scale_colour_brewer(palette = "RdBu",guide=FALSE) %>%  
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)
# Shucked Weight plot
SUW <- full_data %>% ggplot(aes(Shucked.weight, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Shucked.weight ") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Shucked.weight ") %>%
   + scale_color_hue(labels = c("Female", "Infant", "Male")) %>%    
   + scale_colour_brewer(palette = "RdBu") %>%
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)
# Viscera Weight plot
VW <- full_data %>% ggplot(aes(Viscera.weight, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Viscera.weight ") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Viscera.weight ") %>%
   + scale_colour_brewer(palette = "RdBu",guide=FALSE) %>%  
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)
# Shell Weight plot
SEW <- full_data %>% ggplot(aes(Shell.weight, Rings, color = Sex)) %>%
   + geom_point() %>%
   + xlab("Shell.weight") %>%
   + ylab("Ring Count") %>%
   + ggtitle("Shell.weight") %>%
   + scale_color_hue(labels = c("Female", "Infant", "Male")) %>%    
   + scale_colour_brewer(palette = "RdBu") %>%
   + theme_dark()%>%
   +  stat_smooth(method = "lm", se = FALSE)

# Arrange the plots
grid.arrange(WW, SUW,VW,SEW ,ncol = 2, widths = c(1.75,2),heights = c(2,2))

my_data <- full_data[, 2:9]
chart.Correlation(my_data, histogram=TRUE, pch=2) 

# Create 3 separate subsets, one for each Sex
full_M <- full_data %>% filter(Sex == "M")
full_I <- full_data %>% filter(Sex == "I")
full_F <- full_data %>% filter(Sex == "F")

# Calculate the correlation with Rings of each variable individually split by Sex
Correltations_with_rings <-  as.data.frame(rbind(
   c(Variable = "Length"
     ,Correlation = round(cor(full_data$Rings,full_data$Length),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Length),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Length),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Length),4)),    
   c(Variable = "Diameter"
     ,Correlation = round(cor(full_data$Rings,full_data$Diameter),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Diameter),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Diameter),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Diameter),4)),  
   c(Variable = "Height"
     ,Correlation = round(cor(full_data$Rings,full_data$Height),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Height),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Height),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Height),4)),  
   c(Variable = "Whole.weight"
     ,Correlation = round(cor(full_data$Rings,full_data$Whole.weight),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Whole.weight),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Whole.weight),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Whole.weight),4)),           
   c(Variable = "Shucked.weight"
     ,Correlation = round(cor(full_data$Rings,full_data$Shucked.weight),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Shucked.weight),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Shucked.weight),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Shucked.weight),4)),           
   c(Variable = "Viscera.weight"
     ,Correlation = round(cor(full_data$Rings,full_data$Viscera.weight),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Viscera.weight),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Viscera.weight),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Viscera.weight),4)),    
   c(Variable = "Shell.weight"
     ,Correlation = round(cor(full_data$Rings,full_data$Shell.weight),4)
     ,Correlation_Male = round(cor(full_M$Rings,full_M$Shell.weight),4)
     ,Correlation_Female = round(cor(full_F$Rings,full_F$Shell.weight),4)
     ,Correlation_Infant = round(cor(full_I$Rings,full_I$Shell.weight),4))))

# Format the table
formattable(Correltations_with_rings,align =c("l","l","l", "l","l")
            ,format = "markdown",
            list(`Variable` = formatter("span"
                                        ,style = ~ style(color = "black",font.weight = "bold"))))

######################
# Base Models
######################

# Linear regression
# Train the Linear Regression model using the train_set  
set.seed(1) 
lm.Rings <- lm(Rings ~., data = train_set)
summary(lm.Rings)  


# Stepwise Regression using MASS

# Improve the model by performing a backwards stepwise regression
lm.step.Rings  <- stepAIC(lm.Rings, direction="backward")
summary(lm.step.Rings)
#length is removed as a predictor variable

# Compare the two models
anova(lm.Rings, lm.step.Rings)

#F statistic is high so we cannot reject the null hypothesis that the models are different

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm.step.Rings)

# 1 Linear Regression
lm.predictions.Rings <- predict(lm.step.Rings, test_set)
lm.RMSE <- RMSE(lm.predictions.Rings,test_set$Rings)
lm.RMSE

set.seed(1) 
#Train the Model
rt.Rings <- train(Rings ~ ., 
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)),
                  data = train_set)
rt.Rings

#Plot the Regression Tree with Rpart Plot
rpart.plot(rt.Rings$finalModel, type = 2, extra =1,shadow.col = "darkgray",tweak = 1.3)

# 2 Regression Tree using R part
rt.predictions.Rings <- predict(rt.Rings, test_set)
rt.RMSE <- RMSE(rt.predictions.Rings,test_set$Rings)
rt.RMSE

# Modal Tree Using Cubist

# Set the grid to train the model
mt.grid <- expand.grid(committees = c(1, 10, 50, 100),
                       neighbors = c(0, 1, 5, 9))
set.seed(1) 
# Set the grid to train the model
Modal_Tree.Rings <- train(Rings ~ ., 
                          method = "cubist", 
                          data = train_set,   
                          tuneGrid = mt.grid,
                          trControl = trainControl(method = "cv")
)

# Final Model  
Modal_Tree.Rings
Modal_Tree.Rings$finalModel

# 2 Regression Tree using R part
Modal_tree.predictions.Rings <- predict(Modal_Tree.Rings, test_set)
Modal_tree.RMSE <- RMSE(Modal_tree.predictions.Rings,test_set$Rings)
Modal_tree.RMSE

# Random Forrest

set.seed(2) 
rf.Rings <- randomForest(Rings ~., data = train_set,ntree=500)
rf.Rings

# 4 Random Forrest RMSE
rf.predictions.Rings <- predict(rf.Rings, test_set)
rf.RMSE <- RMSE(rf.predictions.Rings,test_set$Rings)
rf.RMSE

# KNN

set.seed(1) 
ctrl <- trainControl(method="repeatedcv",repeats = 10)

knn_grid  <- data.frame(k = seq(3, 30, 2))
knn.Rings <- train(Rings ~ ., 
                   method = "knn", 
                   data = train_set,
                   tuneGrid = knn_grid
)

# Model Training results
formattable(knn.Rings$results,format = "markdown")

# Final Model Chosen
knn.Rings$finalModel

# 5 K Nearest Neighbours RMSE
knn.predictions.Rings <- predict(knn.Rings, test_set)
knn.RMSE <- RMSE(knn.predictions.Rings,test_set$Rings)
knn.RMSE

# Generalized Additive Model using Splines 
set.seed(1) 
gam.Rings <- train(Rings ~ ., method = "gam", data = train_set)
summary(gam.Rings)
gam.Rings$finalModel	 

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(gam.Rings$finalModel)

# Generalized Additive Model using Splines 
gam.predictions.Rings <- predict(gam.Rings, test_set)
gam.predictions.Rings.RMSE <- RMSE(gam.predictions.Rings,test_set$Rings)
gam.predictions.Rings.RMSE


# Create a grid needed to pick the tuning parameters via cross validation
hyper_grid <- expand.grid(
   degree = 1:3, 
   nprune = seq(2, 100, length.out = 10) %>% floor()
)

earth.Rings_Tuned <- train(
   Rings ~ .,
   method = "earth",
   metric = "RMSE",
   trControl = trainControl(method = "cv", number = 10),
   tuneGrid = hyper_grid,
   data = train_set
)

#  Show the best model best tune
earth.Rings_Tuned$bestTune

#  Show the final model
earth.Rings_Tuned$finalModel
summary(earth.Rings_Tuned$finalModel)


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot( earth.Rings_Tuned$finalModel)

# Multivariate Adaptive Regression Splines MARS 
earth.predictions.Rings <- predict(earth.Rings_Tuned, test_set)
earth.RMSE <- RMSE(earth.predictions.Rings,test_set$Rings) 
earth.RMSE

# Make predictions on test set and compare RMSE

model_results <- as.data.frame(
   
   rbind(
      c(model = "Linear Regression"
        , RMSE =  round(lm.RMSE,6))
      ,c(model = "Regression Tree using rpart"
         , RMSE =  round(rt.RMSE,6))
      ,c(model = "Modal Tree using Cubist"
         , RMSE = round(Modal_tree.RMSE,6))
      ,c(model = "Random Forrest"
         , RMSE =  round(rf.RMSE,6))
      ,c(model = "K Nearest Neighbours"
         , RMSE =  round(knn.RMSE,6))
      ,c(model = "Generalized Additive Model using Splines GAM"
         , RMSE =  round(gam.predictions.Rings.RMSE,6))
      ,c(model = "Multivariate Adaptive Regression Splines MARS"
         , RMSE =  round(earth.RMSE,6)) 
   )
)    

######################
# Ensemble Models
######################


formattable(model_results, format = "markdown")


# Matrix of predictions of each of the base models for the test set data
preds_v1 <- as.data.frame(cbind(lm.predictions.Rings
                                ,rt.predictions.Rings
                                ,Modal_tree.predictions.Rings
                                ,rf.predictions.Rings
                                ,knn.predictions.Rings
                                ,gam.predictions.Rings
                                ,earth.predictions.Rings))

names(preds_v1) <- c("lm","rpart","Cubist","rf","knn","gam","Mars")


# Ensemble 1 Predictions
ens_v1.pred = as.matrix(rowMeans(preds_v1))

#Ensemble 1 RMSE
ens_v1.RMSE <- RMSE(ens_v1.pred,test_set$Rings)   
ens_v1.RMSE


# Ensemble Cross Validations

# Create 5 folds from the train set data  
set.seed(5) 
train_set_index_cv <- createDataPartition(train_set$Rings
                                          , times = 5, p = 0.1, list = FALSE)
train_cv1 <- train_set %>% slice(-train_set_index_cv[,1])
test_cv1 <- train_set %>% slice(train_set_index_cv[,1])

train_cv2 <- train_set %>% slice(-train_set_index_cv[,2])
test_cv2 <- train_set %>% slice(train_set_index_cv[,2])

train_cv3 <- train_set %>% slice(-train_set_index_cv[,3])
test_cv3 <- train_set %>% slice(train_set_index_cv[,3])

train_cv4 <- train_set %>% slice(-train_set_index_cv[,4])
test_cv4 <- train_set %>% slice(train_set_index_cv[,4])

train_cv5 <- train_set %>% slice(-train_set_index_cv[,5])
test_cv5 <- train_set %>% slice(train_set_index_cv[,5])


###############################################
# This code can take several minutes to run
###############################################     

# Fit each model to the train_cv folds
# Predict on each of the test_cv folds

# Backwards Stepped Linear Regression Model

lm.ens.cv1 <- stepAIC(lm(Rings ~., data = train_cv1), direction="backward")
lm.ens.cv2 <- stepAIC(lm(Rings ~., data = train_cv2), direction="backward")
lm.ens.cv3 <- stepAIC(lm(Rings ~., data = train_cv3), direction="backward")
lm.ens.cv4 <- stepAIC(lm(Rings ~., data = train_cv4), direction="backward")
lm.ens.cv5 <- stepAIC(lm(Rings ~., data = train_cv5), direction="backward")

pred_lm.ens.cv1 <- predict(lm.ens.cv1, test_cv1)
pred_lm.ens.cv2 <- predict(lm.ens.cv2, test_cv2)
pred_lm.ens.cv3 <- predict(lm.ens.cv3, test_cv3)
pred_lm.ens.cv4 <- predict(lm.ens.cv4, test_cv4)
pred_lm.ens.cv5 <- predict(lm.ens.cv5, test_cv5)

# rpart Regression Tree

rt.ens.cv1 <- train(Rings ~ ., method = "rpart"
                    , tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), data = train_cv1)   
rt.ens.cv2 <- train(Rings ~ ., method = "rpart"
                    , tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), data = train_cv2)   
rt.ens.cv3 <- train(Rings ~ ., method = "rpart"
                    , tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), data = train_cv3)   
rt.ens.cv4 <- train(Rings ~ ., method = "rpart"
                    , tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), data = train_cv4)   
rt.ens.cv5 <- train(Rings ~ ., method = "rpart"
                    , tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), data = train_cv5)   

pred_rt.ens.cv1 <- predict(rt.ens.cv1, test_cv1)
pred_rt.ens.cv2 <- predict(rt.ens.cv2, test_cv2)
pred_rt.ens.cv3 <- predict(rt.ens.cv3, test_cv3)
pred_rt.ens.cv4 <- predict(rt.ens.cv4, test_cv4)
pred_rt.ens.cv5 <- predict(rt.ens.cv5, test_cv5)

# Modal Tree using Cubist

Modal_Tree.ens.cv1 <- train(Rings ~ ., method = "cubist"
                            , data = train_cv1, tuneGrid = mt.grid
                            ,trControl = trainControl(method = "cv"))
Modal_Tree.ens.cv2 <- train(Rings ~ ., method = "cubist"
                            , data = train_cv2, tuneGrid = mt.grid
                            ,trControl = trainControl(method = "cv"))
Modal_Tree.ens.cv3 <- train(Rings ~ ., method = "cubist"
                            , data = train_cv3, tuneGrid = mt.grid
                            ,trControl = trainControl(method = "cv"))
Modal_Tree.ens.cv4 <- train(Rings ~ ., method = "cubist"
                            , data = train_cv4, tuneGrid = mt.grid
                            ,trControl = trainControl(method = "cv"))
Modal_Tree.ens.cv5 <- train(Rings ~ ., method = "cubist"
                            , data = train_cv5, tuneGrid = mt.grid
                            ,trControl = trainControl(method = "cv"))

pred_Modal_Tree.ens.cv1 <- predict(Modal_Tree.ens.cv1, test_cv1)
pred_Modal_Tree.ens.cv2 <- predict(Modal_Tree.ens.cv2, test_cv2)
pred_Modal_Tree.ens.cv3 <- predict(Modal_Tree.ens.cv3, test_cv3)
pred_Modal_Tree.ens.cv4 <- predict(Modal_Tree.ens.cv4, test_cv4)
pred_Modal_Tree.ens.cv5 <- predict(Modal_Tree.ens.cv5, test_cv5)

# Random Forrest  

rf.cv1 <- randomForest(Rings ~., data = train_cv1,ntree=500)
rf.cv2 <- randomForest(Rings ~., data = train_cv2,ntree=500)   
rf.cv3 <- randomForest(Rings ~., data = train_cv3,ntree=500)   
rf.cv4 <- randomForest(Rings ~., data = train_cv4,ntree=500)   
rf.cv5 <- randomForest(Rings ~., data = train_cv5,ntree=500) 

pred_rf.ens.cv1 <- predict(rf.cv1, test_cv1)
pred_rf.ens.cv2 <- predict(rf.cv2, test_cv2)
pred_rf.ens.cv3 <- predict(rf.cv3, test_cv3)
pred_rf.ens.cv4 <- predict(rf.cv4, test_cv4)
pred_rf.ens.cv5 <- predict(rf.cv5, test_cv5) 

# K Nearest Neighbours KNN

knn.cv1 <- train(Rings ~ .,method = "knn", data = train_cv1,tuneGrid = knn_grid)
knn.cv2 <- train(Rings ~ .,method = "knn", data = train_cv2,tuneGrid = knn_grid)
knn.cv3 <- train(Rings ~ .,method = "knn", data = train_cv3,tuneGrid = knn_grid)
knn.cv4 <- train(Rings ~ .,method = "knn", data = train_cv4,tuneGrid = knn_grid)
knn.cv5 <- train(Rings ~ .,method = "knn", data = train_cv5,tuneGrid = knn_grid)

pred_knn.ens.cv1 <- predict(knn.cv1, test_cv1)
pred_knn.ens.cv2 <- predict(knn.cv2, test_cv2)
pred_knn.ens.cv3 <- predict(knn.cv3, test_cv3)
pred_knn.ens.cv4 <- predict(knn.cv4, test_cv4)
pred_knn.ens.cv5 <- predict(knn.cv5, test_cv5) 

# Generalized Additive Model using Splines (GAM)

gam.cv1 <- train(Rings ~ ., method = "gam", data = train_cv1)
gam.cv2 <- train(Rings ~ ., method = "gam", data = train_cv2)
gam.cv3 <- train(Rings ~ ., method = "gam", data = train_cv3)
gam.cv4 <- train(Rings ~ ., method = "gam", data = train_cv4)
gam.cv5 <- train(Rings ~ ., method = "gam", data = train_cv5)

pred_gam.ens.cv1 <- predict(gam.cv1, test_cv1)
pred_gam.ens.cv2 <- predict(gam.cv2, test_cv2)
pred_gam.ens.cv3 <- predict(gam.cv3, test_cv3)
pred_gam.ens.cv4 <- predict(gam.cv4, test_cv4)
pred_gam.ens.cv5 <- predict(gam.cv5, test_cv5)    

### Multivariate Adaptive Regression Splines (MARS) 

earth.cv1 <- train(Rings ~ .,method = "earth",metric = "RMSE"
                   ,trControl = trainControl(method = "cv"
                                             , number = 10),tuneGrid = hyper_grid,data = train_cv1)
earth.cv2 <- train(Rings ~ .,method = "earth",metric = "RMSE"
                   ,trControl = trainControl(method = "cv"
                                             , number = 10),tuneGrid = hyper_grid,data = train_cv2)
earth.cv3 <- train(Rings ~ .,method = "earth",metric = "RMSE"
                   ,trControl = trainControl(method = "cv"
                                             , number = 10),tuneGrid = hyper_grid,data = train_cv3) 
earth.cv4 <- train(Rings ~ .,method = "earth",metric = "RMSE"
                   ,trControl = trainControl(method = "cv"
                                             , number = 10),tuneGrid = hyper_grid,data = train_cv4)
earth.cv5 <- train(Rings ~ .,method = "earth",metric = "RMSE"
                   ,trControl = trainControl(method = "cv"
                                             , number = 10),tuneGrid = hyper_grid,data = train_cv5)

pred_earth.ens.cv1 <- predict(earth.cv1, test_cv1)
pred_earth.ens.cv2 <- predict(earth.cv2, test_cv2)
pred_earth.ens.cv3 <- predict(earth.cv3, test_cv3)
pred_earth.ens.cv4 <- predict(earth.cv4, test_cv4)
pred_earth.ens.cv5 <- predict(earth.cv5, test_cv5)    



# Create a matrix of predictions
cv1 <- as.matrix(cbind(pred_lm.ens.cv1
                       ,pred_rt.ens.cv1
                       ,pred_Modal_Tree.ens.cv1
                       ,pred_rf.ens.cv1
                       ,pred_knn.ens.cv1
                       ,pred_gam.ens.cv1
                       ,pred_earth.ens.cv1
                       ,test_cv1$Rings))

cv2 <- as.matrix(cbind(pred_lm.ens.cv2
                       ,pred_rt.ens.cv2
                       ,pred_Modal_Tree.ens.cv2
                       ,pred_rf.ens.cv2
                       ,pred_knn.ens.cv2
                       ,pred_gam.ens.cv2
                       ,pred_earth.ens.cv2
                       ,test_cv2$Rings))                          

cv1 <- as.matrix(cbind(pred_lm.ens.cv1
                       ,pred_rt.ens.cv1
                       ,pred_Modal_Tree.ens.cv1
                       ,pred_rf.ens.cv1
                       ,pred_knn.ens.cv1
                       ,pred_gam.ens.cv1
                       ,pred_earth.ens.cv1
                       ,test_cv1$Rings))

cv3 <- as.matrix(cbind(pred_lm.ens.cv3
                       ,pred_rt.ens.cv3
                       ,pred_Modal_Tree.ens.cv3
                       ,pred_rf.ens.cv3
                       ,pred_knn.ens.cv3
                       ,pred_gam.ens.cv3
                       ,pred_earth.ens.cv3
                       ,test_cv3$Rings))   

cv4 <- as.matrix(cbind(pred_lm.ens.cv4
                       ,pred_rt.ens.cv4
                       ,pred_Modal_Tree.ens.cv4
                       ,pred_rf.ens.cv4
                       ,pred_knn.ens.cv4
                       ,pred_gam.ens.cv4
                       ,pred_earth.ens.cv4
                       ,test_cv4$Rings))   

cv5 <- as.matrix(cbind(pred_lm.ens.cv5
                       ,pred_rt.ens.cv5
                       ,pred_Modal_Tree.ens.cv5
                       ,pred_rf.ens.cv5
                       ,pred_knn.ens.cv5
                       ,pred_gam.ens.cv5
                       ,pred_earth.ens.cv5
                       ,test_cv5$Rings))  
# Combine in a single dataframe
full_set_cv <- as.data.frame(rbind(cv1,cv2,cv3,cv4,cv5))
names(full_set_cv) <- c("lm","rpart","Cubist","rf","knn","gam","Mars","Rings")

# Train the Ensemble 2 Linear Regression model
# Use stepwise model selection 'stepAIC'
ens.2.lm <- stepAIC(lm(Rings ~., data = full_set_cv), direction="both")

#Summary Linear Regression
summary(ens.2.lm)

#Ensemble 2 Predictions 
# Predict using the predictions of each base models predictions on the full test set data 
ens.2.pred <- predict(ens.2.lm, preds_v1)
ens.2.RMSE <- RMSE(ens.2.pred,test_set$Rings)

#Ensemble 2 RMSE
ens.2.RMSE

######################
# RESULTS
######################

# Show the RMSE scores of each model in a single table
model_results_FINAL <- as_tibble(
   
   rbind(
      c(model = "1 Linear Regression"
        , RMSE =  round(lm.RMSE,4))
      ,c(model = "2 Regression Tree using rpart"
         , RMSE =  round(rt.RMSE,4))
      ,c(model = "3 Modal Tree using Cubist"
         , RMSE = round(Modal_tree.RMSE,4))
      ,c(model = "4 Random Forrest"
         , RMSE =  round(rf.RMSE,4))
      ,c(model = "5 K Nearest Neighbours"
         , RMSE =  round(knn.RMSE,4))
      ,c(model = "6 Generalized Additive Model using Splines GAM"
         , RMSE =  round(gam.predictions.Rings.RMSE,4))
      ,c(model = "7 Multivariate Adaptive Regression Splines MARS"
         , RMSE =  round(earth.RMSE,4)) 
      ,c(model = "Ensemble 1 - average of base model predictions"
         , RMSE =  round(ens_v1.RMSE,4)) 
      ,c(model = "Ensemble 2 -Stepped Linear Regression of base model predictions"
         , RMSE =  round(ens.2.RMSE,4)) 
   )
)  
# Format the table
formattable(model_results_FINAL ,align = c("l","r")
            , format = "markdown",list(`model` = formatter(
               "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))


ENS_2_FULL <-   cbind(test_set,ens.2.pred)
ENS_2_FULL <- as.data.frame(cbind(test_set$Rings,ens.2.pred))
ENS_2_FULL <- cbind(test_set$Sex,ENS_2_FULL)
names(ENS_2_FULL) <- c("Sex","Rings","Predictions")

#RMSE For Male Abalone only
ENS_2_M <- ENS_2_FULL %>%
   filter(Sex == "M")

RMSE_M <- RMSE(ENS_2_M$Predictions,ENS_2_M$Rings) 

#RMSE For Female Abalone only
ENS_2_F <- ENS_2_FULL %>%
   filter(Sex == "F")

RMSE_F <- RMSE(ENS_2_F$Predictions,ENS_2_F$Rings) 

#RMSE For Infant Abalone only
ENS_2_I <- ENS_2_FULL %>%
   filter(Sex == "I")

RMSE_I <- RMSE(ENS_2_I$Predictions,ENS_2_I$Rings)

RMSE_BY_SEX <- as_tibble(
   
   as.data.frame(rbind(
      c(Sex = "Female", RMSE =  round(RMSE_F,3))
      ,c(Sex = "Male", RMSE =  round(RMSE_M,3))
      ,c(Sex = "Infant", RMSE = round(RMSE_I,3))
   ))
)       

formattable(RMSE_BY_SEX,align = c("l","l"),format = "markdown")



