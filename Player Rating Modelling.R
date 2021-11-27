# Player Attributes 
library(data.table)
library(plyr)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(nnet)
library(car)
library(earth)
library(xgboost)
library(readr)
library(stringr)
library(Matrix)
library(ggplot2)
library(vip)
library(pdp)
library(quantreg)

player <- fread("Class 1 Team 3 player_ratings_model.csv", stringsAsFactors = T)
player[player == "null"] <- NA

clean.player = player[,!c('V1','player_name', 'player_fifa_api_id', 'birthday', 
                          'date', 'potential', 'attacking_work_rate', 
                          'defensive_work_rate')]
# remove potential due to possible multi-collinearity 

clean.player = na.omit(clean.player)

sum(is.na(clean.player)) # 0
# 10582 rows, 38 columns

set.seed(2021)

### Linear Regression ###
lrdata = clean.player

lrmodel.full <- lm(overall_rating ~ . , data = lrdata)
lrmodel.1 <- step(lrmodel.full) # backward elimination
summary(lrmodel.1) 
# dribbling, penalties to be removed due to p-values being higher than 0.05

lrmodel.1 <- lm(formula = overall_rating ~ weight + crossing + finishing + 
                  heading_accuracy + short_passing + curve + long_passing + 
                  ball_control + acceleration + sprint_speed + 
                  agility + reactions + shot_power + jumping + stamina + 
                  strength + long_shots + aggression + interceptions + 
                  positioning + marking + gk_diving + gk_handling + gk_kicking + 
                  gk_positioning + gk_reflexes, data = lrdata)

# vif(lrmodel.1)

set.seed(2021)
train <- sample.split(Y = clean.player$overall_rating, SplitRatio = 0.7)
trainset <- subset(clean.player, train == T)
testset <- subset(clean.player, train == F)

summary(trainset$overall_rating) # check distribution of Y in trainset and testset
summary(testset$overall_rating)

lrmodel.train <- lm(formula = overall_rating ~ weight + crossing + finishing + 
                      heading_accuracy + short_passing + curve + long_passing + 
                      ball_control + acceleration + sprint_speed + 
                      agility + reactions + shot_power + jumping + stamina + 
                      strength + long_shots + aggression + interceptions + 
                      positioning + marking + gk_diving + gk_handling + gk_kicking + 
                      gk_positioning + gk_reflexes, data = trainset)
summary(lrmodel.train)
# agility with p-value > 0.05 in trainset - remove 
# followed by long_passing

lrmodel.train <- lm(formula = overall_rating ~ weight + crossing + finishing + 
                      heading_accuracy + short_passing + curve +  
                      ball_control + acceleration + sprint_speed + reactions + 
                      shot_power + jumping + stamina + strength + long_shots + 
                      aggression + interceptions + positioning + marking + gk_diving + 
                      gk_handling + gk_kicking + gk_positioning + gk_reflexes, 
                    data = trainset)

rmse.lrmodel.train <- sqrt(mean(residuals(lrmodel.train)^2))
rmse.lrmodel.train
# 2.979085
summary(abs(residuals(lrmodel.train))) # min Abs Error and max Abs Error

predict.lrmodel.test <- predict(lrmodel.train, newdata = testset)
testset.error <- testset$overall_rating - predict.lrmodel.test

rmse.lrmodel.test <- sqrt(mean(testset.error^2))
rmse.lrmodel.test
# 2.98302
summary(abs(testset.error))

plot(predict.lrmodel.test, testset$overall_rating)
abline(0,1,col="red")

lrvarimpt <- varImp(lrmodel.train, scale = TRUE)
lrvarimpt <- as.data.frame(lrvarimpt)
lrvarimpt <- cbind(newColName = rownames(lrvarimpt), lrvarimpt)
rownames(lrvarimpt) <- 1:nrow(lrvarimpt)
colnames(lrvarimpt) <- c("X_Variables", "Overall")
theme_set(theme_classic())
ggplot(lrvarimpt, aes(x=reorder(X_Variables, Overall), y=Overall)) + 
  geom_bar(stat='identity', width=.5, fill = 'blue')  +
  labs(title= "Variable Importance under Linear Regression (t-value)", x = "Variables", y = "Variable Importance") + 
  coord_flip()


### CART ###
set.seed(2021)

cart1 <- rpart(overall_rating ~ ., data = trainset, method = "anova", control = rpart.control(minsplit = 2, maxdepth = 4, cp = 0))
#print(cart1)
#printcp(cart1)


CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

cart.pruned <- prune(cart1, cp = cp.opt)
print(cart.pruned)
printcp(cart.pruned)
# avg CV error - 39.39 x 0.21931
# 1 SE - 39.39 x 0.0048203

summary(cart.pruned)
cart.pruned$variable.importance
cart.pruned$cptable
dev.off()
options(device='windows')
rpart.plot(cart.pruned)

scaledVarImpt <- round(100*cart.pruned$variable.importance/sum(cart.pruned$variable.importance))
scaledVarImpt[scaledVarImpt > 3] #Scaling Variable Impt so as to rep as percentage impt

cart.predict <- predict(cart.pruned, newdata = testset)
testset.error.cart <- testset$overall_rating - cart.predict
rmse.cart <- sqrt(mean(testset.error.cart^2))
rmse.cart
# 3.362125

svi.df <- as.data.frame(scaledVarImpt)
svi.df <- cbind(newColName = rownames(svi.df), svi.df)
rownames(svi.df) <- 1:nrow(svi.df)
colnames(svi.df) <- c("X_Variables", "ScaledVarImpt")
theme_set(theme_classic())
ggplot(svi.df, aes(x=reorder(X_Variables, ScaledVarImpt), y=ScaledVarImpt)) + 
  geom_bar(stat='identity', width=.5, fill = 'blue')  +
  labs(title= "Variable Importance under CART Model", x = "Variables", y = "Variable Importance (%)") + 
  coord_flip()

### RF ###
set.seed(2021)

rf.model <- randomForest(overall_rating ~ . , data = trainset, importance = T)
rf.model
# OOB MSE - 1.868 
plot(rf.model)

rf.model.yhat <- predict(rf.model, newdata = testset)

RMSE.rf.model <- sqrt(mean((testset$overall_rating - rf.model.yhat)^2))
RMSE.rf.model
# 1.396622

plot(rf.model.yhat, testset$overall_rating)
abline(0,1,col="red")

var.impt.RF <- importance(rf.model)

varImpPlot(rf.model, type = 1)


### MARS ###
# Degree = 1
mars.model.1 <- earth(overall_rating ~ . , data = trainset, degree = 1)
mars.model1.yhat <- predict(mars.model.1, newdata = testset)

RMSE.mars.model1 <- sqrt(mean((testset$overall_rating - mars.model1.yhat)^2))
RMSE.mars.model1
# 2.082952

var.impt.mars1 <- evimp(mars.model.1)
print(var.impt.mars1)
vip(mars.model.1, num_features = 10) + ggtitle("nsubsets")

# Degree = 2
mars.model.2 <- earth(overall_rating ~ . , data = trainset, degree = 2)
mars.model2.yhat <- predict(mars.model.2, newdata = testset)

RMSE.mars.model2 <- sqrt(mean((testset$overall_rating - mars.model2.yhat)^2))
RMSE.mars.model2
# 1.542271

var.impt.mars2 <- evimp(mars.model.2)
print(var.impt.mars2)
vip(mars.model.2, num_features = 9) + ggtitle("nsubsets")

par(mfrow = c(1,2))
plot(testset$overall_rating, mars.model1.yhat)
abline(0,1,col="red")
plot(testset$overall_rating, mars.model2.yhat)
abline(0,1,col="red")

### XGBoost ###
clean.player2 <- clean.player %>% mutate_if(is.integer,as.numeric)
clean.player2$preferred_foot <-as.numeric(factor(clean.player2$preferred_foot))
# left 1, right 2
clean.player2$preferred_foot[clean.player2$preferred_foot == "1"] <- "0"
clean.player2$preferred_foot[clean.player2$preferred_foot == "2"] <- "1"

set.seed(2021)
train2 <- sample.split(Y = clean.player2$overall_rating, SplitRatio = 0.7)
trainset2 <- subset(clean.player2, train2 == T)
testset2 <- subset(clean.player2, train2 == F)

trainset3 = trainset2[, !c("overall_rating")]
trainset3 = as.matrix(trainset3)
trainset3 = as(trainset3, "dgCMatrix")

testset3 = testset2[, !c("overall_rating")]
testset3 = as.matrix(testset3)
testset3 = as(testset3, "dgCMatrix")
# trainset3/testset3 do not have rating, while trainset2/testset2 has

model.xgb <- xgboost(data = trainset3, label = trainset2$overall_rating, 
                     eta = 1, nthread = 2, nrounds = 2, 
                     objective = "reg:squarederror")
pred <- predict(model.xgb, testset3)
xgb.error <- testset2$overall_rating - pred
rmse.xgb <- sqrt(mean(xgb.error^2))
rmse.xgb
# 2.777642
