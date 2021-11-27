rm(list=ls())
library(data.table)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(nnet)

dataset <- fread("Class 1 Team 3 match_outcome_model.csv", header = T)
dataset[, `:=`(match_api_id=NULL,
               home_team_long_name=NULL,
               away_team_long_name=NULL,
               B365_Defeat=NULL,
               B365_Draw=NULL,
               BW_Defeat=NULL,
               BW_Draw=NULL,
               B365_Win=NULL)]

sum(is.na(dataset))
set.seed(2021)  # for Bootstrap sampling & RSF selection.
train <- sample.split(Y = dataset$label, SplitRatio = 0.7)
trainset <- subset(dataset, train == T)
testset <- subset(dataset, train == F)

m.RF <- randomForest(label ~ . , data = trainset, 
                       na.action = na.omit, 
                       importance = T)

(OOB.error <- m.RF$err.rate[m.RF$ntree, 1])

# Trainset results
y.hat <- predict(m.RF, newdata=trainset, type='class')
(table <- table(trainset$label, y.hat, deparse.level=2))
mean(y.hat==trainset$label)

# Testset results:
# tp = 807 + 101 + 2123 = 3031
# fp = (403+437) + (125+138) + (763+978) = 2844
# fn = (125+763) + (403+978) + (437+138) = 2844
# Precision = tp/(tp+fp) = 3031/(3031+2844) = 0.5159149
# Recall = tp/(tp+fn) = 3031/(3031+2844) = 0.5159149
y.hat <- predict(m.RF, newdata=testset, type='class')
(table <- table(y.hat, testset$label, deparse.level=2))
mean(y.hat==testset$label)


var.impt <- importance(m.RF)

varImpPlot(m.RF, type = 1)

############################# CART ##########################################
auto_prune <- function(cart1){
  CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
  # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
  i <- 1; j<- 4
  while (cart1$cptable[i,j] > CVerror.cap) {
    i <- i + 1
  }
  # Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
  cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
  # Prune the max tree using a particular CP value
  return(prune(cart1, cp = cp.opt))
}


# Max Tree
cart1 <- rpart(label ~ ., data = trainset, method = 'class',
               control = rpart.control(minsplit = 2, cp = 0))
# rpart.plot(cart1)
#printcp(cart1)
plotcp(cart1, main="Subtrees in churn")
#summary(cart1)
#cart1$cptable
#cart1$variable.importance

cart.pruned <- auto_prune(cart1)
printcp(cart.pruned)
summary(cart.pruned)
cart.pruned$variable.importance
cart.pruned$cptable
dev.off()
options(device='windows')
rpart.plot(cart.pruned)

# Trainset results
y.hat <- predict(cart.pruned, newdata=trainset, type='class')
(table <- table(trainset$label, y.hat, deparse.level=2))
mean(y.hat==trainset$label)

# Testset results:
# tp = 1056 + 0 + 1989 = 3045
# fp = (601+709) + (0+0) + (639+881) = 2830
# fn = (0+639) + (601+881) + (709+0) = 2830
# Precision = tp/(tp+fp) = 3045/(3045+2830) = 0.5182979
# Recall = tp/(tp+fn) = 3045/(3045+2830) = 0.5182979
y.hat <- predict(cart.pruned, newdata=testset, type='class')
(table <- table( y.hat, testset$label, deparse.level=2))
mean(y.hat==testset$label)

######################## multi-nomial regression ##############################
# Training the multinomial model
multinom.fit <- multinom(label ~ ., data = trainset)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
results <- t(exp(coef(multinom.fit)))
head(probability.table <- fitted(multinom.fit))

# Trainset results
y.hat <- predict(multinom.fit, newdata=trainset, type='class')
(table <- table(trainset$label, y.hat, deparse.level=2))
mean(y.hat==trainset$label)

# Testset results:
# tp = 857 + 36 + 2211 = 3104
# fp = (434+451) + (47+36) + (791+1012) = 2771
# fn = (47+791) + (434+1012) + (451+36) = 2771
# Precision = tp/(tp+fp) = 3104/(3104+2771) = 0.5283404
# Recall = tp/(tp+fn) = 3104/(3104+2771) = 0.5283404
y.hat <- predict(multinom.fit, newdata=testset, type='class')
(table <- table( y.hat, testset$label, deparse.level=2))
mean(y.hat==testset$label)
