setwd('C:/R')
voice <- read.csv('voice_info.csv',header = T)
voice[1,] <- gsub(":","",voice[1,])
colnames(voice) = voice[1,]
voice <- voice[-1,]

voice$`Reflux Symptom Index (RSI) Score`
library(stringr)
str(voice$`Occupation status`)
voice$Age = as.numeric(voice$Age)
voice$Gender <- ifelse(voice$Gender == 'f',1,0)
voice$Diagnosis <- ifelse(voice$Diagnosis == 'healthy',1,0)
table(voice$Diagnosis)
voice$`Voice Handicap Index (VHI) Score` <- as.numeric(voice$`Voice Handicap Index (VHI) Score`)
voice$`Reflux Symptom Index (RSI) Score` <- as.numeric(voice$`Reflux Symptom Index (RSI) Score`)
voice$Smoker <- ifelse(voice$Smoker == 'no',0,ifelse(voice$Smoker =='yes',2,1))
voice$`Number of cigarettes smoked per day` <- ifelse(voice$`Number of cigarettes smoked per day` == 'NU',0,voice$`Number of cigarettes smoked per day`)
voice$`Number of cigarettes smoked per day` <-as.numeric(voice$`Number of cigarettes smoked per day`)

voice$`Alcohol consumption` = str_trim(voice$`Alcohol consumption`)
voice$`Alcohol consumption` = ifelse(voice$`Alcohol consumption` == 'nondrinker',0,
                                     ifelse(voice$`Alcohol consumption` =='habitual drinker',2,1))

voice[,12] <- gsub(",",".",voice[,12])
voice[,12] <- as.numeric(voice[,12])

voice <- voice[,-c(5,11,13)]

voice <- voice[,1:10]

voice2 <- read.csv('voice19_28.csv',header = T)
voice2

voice2[1,] <- gsub(":","",voice2[1,])
colnames(voice2) = voice2[1,]
voice2 <- voice2[-1,]
voice2 <- voice2[,15:21]
voice2

voice3 <- read.csv('voice.csv',header = T)
voice3 <-voice3[,-1]
voice3

last <- cbind(voice,voice2,voice3)
last
last2 <- cbind(voice,voice2)
write.csv(last, 'data_voice.csv')
write.csv(last2, 'data_voice2.csv')



setwd('C:/R')
voice <- read.csv('all.csv',header = T)
voice
voice <- voice[,-c(1,2)]
ncol(voice)
voice$Alcohol.consumption
boxplot(voice$Voice.Handicap.Index..VHI..Score~voice$Diagnosis)

table(voice$Tomatoes,voice$Diagnosis)

last2
last2 <- last2[,-1]

library(randomForest)
library(caret)
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(voice), replace=TRUE, prob=c(0.7,0.3))
train  <- voice[sample, ]
test   <- voice[!sample,]
train
train$Diagnosis <- as.character(train$Diagnosis)
train$Diagnosis <- as.factor(train$Diagnosis)

test$Diagnosis <- as.character(test$Diagnosis)
test$Diagnosis <- as.factor(test$Diagnosis)

train$Voice.Handicap.Index..VHI..Score

tree <- randomForest(Diagnosis ~.,proximity=TRUE,data = train)
tree

importance(tree, type=2)

varImpPlot(tree)

p1 <- predict(tree, train)
confusionMatrix(p1, train$Diagnosis)
p2
p2 <- predict(tree, test)
confusionMatrix(p2, test$Diagnosis)
train$
t <- tuneRF(train[,-3], train[,3],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(tree),
     main = "No. of Nodes for the Trees",
     col = "green")


varImpPlot(tree,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

partialPlot(tree, train, Voice.Handicap.Index..VHI..Score,1)
MDSplot(tree, train$Diagnosis)

str(train)
library(xgboost)
library(DiagrammeR)
train$Diagnosis = as.numeric(train$Diagnosis)
train
dtrain <- xgb.DMatrix(data = as.matrix(train[,-3]), label = train$Diagnosis)

xgb <- xgboost(data = as.matrix(train[-3]),label = train$Diagnosis, max.depth = 2, eta = 0.4, nthread = 2,
               nrounds = 3, objective = "binary:hinge")
pred <- predict(xgb,as.matrix(train[,-3]))
pred <- as.factor(pred)
train$Diagnosis <- as.factor(train$Diagnosis)
confusionMatrix(pred, train$Diagnosis)

importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
xgb.plot.tree(model = xgb)

