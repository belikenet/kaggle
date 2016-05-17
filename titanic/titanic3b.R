library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)
library(stringr)
library(caret)
library(leaps)

library(randomForest)
library(rpart)
library(party)
library(knitr)
library(xtable)

train <- read_csv(file = "../titanic/train.csv")
test <- read_csv("../titanic/test.csv")

combi <- train %>% bind_rows(test %>% mutate(Survived = NA))
rows.train <- !is.na(combi$Survived)

combi$Pclass <- factor(combi$Pclass)
combi$Sex <- factor(combi$Sex)

combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = '[,.]')[[1]][2]
})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Capt', 'Col', "Don", "Dr", "Lady", "Major", "Rev", "Sir")] <- 'Noble'
combi$Title[combi$Title %in% c('Dona', 'Ms', 'the Countess')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Jonkheer')] <- 'Mr'

combi$FamilySize <- combi$SibSp + combi$Parch + 1


table(combi$Title, combi$Sex)
tapply(combi$Age, combi$Title, summary)

table(combi$Embarked)
rows.fix.embarked <- is.na(combi$Embarked)
combi$Embarked[rows.fix.embarked] = "S"
combi$Embarked <- factor(combi$Embarked)


rows.fix.fare <- is.na(combi$Fare)
model.fare <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Title + FamilySize + Age,
                data = combi[!rows.fix.fare,], method = "anova")
combi$Fare[rows.fix.fare] <- predict(model.fare, combi[rows.fix.fare,])

combi$Fare.PerPerson <- combi$Fare / combi$FamilySize
combi$Fare.IsComplimentary <- factor(combi$Fare == 0)








subset <- combi[combi$Age >= 14 & combi$Age <= 18 & (combi$Title == "Mrs" || combi$Title == "Mrs"), c("Title", "Sex", "Age", "SibSp", "Parch", "Ticket")]

subset <- combi[combi$Age >= 14 & combi$Age <= 18, c("Title", "Sex", "Age", "SibSp", "Parch", "Ticket")]


bacc <- 0
brb <- 0
trc.recv <- trainControl(method = "repeatedcv", repeats = 10, summaryFunction = twoClassSummary, classProbs = TRUE)
for (i in 1:10)
{
    print(paste(i, "iteration"))
    rb <- createDataPartition(train.pre$Survived, p = 0.8, list = FALSE)
    print(rb[1:50])
    tb <- train.pre[rb,]
    sb <- train.pre[ - rb,]

    tt <- train(Fate ~ Sex + Pclass + Fare.PerPerson + Family.Size + Title + Age + Ticket.Count + Family.ID + Embarked, tb, method = "C5.0", metric = "ROC", trControl = trc.recv, tuneLength = 5)
    pp <- predict(tt, sb)
    cm <- confusionMatrix(pp, sb$Fate, positive = "survived")
    lacc <- cm$overall[["Accuracy"]]
    print (paste("local accuracy:", lacc))
    if (lacc > bacc) {
        brb <- rb
        bacc <- lacc
    }
}

