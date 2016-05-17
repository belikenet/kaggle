# http://armandruiz.com/kaggle/Titanic_Kaggle_Analysis.html

library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)
#library(iterators)
#require(lattice)
#require(Amelia)
library(mice)
library(stringr)
library(caret)
library(leaps)

library(randomForest)
library(rpart)
library(party)

train <- read_csv(file = "../titanic/train.csv")
test <- read_csv("../titanic/test.csv")


# Join together the test and train sets for easier feature engineering
# test$Survived <- NA
#combi <- rbind(train, test)
combi <- train %>% bind_rows(test %>% mutate(Survived = NA))
rows.train <- !is.na(combi$Survived)

# Convert to a string
#combi$Name <- as.character(combi$Name)
combi$Pclass <- factor(combi$Pclass)
combi$Sex <- factor(combi$Sex)

# Engineered variable: Embarked
rows.fix.embarked <- is.na(combi$Embarked)
combi$Embarked[rows.fix.embarked] = "C"
combi$Embarked <- factor(combi$Embarked)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = '[,.]')[[1]][2]
})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
#combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
#combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title[combi$Title %in% c('Capt', 'Col', "Dr", "Major", "Rev", "Sir")] <- 'Noble'
combi$Title[combi$Title %in% c('Dona', 'Ms', 'the Countess','Lady', '')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c("Jonkheer","Don")] <- 'Mr'

# Convert to factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$Family.Size <- combi$SibSp + combi$Parch + 1

# Fill in Age NAs
#summary(combi$Age)

predictAge2 <- function(data, rows.filter) {
    # mean age is calculated based on Pclass, Title and Sex
    # then NA are replaced by their mean
    fare.avg <- data %>% group_by(Pclass, Title, Sex) %>% summarise(avg = mean(Age, na.rm = TRUE))
    imputate <- function(pclass, title, sex) {
        found.ticket <- fare.avg$avg[fare.avg$Pclass == pclass & fare.avg$Title == title & fare.avg$Sex == sex]
        #ifelse(identical(found.ticket$avg, numeric(0)), NA_integer_, found.ticket$avg)
        return(found.ticket)
    }

    data[rows.filter,] %>% rowwise() %>% mutate(Age2 = imputate(Pclass, Title, Sex))
    data
}

rows.fix.age <- is.na(combi$Age)
trc.age <- trainControl(method = "repeatedcv", repeats = 5)
#model.age <- train(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + Family.Size, data = combi[!rows.fix.age,], method = "rpart", metric = "RMSE", trControl = trc.age, preProcess=c("nzv"))
#model.age <- train(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + Family.Size, data = combi[!rows.fix.age,], method = "rf", metric = "RMSE", trControl = cv.age)
#combi$Age[rows.fix.age] <- predict(model.age, combi[rows.fix.age,])
model.age <- complete(mice(combi[, c("Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "Family.Size", "Age", "Fare")], method = "rf"))
combi$Age <- model.age$Age
combi$Age.IsChild <- (combi$Age < 16 & (combi$Pclass == 1 | combi$Pclass == 2)) | (combi$Age < 14 & combi$Pclass == 3)
combi$Age.IsMother <- !combi$Age.IsChild & combi$Parch > 0 & combi$Title == "Mrs"
combi$Age2 <- combi$Age
#combi[rows.fix.age,] <- predictAge2(combi, rows.fix.age)


# Engineered variable: Fare
predictFare2 <- function(data, rows.filter) {
    # mean age is calculated based on Pclass and Title
    # then NA are replaced by their mean
    fare.avg <- data %>% group_by(Pclass, Title) %>% summarise(avg = mean(Fare, na.rm = TRUE))
    fare.avg.simple <- mean(data$Fare, na.rm = TRUE)
    imputate <- function(pclass, title) {
        found.ticket <- fare.avg$avg[fare.avg$Pclass == pclass & fare.avg$Title == title]
        ifelse(is.na(found.ticket), fare.avg.simple, found.ticket)
    }

    data[rows.filter,] %>% rowwise() %>% mutate(Fare2 = imputate(Pclass, Title))
}

rows.fix.fare <- is.na(combi$Fare)
rows.fix.fare.zero <- combi$Fare == 0
model.fare <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Title + Family.Size + Age,
                data = combi[!rows.fix.fare & !rows.fix.fare.zero,], method = "anova")
combi$Fare[rows.fix.fare] <- predict(model.fare, combi[rows.fix.fare,])
combi$Fare.IsZero <- rows.fix.fare.zero
combi$Fare.PerPerson <- combi$Fare / combi$Family.Size
combi$Fare2 <- combi$Fare
#combi[rows.fix.fare,] <- predictFare2(combi, rows.fix.fare)
combi$Fare.PerPerson2 <- round(combi$Fare2 / combi$Family.Size, 0)
#combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)


# Engineered variable: TicketName
combi$Ticket.Name <- sapply(combi$Ticket, FUN = function(ticket) {
    names <- strsplit(ticket, " ")[[1]]
    ifelse(length(names) > 1, names[1], NA)
})
combi$Ticket.Name[combi$Ticket.Name %in% c("A/S", "A.5.", "A/5.", "A./5.")] <- "A/5"
combi$Ticket.Name[combi$Ticket.Name %in% c("A4.", "A/4.")] <- "A/4"
combi$Ticket.Name[combi$Ticket.Name %in% c("CA.", "C.A.")] <- "CA"
combi$Ticket.Name[combi$Ticket.Name %in% c("W.E.P", "W.E.P.")] <- "WE/P"
combi$Ticket.Name[combi$Ticket.Name %in% c("SC/Paris")] <- "SC/PARIS"
combi$Ticket.Name[combi$Ticket.Name %in% c("SOTON/O.Q.", "STON/OQ.")] <- "SOTON/OQ"
combi$Ticket.Name[combi$Ticket.Name %in% c("W./C.")] <- "W/C"
combi$Ticket.Name[combi$Ticket.Name %in% c("S.O./P.P.")] <- "S.O.P."
combi$Ticket.Name[combi$Ticket.Name %in% c("S.O.C.")] <- "SO/C"
combi$Ticket.Name[combi$Ticket.Name %in% c("S.W./PP")] <- "SW/PP"
#combi$Ticket.Name[combi$Ticket.Name %in% c("A.", "AQ/3.", "AQ/4", "LP", "SC/A.3")] <- "NO/NAME"
combi$Ticket.Name[combi$Ticket.Name %in% c("SC/A4")] <- "S.C./A.4."
rows.fix.ticket.name <- is.na(combi$Ticket.Name)
combi$Ticket.Name[rows.fix.ticket.name] <- "UNK"

tabTicketName <- table(combi$Ticket.Name)
ticketNameUnder10 <- row.names(tabTicketName[tabTicketName <= 10])
combi$Ticket.Name2 <- combi$Ticket.Name
combi$Ticket.Name2[combi$Ticket.Name2 %in% ticketNameUnder10] <- "SMALL"
rm(ticketNameUnder10, tabTicketName)

combi$Ticket.Name <- factor(combi$Ticket.Name)
combi$Ticket.Name2 <- factor(combi$Ticket.Name2)

tabTicket <- table(combi$Ticket)
#combi <- combi %>% rowwise() %>% mutate (Ticket.Count = as.numeric(tabTicket[Ticket]))
combi <- combi %>% rowwise() %>% mutate(Ticket.Count = as.numeric(tabTicket[Ticket]), Related.Count = max(Family.Size, Ticket.Count))
rm(tabTicket)

predictCabinByTicket <- function(data, rows.filter) {
    ticket.name.pclass <- distinct(data %>% filter(Ticket.Name2 != "UNK" & Ticket.Name2 != "SMALL" & !is.na(Cabin)) %>% group_by(Ticket.Name2) %>% select(Ticket.Name, Pclass, Cabin))
    ticket.name.pclass$Ticket.Name <- as.character(ticket.name.pclass$Ticket.Name2)

    imputate <- function(ticketName, pclass) {
        found.cabin <- ticket.name.pclass[ticket.name.pclass$Ticket.Name == ticketName & ticket.name.pclass$Pclass == pclass, "Cabin"]
        return(ifelse(identical(found.cabin$Cabin, character(0)), NA_character_, found.cabin$Cabin))
    }

    return(data[rows.filter,] %>% rowwise() %>% mutate(Cabin = imputate(Ticket.Name2, Pclass)))
}

rows.fix.cabin.by.ticket <- which(is.na(combi$Cabin) & !is.na(combi$Ticket.Name2))
combi[rows.fix.cabin.by.ticket,] <- predictCabinByTicket(combi, rows.fix.cabin.by.ticket)

predictCabinByFare <- function(data, rows.filter) {
    ticket.fare <- distinct(data %>% group_by(Fare) %>% filter(!is.na(Cabin)) %>% select(Fare, Pclass, Cabin))

    imputate <- function(fare, pclass) {
        found.ticket <- ticket.fare[ticket.fare$Fare == fare & ticket.fare$Pclass == pclass, "Cabin"]
        ifelse(identical(found.ticket$Cabin, character(0)), NA_character_, found.ticket$Cabin)
    }

    data[rows.filter,] %>% rowwise() %>% mutate(Cabin = imputate(Fare, Pclass))
}

rows.fix.cabin.by.fare <- which(is.na(combi$Cabin) & !rows.fix.fare & !rows.fix.fare.zero)
combi[rows.fix.cabin.by.fare,] <- predictCabinByFare(combi, rows.fix.cabin.by.fare)

# Engineered variable: Deck
combi$Deck <- substring(combi$Cabin, 1, 1)
rows.fix.cabin.unk <- is.na(combi$Deck)
combi$Deck[rows.fix.cabin.unk] <- "UNK"

rows.fix.deck.f <- (combi$Deck == "UNK" &
                        (((combi$Fare > 7.70 & combi$Fare < 26) |
                          (combi$Fare <= 7.70 | combi$Fare.PerPerson <= 7.70))
                   & combi$Pclass == "3")) | combi$Deck == "G" | combi$Deck == "T"
rows.fix.deck.c <- combi$Deck == "T"
combi$Deck[rows.fix.deck.f] <- "F"
combi$Deck[rows.fix.deck.c] <- "C"
combi$Deck <- factor(combi$Deck)

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = '[,.]')[[1]][1]
})
combi$Family.ID <- paste(combi$Surname, as.character(combi$Family.Size), combi$Ticket.Name, sep = "_")
combi$Family.ID[combi$Family.Size <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- table(combi$Family.ID)
famIDs <- rownames(famIDs[famIDs <= 2])
combi$Family.ID[combi$Family.ID %in% famIDs] <- 'Small'
# Convert to a factor
combi$Family.ID <- factor(combi$Family.ID)
rm(famIDs)

# New factor for Random Forests, only allowed <32 levels, so reduce number
# Convert back to string
combi$Family.ID2 <- as.character(combi$Family.ID)
combi$Family.ID2[combi$Family.Size <= 3] <- 'Small'
# And convert back to factor
combi$Family.ID2 <- factor(combi$Family.ID2)


#combi$Survived[rows.train] <- factor(combi$Survived[rows.train])

# Split back into test and train sets
train.pre <- combi[rows.train,]
train.pre$Fate <- factor(train.pre$Survived)
#train.pre$Fare.PerPerson2 <- round(train.pre$Fare.PerPerson2,0)

levels(train.pre$Fate) <- c("perished", "survived")
test.pre <- combi[!rows.train,]
test.pre$Survived <- NULL

# Build Random Forest Ensemble
set.seed(415)
set.seed(666)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
cv.rtt <- trainControl(method = "LGOCV", repeats = 200, summaryFunction = twoClassSummary, classProbs = TRUE)
cv.ens <- trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = TRUE, returnData = FALSE, savePredictions = TRUE, verboseIter = FALSE, allowParallel = TRUE, index = createMultiFolds(train.pre$Fate, times = 3))

rows.batch <- createDataPartition(train.pre$Survived, p = 0.8, list = FALSE)
train.batch <- train.pre[rows.batch,]
test.batch <- train.pre[ - rows.batch,]


#anova(glm(Fate ~ Sex * Pclass + Fare.PerPerson:Embarked + Age * Pclass + Title:Family.Size + Title:Embarked + Title * Pclass, family = binomial("logit"), data = train.batch), test = "Chisq")

#train.tune1 <- train(Fate ~ Sex * Pclass + Title, data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl) #8.8596 # kaggle
#train.tune2  <- train(Fate ~ Sex * Pclass + Fare.PerPerson2:Embarked + Age2 * Pclass + Title:Family.Size + Title:Embarked + Title * Pclass, data = train.batch, method = "xgbTree", metric = "ROC", trControl = cv.ctrl) #79.78 # kaggle
#train.tune2b <- train(Fate ~ Sex + Pclass + Fare.PerPerson2 + Embarked + Age2 + Title + Family.Size + Deck + I(Fare.PerPerson <= 16 & Survived == 1 & Family.Size == 1), data = train.batch, method = "svmLinear", metric = "ROC", trControl = cv.ctrl) #8.8596 # kaggle
#train.tune2c <- train(Fate ~ Sex + Pclass + Fare.PerPerson2 + Embarked + Age2 + Title + Family.Size + I(Fare.PerPerson2 <= 27 & Survived == 1 & Family.Size == 1) + I(Sex == "male" & Family.Size == 1 & Fare.PerPerson2 >= 27 & Fare.PerPerson2 <= 38), data = train.batch, method = "glmnet", metric = "ROC", trControl = cv.ctrl) #8.8596 # kaggle
#train.tune2c <- train(Fate ~ Sex + Pclass + Fare.PerPerson2 + Embarked + Age2 + Title + Family.Size + I(Fare.PerPerson2 <= 27 & Family.Size == 1) + I(Sex == "male" & Family.Size == 1 & Fare.PerPerson2 >= 27 & Fare.PerPerson2 <= 38), data = train.batch, method = "glmnet", metric = "ROC", trControl = cv.ctrl) #8.8596 # kaggle
#train.tune2 <- train(Fate ~ Sex * Pclass + Fare.PerPerson + Embarked + Age + Title + Family.Size + Age.IsChild + Fare.IsZero + Ticket.Name2, data = train.batch, method = "xgbTree", metric = "ROC", trControl = cv.ctrl, probMethod = "Bayes", tuneLength = 10) #8.8596 # kaggle
#train.tune2 <- train(Fate ~ Sex * Pclass + Fare.PerPerson2:Embarked + Age2 * Pclass + Title:Family.Size + Title:Embarked + Title * Pclass, data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl) #8.8596 # kaggle
#train.tune2 <- train(Fate ~ Sex + Pclass + Fare.PerPerson + Age:Pclass + Sex:Pclass + Family.Size + Title + Age, data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl) # 0.8202 # kaggle

#rows.tune2 <- rows.test(cf.test(train.tune2))

#pred.tune2 <- predict(train.tune2, test.batch)
#pred.tune2.prob <- predict(train.tune2, test.batch, type="prob")
#(cf <- confusionMatrix(pred.tune2, test.batch$Fate))

#rows.test(pred.tune2)

#pred.tune8 <- predict(train.tune8, test.batch)
#(cf <- confusionMatrix(pred.tune8, test.batch$Fate))



#train.rf <- randomForest(Fate ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Family.Size + Family.ID2 + Fare.PerPerson)


#fit <- cforest(Fate ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Family.Size + Family.ID2, data = train.pre, controls = cforest_unbiased(ntree = 2000, mtry = 3)) # tutorial
#pred <- predict(fit, test.pre, OOB = TRUE, type = "response")
#pred <- predict(train.tune2, test.pre)
#result <- data.frame(Survived = as.numeric(pred) - 1, PassengerId = test$PassengerId)
#write.csv(result, "../titanic/titanic.csv", quote = F, row.names = F)

#varImpPlot(fit)

#title, sex, age, pclass, and fare
#train.rp <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title, data = train.pre, method = "class")

#pred <- predict(train.tune2, test.pre)
#result <- data.frame(Survived = as.numeric(pred) - 1, PassengerId = test$PassengerId)
#result <- data.frame(Survived = pred$Survived, PassengerId = test$PassengerId)
#write.csv(result, "../titanic/titanic.csv", quote = F, row.names = F)


cf.test <- function(tune, data = test.batch) {
    pred <- predict(tune, data)
    print(confusionMatrix(pred, data$Fate, positive = "survived"))
    return(pred)
}

rows.test <- function(pred, data = test.batch) {
    return(which(!(data$Fate == pred)))
}
