library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(caret)
library("sp")
library(glmnet)
#library(IDPmisc)

trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;

categories <- c("WARRANTS","OTHER OFFENSES","LARCENY/THEFT","VEHICLE THEFT","VANDALISM","NON-CRIMINAL","ROBBERY","ASSAULT","WEAPON LAWS","BURGLARY","SUSPICIOUS OCC","DRUNKENNESS","FORGERY/COUNTERFEITING","DRUG/NARCOTIC","STOLEN PROPERTY","SECONDARY CODES","TRESPASS","MISSING PERSON","FRAUD","KIDNAPPING","RUNAWAY","DRIVING UNDER THE INFLUENCE","SEX OFFENSES FORCIBLE","PROSTITUTION","DISORDERLY CONDUCT","ARSON","FAMILY OFFENSES","LIQUOR LAWS","BRIBERY","EMBEZZLEMENT","SUICIDE","LOITERING","SEX OFFENSES NON FORCIBLE","EXTORTION","GAMBLING","BAD CHECKS","TREA","RECOVERED VEHICLE","PORNOGRAPHY/OBSCENE MAT")
coltypes <- list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"), Category = col_factor(categories))

crime.cv <- function(train, test, alpha = 0.5) {
  features <- train$data %>% select (-matches("Supercategory"), -matches("Category"))
  features.test <- test$data
  df = data.frame(Id = features.test$Id)
  for (c in categories){
    print(paste("crime.cv:", c))
    classes  <- train$data %>% mutate(Cat = as.factor(ifelse(Category == c, "1", "0")))  %>% select (Cat)
    fit.cv <- cv.glmnet(as.matrix(features), as.matrix(classes), family = "binomial", alpha = alpha)
    fit    <-    glmnet(as.matrix(features), as.matrix(classes), family = "binomial", alpha = alpha, lambda = fit.cv$lambda.min)
    
    classes.test <- predict(fit, newx=as.matrix(features.test), type="response", s=fit.cv$lambda.min)
    if (nrow(df)==0)   df <- rbind(df, classes.test)
    else               df <- cbind(df, classes.test)
    names(df)[-1] <- as.character(c)
  }
  
  df
}

supercategory <- function(category){
    category <- tolower(category)
    if (category %in% c('larceny/theft', 'robbery', 'burglary','stolen property')) 'thefts'
    else if (category %in% c('bribery', 'extortion', 'gambling', 'embezzlement')) 'money_offences'
    else if (category %in% c('fraud', 'forgery/counterfeiting', 'bad checks')) 'fraud'
    else if (category %in% c('assault', 'family offenses', 'kidnapping', 'missing person', 'suicide', 'arson')) 'health_danger'
    else if (category %in% c('drug/narcotic', 'driving under the influence', 'drunkenness', 'liquor laws')) 'drugs'
    else if (category %in% c('sex offenses forcible', 'sex offenses non forcible', 'pornography/obscene mat', 'prostitution')) 'sex_offences'
    else if (category %in% c('vehicle theft', 'recovered vehicle')) 'vehicles'
    else if (category %in% c('vandalism', 'trespass',  'disorderly conduct', 'loitering', 'suspicious occ')) 'disorderly_conduct'
    #else if (category %in% c('other offenses', 'non-criminal', 'warrants', 'secondary codes', 'runaway', 'weapon laws')) 'other'
    else 'other'
}

read.crime <- function (file, x_y = NULL) {
    crime <- as.data.frame(read_csv(file=file, col_types=coltypes))
    names(crime)
    crime <-
        crime %>%
        mutate(Year  = year(Dates),
               Month = month(Dates),
               Day   = day(Dates),
               Week  = week(Dates),
               Hour  = hour(Dates),
               Minute = minute(Dates),
               dayDate = as.POSIXct(round(Dates, units = "days")),
               DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                      "Tuesday",
                                                      "Wednesday",
                                                      "Thursday",
                                                      "Friday",
                                                      "Saturday",
                                                      "Sunday")))
    if ("Category" %in% names(crime))
    {
        crime <- crime %>% mutate(Supercategory = as.factor(sapply(crime$Category, supercategory)))
    }
        
    crime <- crime %>% mutate(
        PdDistrict_4 = rank(paste(trunc(X,4), round(Y, 4), sep = ","), ties.method="min"),
        PdDistrict_2 = rank(paste(trunc(X,2), round(Y, 2), sep = ","), ties.method="min")
    )
    
    if (is.null(x_y)) {
        x_y <- preProcess(crime[,c("X","Y")], method=c("center","scale","pca"))
    }
    x_y_pca <- predict(x_y, crime[,c("X","Y")])

    crime <- crime %>% 
        mutate(Dow = as.numeric(DayOfWeek),
               TX = x_y_pca$PC1,
               TY = x_y_pca$PC2) %>% 
        select(TX, TY, Dow, Year, Month, Day, Week, Hour, Minute, PdDistrict_4, PdDistrict_2, matches("Category"), matches("Id"))
    
    list(data=crime, preProcess=x_y)
}

train <- as.data.frame(read_csv(file="train.csv", col_types=coltypes))

train <-
    train %>%
    mutate(Year  = factor(year(Dates), levels=2003:2015),
           Month = factor(month(Dates), levels=1:12),
           Day   = factor(day(Dates), levels=1:31),
           Week  = factor(week(Dates), levels=1:54),
           Hour  = factor(hour(Dates), levels=0:23),
           dayDate = as.POSIXct(round(Dates, units = "days")),
           DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                  "Tuesday",
                                                  "Wednesday",
                                                  "Thursday",
                                                  "Friday",
                                                  "Saturday",
                                                  "Sunday")))

train <- train %>% mutate(
    Supercategory = as.factor(sapply(train$Category, supercategory)),
    PdDistrict_4 = as.factor(paste(trunc(train$X,4), round(train$Y, 4), sep = ",")),
    PdDistrict_2 = as.factor(paste(trunc(train$X,2), round(train$Y, 2), sep = ","))
)


sapply(train, class)

x_y <- preProcess(train[,c("X","Y")], method=c("center","scale","pca"))
x_y_pca <- predict(x_y, train[,c("X","Y")])

train <- train %>% mutate(
    TX = x_y_pca$PC1,
    TY = x_y_pca$PC2
)

x <- NaRV.omit(as.matrix(train[,c("TX","TY","DayOfWeek","Year","Month","Day","Week","Hour","PdDistrict_4","PdDistrict_2")]))

xx <- as.matrix(train[,c("TX","TY","DayOfWeek")])

xx <- train %>% 
    mutate(
        dow = as.numeric(DayOfWeek),
        year = as.numeric(Year),
        month = as.numeric(Month),
        day = as.numeric(Day),
        week = as.numeric(Week),
        hour = as.numeric(Hour),
        pdDistrict_4 = as.numeric(PdDistrict_4),
        pdDistrict_2 = as.numeric(PdDistrict_2)) %>% 
    select(TX, TY, dow, year, month, day, week, hour, pdDistrict_4, pdDistrict_2) %>% as.matrix()

train <- read.crime("train.csv")
test <- read.crime("test.csv", train$preProcess)

features <- train$data %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING")  %>% select (-matches("Category"))
features <- train$data %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING", Supercategory == "thefts")  %>% select (-matches("Supercategory"), -matches("Category"))
features <- train$data %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING")  %>% select (-matches("Supercategory"), -matches("Category"))
features <- train$data %>% filter (Category != "TREA", Category != "GAMBLING")  %>% select (-matches("Supercategory"), -matches("Category"))

classes <- train$data  %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING")  %>% select (Category)
classes <- train$data  %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING", Supercategory == "thefts")  %>% select (Category)
classes <- train$data  %>% filter (Month == 1, Year %in% 2003:2015, Category != "TREA", Category != "GAMBLING") %>% mutate(Cat = as.factor(ifelse(Supercategory == "thefts", as.character(Category), "none")))  %>% select (Cat)
classes <- train$data  %>% filter (Category != "TREA", Category != "GAMBLING") %>% mutate(Cat = as.factor(ifelse(Supercategory == "thefts", as.character(Category), "none")))  %>% select (Cat)
classes <- train$data %>% filter (Category != "TREA", Category != "GAMBLING")  %>% mutate(Cat = as.factor(ifelse(Category == "LARCENY/THEFT", "1", "0")))  %>% select (Cat)

feattest <- test$data %>% filter (Month == 1, Year %in% 2003:2015)


fit <- glmnet(as.matrix(features), as.matrix(classes), family = "multinomial")
fit.cv <- cv.glmnet(as.matrix(features), as.matrix(classes), family = "multinomial", alpha = 1)

fit.cv <- cv.glmnet(as.matrix(features %>% select(-matches("Category"))), as.matrix(features %>% select ("Category")), family = "multinomial", alpha = 0, nfolds = 3)

pred <- predict(fit, newx=as.matrix(feattest), type="response", s=0.01)


param <- list(
  #nthread             = 4,
  booster             = 'gbtree',
  objective           = 'multi:softprob',
  num_class           = 39,
  eta                 = 1.0,
  #gamma               = 0,
  max_depth           = 6,
  #min_child_weigth    = 1,
  max_delta_step      = 1
  #subsample           = 1,
  #colsample_bytree    = 1,
  #early.stop.round    = 5
)

mapSF<-get_map(location="sanfrancisco",zoom=12,source="osm")


dtrain <- xgb.DMatrix(as.matrix(features), label=classes$Category)
param <- list(
#nthread             = 4,
booster             = 'gbtree',
objective           = 'multi:softprob',
num_class           = 39,
eta                 = 1.0,
#gamma               = 0,
max_depth           = 6,
#min_child_weigth    = 1,
max_delta_step      = 1
#subsample           = 1,
#colsample_bytree    = 1,
#early.stop.round    = 5
)
bst <- xgb.train(params = param, data = dtrain, nrounds = 15, eval_metric = "mlogloss")
dtest <- xgb.DMatrix(data=as.matrix(feattest))
bst.pred <- predict(bst, dtest)



#coordinates(train) <- ~ X + Y

