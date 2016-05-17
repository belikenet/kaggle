library(readr) # csv file read / write
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
#library(rpart) # rpart for imputation
library(caret) # classification algorithm
library(mice)

options(width = 120)
#options(error = function() {
#    save.image(file = "error.rmd.RData")
#})
#knitr::opts_chunk$set(fig.align = 'center')
set.seed(3874)

csv.folder <- ""
#kaggle folder
#csv.folder <- "../input/"
train <- read_csv(file = paste0(csv.folder, "train.csv"))
test <- read_csv(file = paste0(csv.folder, "test.csv"))

full <- train %>% rename(Id = AnimalID) %>% select(-OutcomeSubtype) %>% bind_rows(test %>% rename(Id = ID) %>% mutate(OutcomeType = NA, Id = as.character(Id)))
glimpse(full)

full.original <- full
rows.train <- !is.na(full$OutcomeType)

## BREED
sample(unique(full$Breed), 30)

full$Breed.IsMix <- grepl("Mix$", full$Breed)
full$Breed.Main <- sapply(full$Breed, function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))

## NO NAME
full$Name.NoName <- is.na(full$Name)
prop.table(table(full$Name.NoName))

## DATE features
full$Hour <- hour(full$DateTime)
full$Weekday <- wday(full$DateTime, label = T)
full$Month <- month(full$DateTime, label = T)
full$Year <- year(full$DateTime)

timeOfDay <- ifelse(full$Hour > 5 & full$Hour < 11, 'morning',
                  ifelse(full$Hour > 10 & full$Hour < 16, 'midday',
                  ifelse(full$Hour > 15 & full$Hour < 20, 'lateday', 'night')))

full$TimeOfDay <- factor(timeOfDay, levels = c("morning", "midday", "lateday", "night"))
rm(timeOfDay)

## AGE
outcomes <- full[rows.train,] %>% group_by(AnimalType, OutcomeType) %>% summarise(count = n())
ggplot(outcomes, aes(x = AnimalType, y = count, fill = OutcomeType)) + 
    geom_bar(stat = "identity", position = "fill", color = "black") +
    coord_flip() +
    labs(y = "Animals %", x = "Animal", title="Outcomes: cats & dogs")

unique(full$AgeuponOutcome)

rows.fix.age <- is.na(full$AgeuponOutcome)
# fix missing age
full$Age.Value <- sapply(full$AgeuponOutcome, function(x) as.numeric(strsplit(x, split = ' ')[[1]][1]) )
full$Age.Unit <- sapply(full$AgeuponOutcome, function(x) gsub('s', '', strsplit(x, split = ' ')[[1]][2]))
#full$Age.Unit <- as.factor(full$Age.Unit)
convertToDays <- ifelse(full$Age.Unit == 'day', 1,
              ifelse(full$Age.Unit == 'week', 7,
              ifelse(full$Age.Unit == 'month', 30, 
              ifelse(full$Age.Unit == 'year', 365, NA))))
#convertToDays <- switch(full$Age.Unit, week = 7, day = 1, month = 30, year = 365)
full$Age.Days <- full$Age.Value * convertToDays

full$Age.IsAdult <- full$Age.Days >= 365

summary(full$Age.Days)

## SEX
# imputate missing sex
prop.table(table(full$Sex[full$AnimalType=="Cat"]))
prop.table(table(full$Sex[full$AnimalType=="Dog"]))

rows.fix.sex.unknown <- grepl("Unknown", full$SexuponOutcome)
full$SexuponOutcome[rows.fix.sex.unknown] <- NA_character_
rows.fix.sex.na <- is.na(full$SexuponOutcome)

unique(full$SexuponOutcome)

full$SexuponOutcome <- as.factor(full$SexuponOutcome)

full.mice <- complete(mice(full[, c("AnimalType", "SexuponOutcome", "Breed", "Color", "Age.Days", "Breed.IsMix", "Breed.Main")], method = "rf"))


#full$SexuponOutcome[rows.fix.sexoutcome] <- full$Sex[rows.fix.sexoutcome]
full$Sex.IsUnknown <- rows.fix.sex.unknown
full$Sex.IsIntact <- grepl("Intact", full$SexuponOutcome)
full$Sex <- gsub("Neutered |Intact |Spayed ", "", full$SexuponOutcome)



full$Color.Main <- sapply(full$Color, function(x) strsplit(x, split = '/| ')[[1]][1])
full$Color.IsFlat <- !grepl("/", full$Color)

# calico (cat): white, patched with orange/black
# torbie/tortie (cat): black patched with red/orange/yellow/cream
# buff: yellow-brown colour (#F0DC82)
# tricolor: NA
# seal: dark brown (saddle brown? #59260B)
# fawn (dog): light yellow (lightyellow #E5AA70)
# flame: yellow / red  (orangered)
# sable: dark brown (saddlebrown)
# liver: brown (sandybrown)
# apricot: light yellowish-orangish color (?#FBCEB1)
# lynx: medium brown to goldish to beige-white
# lilac: pale violet (palevioletred? ?#C8A2C8) 
# agouti: gray (ivory3)
# ruddy:  reddish-rosy crimson colour (indianred1)

convertToHsv <- function(colorname) {
    colorname <- tolower(colorname)
    rgb <- switch(colorname,
                  calico = "white",
                  cream = "#FFFDD0",
                  torbie = "black",
                  tortie = "black",
                  buff = "#F0DC82",
                  tricolor = "white",
                  seal = "#59260B",
                  fawn = "#E5AA70",
                  flame = "orangered",
                  sable = "saddlebrown",
                  liver = "sandybrown",
                  apricot = "#FBCEB1",
                  lynx = "antiquewhite2",
                  lilac = "#C8A2C8",
                  agouti = "ivory3",
                  ruddy = "indianred1",
                  silver = "#C0C0C0")
    
    if (is.null(rgb))
        rgb <- colorname
    
    return (rgb2hsv(col2rgb(rgb)))
}

colorsHSV <- full %>% 
    select(Color.Main) %>% 
    distinct() %>% 
    rowwise() %>% 
    mutate(Color.H = convertToHsv(Color.Main)["h", 1],
           Color.S = convertToHsv(Color.Main)["s", 1],
           Color.V = convertToHsv(Color.Main)["v", 1])

full <- full %>% inner_join(colorsHSV)

ggplot(full[rows.train,], aes(x = Month, fill = OutcomeType)) +
  geom_bar(position = 'fill', colour = 'black')

ggplot(full[rows.train,], aes(x = Weekday, fill = OutcomeType)) +
  geom_bar(position = 'fill', colour = 'black')

factorVars <- c("OutcomeType", "AnimalType", "SexuponOutcome", "Breed", "Color", "Age.Unit", "Weekday", "Month", "Breed.Main", "Color.Main", "Sex")
full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))

#full.mice <- complete(mice(combi[, c("AnimalType", "SexuponOutcome", "Breed", "Color", "Age.Value", "Age.Unit", "Age.Days", "Breed.IsMix", "Breed.Main", "Color.Main","Sex")], method = "rf"))