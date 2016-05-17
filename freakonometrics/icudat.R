
icudat <- read.table(file = "http://www.medicine.mcgill.ca/epidemiology/joseph/courses/EPIB-621/icudat.txt", header = T)

summary(icudat)

output <- glm(sta ~ age + sex + typ, family = binomial, data = icudat)summary(output)hist(output$fitted.values)sum(output$residuals^2)