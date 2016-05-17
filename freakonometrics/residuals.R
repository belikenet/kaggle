library(splines)
source("http://freakonometrics.free.fr/probit.R")

reg = glm(Y ~ X1 + X2, family = binomial)

par(mfrow = c(2, 2))
plot(reg)
par(mfrow = c(1, 1))

plot(reg, which=1)
plot(predict(reg), residuals(reg))
abline(h = 0, lty = 2, col = "grey")

plot(predict(reg), residuals(reg), col = c("blue", "red")[1 + Y])
abline(h = 0, lty = 2, col = "grey")

lines(lowess(predict(reg), residuals(reg)), col = "black", lwd = 2)

rl = lm(residuals(reg) ~ bs(predict(reg), 8))
y = predict(rl, se = TRUE)
segments(predict(reg), y$fit + 2 * y$se.fit, predict(reg), y$fit - 2 * y$se.fit, col = "green")


# check only X1 feature
plot(X1, residuals(reg), col = c("blue", "red")[1 + Y])
lines(lowess(X1, residuals(reg)), col = "black", lwd = 2)
lines(lowess(X1[Y == 0], residuals(reg)[Y == 0]), col = "blue")
lines(lowess(X1[Y == 1], residuals(reg)[Y == 1]), col = "red")
lines(X1[Y == 1], residuals(reg)[Y == 1], col = "orange")
abline(h = 0, lty = 2, col = "grey")

#include a quadratic effect in regression 
reg = glm(Y ~ X1 + I(X1 ^ 2) + X2, family = binomial)
plot(predict(reg), residuals(reg), col = c("blue", "red")[1 + Y])
lines(lowess(predict(reg)[Y == 0], residuals(reg)[Y == 0]), col = "blue")
lines(lowess(predict(reg)[Y == 1], residuals(reg)[Y == 1]), col = "red")
lines(lowess(predict(reg), residuals(reg)), col = "black", lwd = 2)
abline(h = 0, lty = 2, col = "grey")


X1_0 <- X1[Y == 0]
X1_1 <- X1[Y == 1]
X2_0 <- X2[Y == 0]
X2_1 <- X2[Y == 1]
reg0 <- glm(X1_0 ~ X2_0)

plot(predict(reg0), residuals(reg0))

par(mfrow = c(2, 2))
plot(reg0)
par(mfrow = c(1, 1))

reg1 <- glm(X1_1 ~ X2_1)

par(mfrow = c(2, 2))
plot(reg1)
par(mfrow = c(1, 1))

reg = glm(Y ~ X1 + X2, family = binomial)

hist(reg$fitted.values)