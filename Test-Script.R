#####################
#### Test-Script ####
#####################

x <- seq(1,100, 1)
y <- x +  rnorm(100, mean = 0 , sd = 5) 

model1 <- lm(y ~ x)

summary(model1)

plot(x,y)
abline(model1, col="red", xlab = "X", ylab = "Y")
title("Testregression")
