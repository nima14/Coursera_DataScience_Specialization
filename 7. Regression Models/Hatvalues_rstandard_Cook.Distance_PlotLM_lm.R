

data("swiss")


Model1 <- lm(Fertility~Agriculture,data=swiss)



n <- length(swiss$Agriculture)

x <- swiss$Agriculture

x <- cbind(1,x)

Xmean <- mean(x[,2])
XSigma <- sd(x[,2])


#These 3 are the same:
HatValues3 <- (1/n)+(1/(n-1)*((x-Xmean)/XSigma)^2)
#The first just works with 1 parameter

H <- x %*% solve(t(x) %*% x) %*% t(x)

HatValues2 <- diag(H)

HatValues <- hatvalues(Model1)

#----------------------------------------

Sigma <- summary(Model1)$sigma

e <- resid(Model1)

#These 2 are the same:
rstandard2 <- e/(Sigma*sqrt(1-HatValues))
rstandard <- rstandard(Model1)


#----------------------------------------------
set.seed(24)
x <- c(rnorm(9,mean=5,sd=5),12,15)

set.seed(23)
y <- c(2*x[1:9]+rnorm(9,sd=0.1),36,30)


fit1 <- lm(y~x)

plot(x,y)
abline(fit1)

Hat <- hatvalues(fit1)

n <- length(x) 
k <- 2 #Number of parameters including intercept
cutoff <- 2*k/n

Hat>cutoff

dfbeta(fit1)

#Very high rstudent in 10th:
rstudent(fit1)
rstandard(fit1)

plot(fit1)

x <- x[-10]
y <- y[-10]

fit2 <- lm(y~x)
plot(x,y)
abline(fit1)
abline(fit2)

#These 2 are equal:
Cook.Distance <- cooks.distance(fit1)

Cook.Distance2 <- 1/k*(rstandard(fit1))^2*(Hat/(1-Hat))





#Residual vs Fitted Values:

residuals <- resid(fit1)

pred <- predict(fit1)

par(mfrow=c(1,2)) 
plot(fit1,which=1)
plot(pred,residuals)


#Normal QQ:


Standardizedr <- rstandard(fit1)[order(rstandard(fit1))]

normalNumbers <- numeric(n)
for (i in 1:n)
          {normalNumbers[i]=qnorm(i/n)
}
normalNumbers[11]=3

par(mfrow=c(1,2)) 
plot(fit1,which=2)
plot(normalNumbers,Standardizedr)


#Standardized residuals vs Fitted Value

Standardizedr <- rstandard(fit1)
par(mfrow=c(1,2)) 
plot(fit1,which=3)
plot(pred,sqrt(abs(Standardizedr)))


#Residuals VS Leverage


par(mfrow=c(1,2)) 
plot(fit1,which=5)
plot(Hat,Standardizedr)



# Fitted Value vs Y 


par(mfrow=c(1,1)) 

plot(pred,y)






