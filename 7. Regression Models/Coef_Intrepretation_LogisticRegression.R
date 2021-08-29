x <- c(5,6) # one increase in x

h <- -2 + 0.7*x

P <- 1/(1+exp(-h))

odds <- P/(1-P)

ChangeInOdds <- odds[2] / odds[1]

ExpCoef <- exp(0.7)


#-----------------------------------------------


x <- c(1:10)

y <- c(rep(0,5),rep(1,5))

fit <- glm(y~x,family="binomial")

#how many times does odds increase fot 1 increase in x?
exp(summary(fit)$coef)

plot(x,y)
abline(glm(y~x,family="binomial"))


lnodds <- predict(fit)

exp(lnodds)/(1+exp(lnodds))

