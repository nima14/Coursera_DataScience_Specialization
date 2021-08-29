set.seed(20)

q <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (q-10)^3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise

plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)


model1 <- lm(noisy.y ~ q)
plot(model1,which = 1)
plot(q,resid(model1))
#There is some pattern


model2 <- lm(noisy.y ~ poly(q,2))
plot(model2,which = 1)
plot(q,resid(model2))
#There is some pattern

model3 <- lm(noisy.y ~ poly(q,3))
#model3 <- lm(noisy.y ~ q + I(q^2) + I(q^3))
plot(model3,which = 1)
plot(q,resid(model3))
#Almost no pattern


predicted.intervals <- predict(model3,data.frame(x=q))


#Residual plots:
plot(predicted.intervals,noisy.y)
plot(model3)



plot(q,noisy.y,col='deepskyblue4')
lines(q,predicted.intervals,col='green',lwd=3)




