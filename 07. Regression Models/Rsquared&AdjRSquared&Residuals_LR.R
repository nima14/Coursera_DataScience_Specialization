library(UsingR)

data("Galton")

## Check how the predit function calculates
Model <- lm(child~parent,data=Galton)

B0 <- Model$coefficients[1]

B1 <- Model$coefficients[2]

B0 + B1*newdata

newdata <- expand.grid(parent=c(70) )

predict(object=Model,newdata=newdata)
#------------------------------------

#These equations should result to zero
sum( resid(Model) )

sum( resid(Model)*Galton$parent )
#---------------------------------------

#Plotting residuals

library(ggplot2)

df <- data.frame(Galton$parent,resid(Model))
colnames(df) <- c("Parent","Res")

ggplot(df, aes(Parent,Res)) + geom_point(size=4,alpha=0.07) + theme_bw()
#---------------------------------------

summary(Model)$r.squared

cor(Galton$parent,Galton$child)^2

pred <- predict(Model)

SSreg <-sum( (pred - mean(pred))^2 )

SStot <-sum( (Galton$child - mean(Galton$child))^2 )

Rsq <- SSreg/SStot
--------------------------------------
  
data(swiss)

Model2 <- lm(Fertility~Agriculture,data=swiss)
Rsq <- summary(Model2)$r.square
summary(Model2)$adj.r.square

n <- length(swiss$Agriculture)
k <- 1 #Number of independent variables in lm


Adj.Rsq <- 1 -  (1-Rsq)*(n-1)/(n-(k+1))

