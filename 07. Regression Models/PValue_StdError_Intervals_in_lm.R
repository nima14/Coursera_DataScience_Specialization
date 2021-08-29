library(UsingR)

data("Galton")

fit <- lm(data=Galton,child~parent)

summary(fit)$coef

#------------------------------------

n <- length( Galton$child )

#These 2 are equal: 

sigma <- sqrt( sum( resid(fit)^2 ) /(n-2) )

summary(fit)$sigma

#--------------------------------------
XMinusXbar <- sum((Galton$parent-mean(Galton$parent))^2)

sigmaB0 <- sqrt( ((1/n)+(mean(Galton$parent)^2/XMinusXbar) )* sigma^2 )

sigmaB1 <- sigma / ( sqrt(XMinusXbar) )



B0 <- summary(fit)$coef[1,1]
B1 <- summary(fit)$coef[2,1]


T_statB0 <- B0/sigmaB0
T_statB1 <- B1/sigmaB1


PValueB0 <- 2*pt(abs(T_statB0) , df= n-2 , lower.tail=FALSE )
PValueB1 <- 2*pt(abs(T_statB1) , df= n-2 , lower.tail=FALSE )
#--------------------------------------------

#These 2 are equal

matrix(data=c(B0,B1,sigmaB0,sigmaB1,T_statB0,T_statB1,PValueB0,PValueB1),nrow=2)


summary(fit)$coef

#-----------------------------------------------------------

parent <- 68

Pred <- predict(fit,newdata = data.frame(parent) )

#These 2 are equal

predict(fit,newdata = data.frame(parent),interval ="confidence" )


Pred +c(-1,1)*qt(0.975,df=n-2)*sigma*sqrt((1/n)+( parent - mean(Galton$parent))^2/XMinusXbar)

#--------------------------------------------------------------

#These 2 are equal

predict(fit,newdata = data.frame(parent),interval ="prediction" )

Pred +c(-1,1)*qt(0.975,df=n-2)*sigma*sqrt(1+ (1/n)+( parent - mean(Galton$parent))^2/XMinusXbar)




