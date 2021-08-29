library(dplyr)
library(UsingR)

data("Galton") 

##These 3 codes have the same result
lm(parent~1,Galton)

Galton <- mutate(Galton,cons=1)
lm(parent~cons,Galton)

mean(Galton$parent)
#----------------------------------------------------

B1 <-cor(Galton$child,Galton$parent)*sd(Galton$child)/sd(Galton$parent)

B1 <- cov(Galton$child,Galton$parent)/var(Galton$parent)

YM <- mean(Galton$child)

XM <- mean(Galton$parent)

B0 <- YM - B1*XM

lm(child~parent,Galton)