x <- c(1:800)

D=3 #x & x^2 & x^3
K=5 #Num of knots

knots <- 730*(1:K)/(K+1)

X1 <- outer(x,1:D,"^")
X2 <- outer(x,knots,">")*outer(x,knots,"-")^D

X <- cbind(X1,X2)

