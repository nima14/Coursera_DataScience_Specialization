library(ggplot2)

manipulate({
  



  ggplot(data = data.frame(x = c(mn-2*(sd^2),mn+2*(sd^2))), 
         aes(x)) + stat_function(fun = dnorm, n = n, args = list(mean = mn, sd = sd)) +
        ylab("") + scale_y_continuous(breaks = NULL)},
  
  n=slider(100,1000,step=10,initial = 150),
  mn =  slider(-10,10,initial = 0),
  sd= slider(0,50,initial =1 )
  
  
  
)








