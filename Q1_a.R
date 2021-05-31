# Question 1a
setwd("C:/Users/Hp/OneDrive/Documents/SEM2/ASM")
df1 <- read.csv("simpsonsQ1.csv")

df1$Season <- factor(df1$Season)
dim(df1)
df1
head(df1)

# plot the distributions
# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- df1$Rating
h<-hist(x, breaks=10, col="red", xlab="Rating",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

mean(df1$Rating)
sd(df1$Rating)


library(ggplot2)
ggplot(df1) + geom_boxplot(aes(Season, Rating, fill = Season)) + geom_jitter(aes(Season, Rating, shape = Season))

tapply(df1$Rating, df1$Season, mean)

tapply(df1$Rating, df1$Season, median)

tapply(df1$Rating, df1$Season, sd)

t.test(Rating ~ Season, data=df1, var.equal = TRUE)

# Comparing the means in a Bayesian model
compare_2_gibbs <- function(y, ind, mu0 = 8.221277, tau0 = 1/0.5633503^2, del0 = 0, gamma0 = 1/0.5633503^2, a0 = 1, b0 = 50, maxiter = 10000)
{
  y1 <- y[ind == 2]
  y2 <- y[ind == 6]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  gamma0 + tau*(n1 + n2)
    deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}
library(MCMCpack)
fit1 <- compare_2_gibbs(df1$Rating, as.factor(df1$Season), maxiter = 20000)
fit <- fit1[ 4 * (1 : 5000), ]
par("mar")
par(mar=c(1,1,1,1))
plot(as.mcmc(fit))

raftery.diag(as.mcmc(fit))

apply(fit, 2, mean)

apply(fit, 2, sd)

## easier to interpret standard deviation than precision
mean(1/sqrt(fit[, 3])) 

sd(1/sqrt(fit[, 3]))

