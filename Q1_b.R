# Question 1b
setwd("C:/Users/Hp/OneDrive/Documents/SEM2/ASM")
df1 <- read.csv("simpsons.csv")

dim(df1)

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



df1$Season <- factor(df1$Season)
nlevels(df1$Season)

library("ggplot2")
means <- aggregate(Rating ~  reorder(Season, Rating, mean), df1, mean)

ggplot(df1) + geom_boxplot(aes(x = reorder(Season, Rating, mean), y=Rating, 
                               fill = reorder(Season, Rating, mean)), show.legend=FALSE)



ggplot(df1, aes(x = reorder(Season, Season, length))) + stat_count()

ggplot(df1, aes(Rating)) + stat_bin()

ggplot(data.frame(size = tapply(df1$Rating, df1$Season, length), 
                  mean_rating = tapply(df1$Rating, df1$Season, mean)), 
       aes(size, mean_rating)) + geom_point()
compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters
  eta0 <-1/2 ; t0 <- 50 ## tau_b hyperparameters
  mu0<-7.175714 ; gamma0 <- 1/0.8534095^2
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}
fit1 <- compare_m_gibbs(df1$Rating, df1$Season)
apply(fit1$params, 2, mean)
apply(fit1$params, 2, sd)
mean(1/sqrt(fit1$params[, 3]))
sd(1/sqrt(fit1$params[, 3]))

## reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit1$theta), 
                       season = rep(1:ncol(fit1$theta), each = nrow(fit1$theta))) 

theta_med <- apply(theta_df, 2, mean) ## get basic posterior summary
theta_med
sort(theta_med, decreasing = TRUE) ## which Seasons did best and worst?
ggplot(theta_df) + geom_boxplot(aes(x = reorder(season, samples, mean), samples, 
                                    fill = reorder(season, samples, mean)), show.legend=FALSE)

theta_hat <- apply(fit1$theta, 2, mean)
ggplot(data.frame(size = tapply(df1$Rating, df1$Season, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()
