set.seed(2725)
lambda <- 2.5 #chosen parameters for exponential distribution
alpha <- 10;beta <- 5 #chosen parameters for pareto distribution
a <- 3; b <- 8 #chosen parameters for uniform distribution
n <- 1000 #number of maxima to be simulated
max_rexp <- rep(0,n)
max_rpar <- rep(0,n)
max_runif <- rep(0,n)
#Create a loop to perform 1000 simulations of the follows
for (i in 1:n) {
  max_rexp[i] <- max(rexp(n,lambda)) #simulate a maximum from 1000 exponential r.vs
  max_rpar[i] <- max(rpareto(n,alpha,beta)) #simulate a maximum from 1000 pareto r.vs
  max_runif[i] <- max(runif(n,a,b)) #simulate a maxima from 1000 uniform r.vs 
}
#Check for reasonableness
head(max_rexp)
head(max_rpar)
head(max_runif)
#Fit GEV distribution to simulated exponential samples
exp_fit <- fevd(max_rexp, type="GEV", method="MLE")
exp_fit #print fitted results
print(mu_expfit <- exp_fit$results$par[1]);print(sigma_expfit <- exp_fit$results$par[2]) #print estimated mu and sigma
print(xi_expfit <- exp_fit$results$par[3]) #print estimated xi
#Fit GEV distribution to simulated pareto samples 
par_fit <- fevd(max_rpar, type="GEV", method="MLE")
par_fit #print fitted results
print(mu_parfit <- par_fit$results$par[1]); print(sigma_parfit <- par_fit$results$par[2]) #print estimated mu and sigma
print(xi_parfit <- par_fit$results$par[3]) #print estimated xi
#Fit GEV distribution to simulated uniform samples
unif_fit <- gev.fit(max_runif)
test3 <- unif_fit$mle
#Assign names to the estimated parameters
mu_uniffit <- unif_fit[1]; sigma_uniffit <- unif_fit[2] 
xi_uniffit <- unif_fit[3] 
#Perform goodness-of-fit tests
test_rexp <- gnfit(max_rexp, "gum", pr=c(mu_expfit, sigma_expfit, xi_expfit))
test_rpar <- gnfit(max_rpar, "gev", pr=c(mu_parfit, sigma_parfit, xi_parfit))
test_runif <- gnfit(max_runif, "gev", pr=test3)