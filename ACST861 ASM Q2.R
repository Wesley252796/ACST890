set.seed(888)
alpha0 <- 20 #set value for alpha0
lambda <- 15.5 #set value for lambda
alpha <- 22.5 #set value for alpha
n <- 5000 #number of simulations
beta_sim <- rep(0,n) #create a vector for beta to be simulated
y_sim <- rep(0,n) #create a vector for y to be simulated from simulated beta
x_sim <- rep(0,n) #create a vector for x to be simulated from x
for (i in 1:n) {
  beta_sim[i] <- rgamma(1,alpha0,lambda) #simulate a value for beta from chosen alpha0 and lambda values
  y_sim[i] <- rgamma(1,alpha,beta_sim[i]) #simulate a gamma random variable from simulated beta
  x_sim[i] <- 1/y_sim[i] #simulate x by reciprocating simulated y
}
head(x_sim) #check for reasonableness
hist(x_sim) #draw a histogram of x 
print(b_sim_ave <- mean(beta_sim)) #get an average of the simulated beta
y_sim_dir <- rep(0,n) #generate gamma rvs from the average beta
x_sim_dir <- rep(0,n) #generate inverse gamma x rvs from gamma rvs y
for (i in 1:n) {
  y_sim_dir[i] <- rgamma(1,alpha,b_sim_ave)
  x_sim_dir[i] <- 1/y_sim_dir[i]
}
head(x_sim_dir) #check for reasonableness
hist(x_sim_dir) #draw a histogram of the x simulated
par(mfrow=c(1,2)) #create a matrix of panel of 1 row and 2 columns
#Put two histograms side by side for comparison
hist(x_sim) 
hist(x_sim_dir)