
###brainstorn payoff of zero causing unchosen option to be favored
Softmax <- function(x){
  exp(x)/sum(exp(x))
}
#color palate
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and burnt orange (high payoff)

#simulate data set conditions
dsim <- data.frame( timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
phi <- 0.2 # low is more reliance on past memory and high is reliance on recent experience (0-1)
lambda <- 6 # sensitivity to differences in attraction, low is insensitive (0 is no attention to differences in attraction score, equal chance to pick each behavior), higher is more sensitive (0-infinity)
techpr <- c(.2,.3) # prob succeed

AC <- matrix(0,nrow=timesteps,ncol=2) 	

##simulated data for one discreet individual

for (t in 1:timesteps){
  prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
  tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
  payoff <- rbinom(n=1, size=1, prob=techpr[tech]) #draw a behavior of tech=k with specified mean and SD, realized choice
  obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
  obs_payoffs_t[tech] <- payoff   #populate with each observed payoff
  
  dsim[t,] <- c(  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
  
  # update attractions for next timestep t + 1, don't do on final round
  if(t<timesteps){ 
    for (k in 1:2){
      AC[t+1,k] <- (1-phi)*AC[t,k] + phi*(obs_payoffs_t[k])
    }
  }
}
dsim$succeed <- dsim$payoff_i1 + dsim$payoff_i2
#below plots sim
plot(dsim$Pr1~dsim$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
points(dsim$Pr2~dsim$timestep,col=col.pal[2],pch=19 )
points(rep(1.05,timesteps) ~ dsim$timestep,col=col.pal[dsim$tech],pch=ifelse(dsim$succeed==1 , 1 , 19 ), cex=0.7 )
abline(h=1)
