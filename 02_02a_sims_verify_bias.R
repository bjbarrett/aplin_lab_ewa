#######################################################
######begin data simulations###########################
#######################################################
col.pal=c("blue","red")
library(rethinking)
#data sims
n <- 50                                  #number of individuals/pop size
nbouts <- 50                             #timesteps

set_cmdstan_path(path = "/gpfs/users/brendan_barrett/.cmdstan/cmdstan-2.29.1")


#simulate values for options
#pr_open <- c(mean(d$open[d$choose_blue==1]) , mean(d$open[d$choose_red==1]))
pr_open <- c(.8 , .7)
#parameter sims
gamma.sim <- logit(0.000001)                   #weight given to social info par on log-odds scale
phi.sim <- logit(0.2)                     #memory/attraction updating par on log-odds scale
fc.sim <- log(2)				          #frequency dep par on log scale
lambda.sim <- log(1)                           #sensitivity to attraction score differences

#varying effects offsets for individuals
gamma.sim_i <- rnorm( n , mean=0 , sd=0.1) #weight given to social info offsets per i
phi.sim_i <- rnorm( n , mean=0 , sd=0.5)   #memory/attraction updating offsets per i
fc.sim_i <- rnorm( n , mean=0 , sd=0.1)     #frequency dependent offsets per i
lambda.sim_i <- rnorm( n , mean=0 , sd=0.5)     #frequency dependent offsets per i

#unique parameters for each individual, to visualize heterogeneity and for plotting individual predictions
gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals
phi.sim_id <- round(logistic(phi.sim +phi.sim_i), digits=2) 				##simulated phis for all n individuals
fc.sim_id <- round(exp(fc.sim + fc.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals
lambda.sim_id <- round(exp(lambda.sim + lambda.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals

### lets add a preference
psi <- c(1 , 0)

dsim_s <- data.frame( id=0 , bout=0 , tech=0 , y1=0 , y2=0, s1=0 , s2=0 , A1=0 , A2=0  , Pr1=0 , Pr2=0 )
therow <- 1

AC <- matrix(2,ncol=2,nrow=n) 												#attraction scores for each behavior                                                           #make low payoff behavior rare
softmax(AC[1,]) 															#run to see initial prob of choosing a behavior

S1 <- S2  <- rep(0,n+1) 												# num of individuals choosing each tech in previous bout
PS1 <- PS2 <-  rep(0,nbouts+1) 										# empty vector for mean observed in previous rounds
s_temp <-  rep(0,1)

# S1[1] <- 0.5
# S2[1] <- 0.3
# S3[1] <- 0.2

for ( r in 1:nbouts ) {
  for ( i in 1:n ) {  
    my.gam <- logistic( gamma.sim + gamma.sim_i[i] ) 	#social info weight for individual i
    my.phi <- logistic( phi.sim + phi.sim_i[i] )		#weight given to past experience for individual i
    my.fconf <- exp( fc.sim + fc.sim_i[i])  			#strength of frequency dependence for individual i
    my.lambda <- exp( lambda.sim + lambda.sim_i[i])  			#strength of frequency dependence for individual i
    
    prtech_i <-  softmax(my.lambda*AC[i,]) 
    prtech_su <- c(S1[r],S2[r]) 					#social info individual i observed
    
    # frequency dependent social learning aspect below
    if ( r >= 1 ) { 
      if (sum( prtech_su ) > 0 ) {
        
        # compute frequency cue
        for ( j in 1:2 ){ s_temp[j] <- prtech_su[j]^my.fconf}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1-my.gam)*prtech_i + my.gam*prtech_s
        
      } else {
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    # choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech)
    yield <- rbinom(1, 1, prob=pr_open[tech])
    
    
    # update attractions
    yields <- rep(0,2)
    yields[tech] <- yield#makes payoff yield
    for (k in 1:2){
      AC[i,k] <- (1-my.phi)*AC[i,k] + my.phi*yields[k] + psi[k]
    }
    
    dsim_s[therow,] <- c( i , r , tech , yields[1] , yields[2] , S1[r] , S2[r] ,  AC[i,1] , AC[i,2] ,   prtech[1] , prtech[2] )
    therow <- therow + 1
  } #i
  S1[r+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$bout==r] )
  S2[r+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$bout==r] )
  
}

o <- order( dsim_s$i )
dsim <- dsim_s[o,]

#plot raw data of group level effects

plot(s1/n ~ bout, data=dsim[dsim$bout>1,], col=col.pal[1] , ylim=c(0,1.1) , xlim=c(2,nbouts+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" )
points(s2/n ~ bout, data=dsim[dsim$bout>1,] , col=col.pal[2], pch=19)
legend("topleft", cex=0.85 , as.character(pr_open), pch=19 ,col=col.pal, horiz=TRUE , bty="n", title="Payoffs")
title(main = paste("Population Mean: lambda=",lambda.sim ,", gamma=",round(logistic(gamma.sim), digits=2),", phi=",round(logistic(phi.sim),digits=2),", f=", round( exp(fc.sim), digits=2 ) ) , line = 0.5, outer = FALSE)

# 
# pdf("freq_sims.pdf",width=9,height=11)
# par(mfrow=c(5,2))##set up the plot
# par( mar=c(4,5,0.6,0.6) , oma=c(1,1,.1,.1) )
# par(cex = 0.5)
# 
# ###main plot
# plot(s1/n ~ bout, data=dsim[dsim$bout>1,], col=col.pal[1] , ylim=c(0,1.2) , pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" , xlim=c(2,nbouts) )
# points(s2/n ~ bout, data=dsim[dsim$bout>1,] , col=col.pal[2], pch=19)
# title(main = paste("Pop. Mean: lambda=",lambda.sim ,", gamma=",round(logistic(gamma.sim), digits=2),", phi=",round(logistic(phi.sim),digits=2),", f=", exp(round(fc.sim,digits=2 ))) , line = -1.2, outer = FALSE)
# legend("top", inset=.05, cex=1 , as.character(pr_open), pch=19 ,col=col.pal, horiz=TRUE , bty="n")
# ###individual predictions
# for(i in 1:n){
#   plot(Pr1 ~ (bout-1), data=dsim[dsim$id==i & dsim$bout>1,] , col=col.pal[1] , ylim=c(0,1.2) , pch=19 , xlab="Time" , ylab="Prob Choosing Option" , xlim=c(2,nbouts) )
#   points(Pr2 ~ (bout-1), data=dsim[dsim$id==i & dsim$bout>1,] , col=col.pal[2], pch=19)
#   title(main = paste("id=",i ,", lambda=",lambda.sim ,", gamma=",gamma.sim_id[i],", phi=",phi.sim_id[i],", f=", round(fc.sim_id[i],digits=2 ) ) , line = -1.2, outer = FALSE)
#   legend("top", inset=.05, cex=1 , as.character(techmeans), pch=19 ,col=col.pal, horiz=TRUE , bty="n")
# 
# }
# 
# dev.off()


###fit model
datalist_i <- list(
  N = nrow(dsim),                                  #length of dataset
  J = length( unique(dsim$id) ),       #number of individuals
  K = max(dsim$tech),                   #number of processing techniques
  tech = dsim$tech,                     #technique index
  pay_i = cbind( dsim$y1 , dsim$y2 ),  #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = dsim$bout,                          #processing bout unique to individual J
  id = dsim$id ,                      #individual ID
  N_effects=2                               #number of parameters to estimates
)


fit_i_bias = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias.stan', 
              data = datalist_i ,
              iter = 1200, 
              warmup=600, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.9) , 
              pars=c("phi" , "lambda" , "psi" ,"phi_i" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
              refresh=100,
              seed=as.integer(207)
)

fits <- precis(fit_i_bias , depth=2 ,pars=c("lambda" ,"psi" , "phi" ,"phi_i" , "lambda_i"))
fits[[1]]
sims <- c( exp(lambda.sim) , psi, phi.sim ,phi.sim_id , lambda.sim_id)

plot(fits[[1]]~sims , xlim=c(0,3) , ylim=c(0,3) )
abline(a=0 , b=1)
plot(fits[[1]]~sims , xlim=c(0,1) , ylim=c(0,1) )
abline(a=0 , b=1)


