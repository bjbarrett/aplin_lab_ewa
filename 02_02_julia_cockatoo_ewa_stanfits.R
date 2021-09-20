library(rstan)
library(rethinking)
options(mc.cores=4) 

#assuming previous cleaning code is run, which we need to add to github
d <- read.csv("cockatoo_data/BA_Almonds_cockatoo_60s.csv")
str(d)
d <- d[with(d, order(subject_index,date, rel_time)), ]

d$bout <- rep(0,nrow(d))
ff <- rep(0,length(unique(d$subject)))
for (r in 1:nrow(d)) {
  for(i in 1:length(unique(d$subject_index))) {
    if( d[r,"subject_index"]==i){ #this is temporary due to zeros
      ff[i] <-ff[i] + 1
      d$bout[r] <- ff[i]
    }
  }
}

d$tech_index <- as.integer(as.factor(d$behav1))
##### create datalists

### individual learning models
datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,                   #number of processing techniques
  tech = d$tech_index,                     #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),  #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$bout,                          #processing bout unique to individual J
  id = d$subject_index ,                      #individual ID
  N_effects=2                               #number of parameters to estimates
)

datalist_s <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4                                                                        #number of parameters to estimates
)

#parlist <- c("mu", "I", "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds","phi" , "lambda" )

fit_i = stan( file = 'cockatoo_data/stan_code/ewa_ind.stan', 
              data = datalist_i ,
              iter = 1200, 
              warmup=600, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.9) , 
              pars=c("mu", "I", "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds","phi" , "lambda" ), 
              refresh=100,
              seed=as.integer(20210917)
              )

precis(fit_i, depth=2)
precis(fit_i, depth=3 , pars='I')

plot(precis(fit_i, depth=2 , pars='lambda'))
plot(precis(fit_i, depth=2 , pars='phi'))

post <- extract(fit_i)

#trace plots
traceplot(fit_i , pars='mu')
traceplot(fit_i , pars='sigma_i')
traceplot(fit_i , pars='lambda')
traceplot(fit_i , pars='phi')


####frequency dependent learning model

fit_freq = stan( file = 'cockatoo_data/stan_code/ewa_freq.stan', 
              data = datalist_s ,
              iter = 1200, 
              warmup=600, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.99) , 
              pars=c("mu","I","sigma_i","Rho_i","log_lik","PrPreds","phi","lambda","gamma","fc" ), 
              refresh=100,
              init=0,
              seed=as.integer(108)
              )


traceplot(fit_freq , pars='mu') #params on link fcn scale
traceplot(fit_freq , pars='sigma_i')
traceplot(fit_freq , pars='lambda')
traceplot(fit_freq , pars='phi')
traceplot(fit_freq , pars='gamma')
traceplot(fit_freq , pars='fc' )

precis(fit_freq, depth=2 , pars='mu') #params on link fcn scale
precis(fit_freq, depth=2 , pars='lambda')
precis(fit_freq, depth=2 , pars='phi')
precis(fit_freq, depth=2 , pars='gamma')
precis(fit_freq, depth=2 , pars='fc')

#plots
post_f <- extract(fit_freq)
post <- post_f
#lambda
dens(exp(post$mu[,1]) , col="white" , xlim=c(0,12) , main="lambda" )
for(i in 1:max(d$ID_all_index)){
  dens(post$lambda[,i] , add=TRUE , col=col.alpha("slateblue", 0.2))
}
shade(density(exp(post$mu[,1])) , HPDI(exp(post$mu[,1]) , prob=0.9999) , col=col.alpha("slateblue", 0.5) )
dens( exp(rnorm(50000, mean = 1, sd = 0.6)) , add=TRUE , lty=2 , col="black")

#phi
dens(logistic(post$mu[,2]) , col="white" , xlim=c(0,1) , main="phi" )
for(i in 1:max(d$ID_all_index)){
  dens(post$phi[,i] , add=TRUE , col=col.alpha("slateblue", 0.2))
}
shade(density(logistic(post$mu[,2])) , HPDI(logistic(post$mu[,2]) , prob=0.9999) , col=col.alpha("slateblue", 0.5) )
dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black")

#gamma
dens(logistic(post$mu[,3]) , col="white" , xlim=c(0,1) , main="gamma" )
for(i in 1:max(d$ID_all_index)){
  dens(post$gamma[,i] , add=TRUE , col=col.alpha("slateblue", 0.2))
}
shade(density(logistic(post$mu[,3])) , HPDI(logistic(post$mu[,3]) , prob=0.9999) , col=col.alpha("slateblue", 0.5) )
dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black")

#fc
dens(exp(post$mu[,4]) , col="white" , xlim=c(0,12) , main="fc" )
for(i in 1:max(d$ID_all_index)){
  dens(post$fc[,i] , add=TRUE , col=col.alpha("slateblue", 0.2))
}
shade(density(exp(post$mu[,4])) , HPDI(exp(post$mu[,4]) , prob=0.9999) , col=col.alpha("slateblue", 0.5) )
dens( exp(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black")

##turn these into fcns and add prior option, should reparameterize models

str(post)
x<- apply(post$PrPreds[,,1] , 2 , mean)
str(x)
points(x[1:100], nrow(x[1:100]) , cex=.5)

######fit_freq_2

fit_freq_2 = stan( file = 'cockatoo_data/stan_code/ewa_freq.stan', 
                 data = datalist_s ,
                 iter = 1000, 
                 warmup=500, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("sigma_i","Rho_i","log_lik","PrPreds","phi","lambda","gamma","fc","phi_i","lambda_i","gamma_i","fc_i" ), 
                 refresh=100,
                 init=0,
                 seed=as.integer(108)
)
