library(rstan)
library(rethinking)
options(mc.cores=4) 

#assuming previous cleaning code is run
d <- read.csv("cockatoo_Data/BA_Almonds_cockatoo_60s.csv")
str(d)
str(d_BA)
d <- d_BA
d <- d[with(d, order(subject_index, rel_time)), ]

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
#individual learning models
datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,                   #number of processing techniques
  tech = d$tech_index,                     #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),              #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$bout,                          #processing bout unique to individual J
  id = d$subject_index ,                      #individual ID
  N_effects=2                               #number of parameters to estimates
)

parlist <- c("mu", "I", "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds","phi" , "lambda" )

fit_i = stan( file = 'cockatoo_data/stan_code/ewa_ind.stan', 
              data = datalist_i ,
              iter = 1000, 
              warmup=500, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.9) , 
              pars=parlist, 
              refresh=50)

precis(fit_i, depth=2)
precis(fit_i, depth=3 , pars='I')

plot(precis(fit_i, depth=2 , pars='lambda'))
plot(precis(fit_i, depth=2 , pars='phi'))

precis()
post <- extract(fit_i)