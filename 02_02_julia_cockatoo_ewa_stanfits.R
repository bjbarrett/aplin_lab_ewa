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

###male bias
datalist_s_male <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J
  q = cbind(d$s_male_blue,d$s_male_red), 
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4                                                                        #number of parameters to estimates
)



#scale cues by dividing by max value
datalist_s_male$q <- datalist_s_male$q / max(datalist_s_male$q)

###adult bias
datalist_s_adult <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J
  q = cbind(d$s_adult_blue,d$s_adult_red), 
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4                                                                        #number of parameters to estimates
)
datalist_s_adult$q <- datalist_s_adult$q / max(datalist_s_adult$q)

###roost bias
datalist_s_roost <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J
  q = cbind(d$s_roost_blue,d$s_roost_red), 
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4                                                                        #number of parameters to estimates
)
datalist_s_roost$q <- datalist_s_roost$q / max(datalist_s_roost$q)

###rank bias
datalist_s_rank <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J
  q = cbind(d$s_highrank_blue,d$s_highrank_red), 
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4                                                                        #number of parameters to estimates
)
datalist_s_rank$q <- datalist_s_rank$q / max(datalist_s_rank$q)


#########model fits

fit_i = stan( file = 'cockatoo_data/stan_code/ewa_ind.stan', 
              data = datalist_i ,
              iter = 1200, 
              warmup=600, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.9) , 
              pars=c("phi" , "lambda" ,"phi_i" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
              refresh=100,
              seed=as.integer(20210917)
              )

####frequency dependent learning model
fit_freq = stan( file = 'cockatoo_data/stan_code/ewa_freq_slu.stan', 
                 data = datalist_s ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","fc","phi_i","lambda_i","gamma_i","fc_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(108)
)

####male-bias
fit_male= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu.stan', 
                 data = datalist_s_male ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","betaq","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(222)
)

###adult-bias
fit_adult= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu.stan', 
                data = datalist_s_adult ,
                iter = 1200, 
                warmup=600, 
                chains=4, 
                cores=4, 
                control=list(adapt_delta=0.99) , 
                pars=c("phi","lambda","gamma","betaq","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                refresh=100,
                init=0,
                seed=as.integer(543)
)

### roost bias
fit_roost= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu.stan', 
                 data = datalist_s_roost ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","betaq","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(1649)
)

### rank bias
fit_rank= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu.stan', 
                 data = datalist_s_rank ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","betaq","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(5209)
)

saveRDS(fit_i, "fit_i_60s_slu.rds")
saveRDS(fit_freq, "fit_freq_60s_slu.rds")
saveRDS(fit_male, "fit_male_60s_slu.rds")
saveRDS(fit_adult, "fit_adult_60s_slu.rds")
saveRDS(fit_roost, "fit_roost_60s_slu.rds")
saveRDS(fit_rank, "fit_rank_60s_slu.rds")
# 
# ###experimental IL
# # fit_i_exp = stan( file = 'cockatoo_data/stan_code/ewa_ind_trialcost.stan', 
# #               data = datalist_i ,
# #               iter = 1200, 
# #               warmup=600, 
# #               chains=4, 
# #               cores=4, 
# #               control=list(adapt_delta=0.9) , 
# #               pars=c("phi" , "lambda" , "lo_c" , "phi_i" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds"  ), 
# #               refresh=100,
# #               seed=as.integer(207)
# # )
# 
# 
# datalist_s2 <- list(
#   N = nrow(d),                            #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   K = 2,         #number of processing techniques
#   tech = d$tech_index,           #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
#   s = cbind(d$n_obs_blue + .01,d$n_obs_red + .01), #observed counts of all K techniques to individual J (frequency-dependence)
#   bout = d$bout,
#   id = d$subject_index ,                                           #individual ID
#   N_effects=4                                                                        #number of parameters to estimates
# )
# 
# 
# fit_f= stan( file = 'cockatoo_data/stan_code/freq.stan', 
#                 data = datalist_s2 ,
#                 iter = 1200, 
#                 warmup=600, 
#                 chains=4, 
#                 cores=4, 
#                 control=list(adapt_delta=0.99) , 
#                 pars=c("I","sigma_i","mu"), 
#                 refresh=100,
#                 init=0,
#                 seed=as.integer(5209)
# )
