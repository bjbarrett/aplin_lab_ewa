library(rstan)
library(rethinking)
library(cmdstanr)
options(mc.cores=4) 
library('janitor')
library('beepr')
#assuming previous cleaning code is run, which we need to add to github
#d <- read.csv("cockatoo_data/BA_Almonds_cockatoo_60s.csv")
d <- read.csv("cockatoo_data/ALL_ROOSTS_Almonds_cockatoo_30s.csv")
d$subject_index <- as.integer(as.factor(d$subject) )
d <- clean_names(d)

str(d)
d <- d[with(d, order(subject_index,date,rel_time)), ]

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
beep(2)
d$tech_index <- as.integer(as.factor(d$behav1))
unique(d$subject_index)
unique(d$subject[])
which(d$subject)
counts<-data.frame(table(d$subject_index))
counts

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

#freq dep
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
#scale cues by dividing by max value
datalist_s$s <- datalist_s$s / max(datalist_s$s)

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


##freq dep and rank
datalist_s_rankfreq <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  q = cbind(d$s_highrank_blue,d$s_highrank_red), 
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=5                                                                        #number of parameters to estimates
)

datalist_s_rankfreq$q <- datalist_s_rankfreq$q / max(datalist_s_rankfreq$q)
datalist_s_rankfreq$s <- datalist_s_rankfreq$s / max(datalist_s_rankfreq$s)

##freq dep and adult
datalist_s_adultfreq <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  q = cbind(d$s_adult_blue,d$s_adult_red), 
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=5                                                                        #number of parameters to estimates
)

datalist_s_adultfreq$q <- datalist_s_adultfreq$q / max(datalist_s_adultfreq$q)
datalist_s_adultfreq$s <- datalist_s_adultfreq$s / max(datalist_s_adultfreq$s)

##freq dep and male
datalist_s_malefreq <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  q = cbind(d$s_male_blue,d$s_male_red), 
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=5                                                                        #number of parameters to estimates
)

datalist_s_malefreq$q <- datalist_s_malefreq$q / max(datalist_s_malefreq$q)
datalist_s_malefreq$s <- datalist_s_malefreq$s / max(datalist_s_malefreq$s)

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
                 seed=as.integer(5498)
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

### rank and freq bias
fit_rank_freq= stan( file = 'cockatoo_data/stan_code/ewa_freq_and_cue_slu.stan', 
                data = datalist_s_rankfreq ,
                iter = 1200, 
                warmup=600, 
                chains=4, 
                cores=4, 
                control=list(adapt_delta=0.99) , 
                pars=c("phi","lambda","gamma","fc","betaq","phi_i","lambda_i","gamma_i","fc_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                refresh=100,
                init=0,
                seed=as.integer(524)
)

### adult and freq bias
fit_adult_freq= stan( file = 'cockatoo_data/stan_code/ewa_freq_and_cue_slu.stan', 
                     data = datalist_s_adultfreq ,
                     iter = 1200, 
                     warmup=600, 
                     chains=4, 
                     cores=4, 
                     control=list(adapt_delta=0.999) , 
                     pars=c("phi","lambda","gamma","fc","betaq","phi_i","lambda_i","gamma_i","fc_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                     refresh=100,
                     init=0,
                     seed=as.integer(262)
)

### male and freq bias
fit_male_freq= stan( file = 'cockatoo_data/stan_code/ewa_freq_and_cue_slu.stan', 
                      data = datalist_s_malefreq ,
                      iter = 1200, 
                      warmup=600, 
                      chains=4, 
                      cores=4, 
                      control=list(adapt_delta=0.999) , 
                      pars=c("phi","lambda","gamma","fc","betaq","phi_i","lambda_i","gamma_i","fc_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                      refresh=100,
                      init=0,
                      seed=as.integer(2723)
)

saveRDS(fit_i, "fit_i_30s_slu_all.rds")
saveRDS(fit_freq, "fit_freq_30s_slu_all.rds")
saveRDS(fit_male, "fit_male_30s_slu_all.rds")
saveRDS(fit_adult, "fit_adult_30s_slu_all.rds")
saveRDS(fit_roost, "fit_roost_30s_slu_all.rds")
saveRDS(fit_rank, "fit_rank_30s_slu_all.rds")
saveRDS(fit_rank_freq, "fit_rank_freq_30s_slu_all.rds")
saveRDS(fit_adult_freq, "fit_adult_freq_30s_slu_all.rds")
saveRDS(fit_male_freq, "fit_male_freq_30s_slu_all.rds")

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
