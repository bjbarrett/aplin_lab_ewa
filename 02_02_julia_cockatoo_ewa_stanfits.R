library(rstan)
library(rethinking)
library(cmdstanr)
options(mc.cores=4) 
library('janitor')
library('beepr')
#assuming previous cleaning code is run, which we need to add to github
#d <- read.csv("cockatoo_data/BA_Almonds_cockatoo_60s.csv")
d <- read.csv("ALL_ROOSTS_Almonds_cockatoo_30s.csv")
#drop the 70 obs of missing age sex class
d <- d[which(is.na(d$age_index)==FALSE),]
d <- d[which(is.na(d$sex_index)==FALSE),]


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
d$age_index[is.na(d$age_index)] <- 2
d$sex_index[is.na(d$sex_index)] <- 2

d$age_index <- d$age_index + 1 #1 is adult, 2 is uv, 3 is unknown
d$sex_index <- d$sex_index + 1 #1 is female, 2 is male, 3 is unknown
d$group_index  <- as.integer(as.factor(d$group))
unique(d$subject_index)
unique(d$subject[])
which(d$subject)
counts<-data.frame(table(d$subject_index))

##seed attraction scores for tutors:
d$ac_b_init <- d$ac_r_init <- 0
#seed attraction scores with a preference for tutors
d$ac_r_init[d$subject=="X11"] <- 1
d$ac_r_init[d$subject=="BNV_H_CG"] <- 1
#blue
d$ac_b_init[d$subject=="BPO_V_BA"] <- 1
d$ac_b_init[d$subject=="MVT_V_BA"] <- 1


### individual learning models
datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  L = length( unique(d$group_index) ),       #number of individuals
  K = 2,                   #number of processing techniques
  tech = d$tech_index,                     #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),  #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$bout,                          #processing bout unique to individual J
  id = d$subject_index ,                      #individual ID
  N_effects=2 ,                               #number of parameters to estimates
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init )
)

#freq dep
datalist_s <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques,
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4,
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )       #number of groups
  
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
  N_effects=4,                                                                        #number of parameters to estimates
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )       #number of groups
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
  N_effects=4,                                                                        #number of parameters to estimates
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )       #number of groups
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
  N_effects=4,                                                                        #number of parameters to estimates
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )       #number of groups
)
datalist_s_roost$q <- datalist_s_roost$q / max(datalist_s_roost$q)


########CMDSTANR model fits#########
file <- file.path("cockatoo_data/stan_code/ewa_ind2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_i <- mod$sample(
  data = datalist_i,
  seed = 123,
  chains = 5,
  parallel_chains = 5,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  adapt_delta = 0.9,
)

fit_i$summary( "log_lambda" )
fit_i$summary( "logit_phi" )
fit_i$summary( "lambda_i" )
fit_i$summary( "phi_i" )
fit_i$summary( "G" )
fit_i$summary( "I" )
fit_i$summary( "psi" )

stanfit <- rstan::read_stan_csv(fit_i$output_files())
#post_i <- extract(stanfit)
save(stanfit , file="ind.rds")

######
file <- file.path("cockatoo_data/stan_code/ewa_freq2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_freq <- mod$sample(
  data = datalist_s,
  seed = 13,
  adapt_delta = 0.999,
  init = 0.1,
  chains = 5,
  parallel_chains = 5,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 13
  )

# draws_f <- fit_freq$draws()
 mcmc_dens(fit_i$draws(c("G")))
# fit_freq$summary( "log_lambda" )
# fit_freq$summary( "logit_phi" )
# fit_freq$summary( "logit_gamma" )
fit_freq$summary( "log_f" )
# fit_freq$summary( "lambda_i" )
# fit_freq$summary( "phi_i" )
# fit_freq$summary( "fc_i" )
# fit_freq$summary( "gamma_i" )
# fit_freq$summary( "G" )
# fit_freq$summary( "psi" )
# mcmc_trace(fit_freq, pars = "sigma_i")

stanfit <- rstan::read_stan_csv(fit_freq$output_files())
#post_freq <- extract(stanfit)
save(stanfit , file="freq.rds")

###male_bias
file <- file.path("cockatoo_data/stan_code/ewa_cue2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_male <- mod$sample(
  data = datalist_s_male,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 5,
  parallel_chains = 5,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

fit_male$summary( "log_lambda" )
# fit_male$summary( "logit_phi" )
# fit_male$summary( "logit_gamma" )
# fit_male$summary( "betaq" )
# fit_male$summary( "lambda_i" )
# fit_male$summary( "phi_i" )
# fit_male$summary( "gamma_i" )
# fit_male$summary( "betaq_i" )
# fit_male$summary( "logit_phi" )

stanfit <- rstan::read_stan_csv(fit_male$output_files())
save(stanfit , file="male_cue.rds")

###adult_bias
file <- file.path("cockatoo_data/stan_code/ewa_cue2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_adult <- mod$sample(
  data = datalist_s_adult,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 5,
  parallel_chains = 5,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

stanfit <- rstan::read_stan_csv(fit_adult$output_files())
save(stanfit , file="adult_cue.rds")

###roost_bias
file <- file.path("cockatoo_data/stan_code/ewa_cue2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_roost <- mod$sample(
  data = datalist_s_roost,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 5,
  parallel_chains = 5,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)
stanfit <- rstan::read_stan_csv(fit_roost$output_files())
save(stanfit , file="roost_cue.rds")


# 
# ###rank bias
# datalist_s_rank <- list(
#   N = nrow(d),                            #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   K = 2,         #number of processing techniques
#   tech = d$tech_index,           #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
#   s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J
#   q = cbind(d$s_highrank_blue,d$s_highrank_red), 
#   bout = d$bout,
#   id = d$subject_index ,                                           #individual ID
#   N_effects=4                                                                        #number of parameters to estimates
# )
# datalist_s_rank$q <- datalist_s_rank$q / max(datalist_s_rank$q)
# 
# 
# ##freq dep and rank
# datalist_s_rankfreq <- list(
#   N = nrow(d),                            #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   K = 2,         #number of processing techniques
#   tech = d$tech_index,           #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$s_highrank_blue,d$s_highrank_red), 
#   s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
#   bout = d$bout,
#   id = d$subject_index ,                                           #individual ID
#   N_effects=5                                                                        #number of parameters to estimates
# )
# 
# datalist_s_rankfreq$q <- datalist_s_rankfreq$q / max(datalist_s_rankfreq$q)
# datalist_s_rankfreq$s <- datalist_s_rankfreq$s / max(datalist_s_rankfreq$s)
# 
# ##freq dep and adult
# datalist_s_adultfreq <- list(
#   N = nrow(d),                            #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   K = 2,         #number of processing techniques
#   tech = d$tech_index,           #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$s_adult_blue,d$s_adult_red), 
#   s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
#   bout = d$bout,
#   id = d$subject_index ,                                           #individual ID
#   N_effects=5                                                                        #number of parameters to estimates
# )
# 
# datalist_s_adultfreq$q <- datalist_s_adultfreq$q / max(datalist_s_adultfreq$q)
# datalist_s_adultfreq$s <- datalist_s_adultfreq$s / max(datalist_s_adultfreq$s)
# 
# ##freq dep and male
# datalist_s_malefreq <- list(
#   N = nrow(d),                            #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   K = 2,         #number of processing techniques
#   tech = d$tech_index,           #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
#   q = cbind(d$s_male_blue,d$s_male_red), 
#   s = cbind(d$n_obs_blue,d$n_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
#   bout = d$bout,
#   id = d$subject_index ,                                           #individual ID
#   N_effects=5                                                                        #number of parameters to estimates
# )
# 
# datalist_s_malefreq$q <- datalist_s_malefreq$q / max(datalist_s_malefreq$q)
# datalist_s_malefreq$s <- datalist_s_malefreq$s / max(datalist_s_malefreq$s)

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

fit_i_bias = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias.stan', 
              data = datalist_i ,
              iter = 1200, 
              warmup=600, 
              chains=4, 
              cores=4, 
              control=list(adapt_delta=0.9) , 
              pars=c("phi" , "lambda" ,"phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
              refresh=100,
              seed=as.integer(20)
)


fit_i_bias_a = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_a.stan', 
                   data = datalist_i ,
                   iter = 1000, 
                   warmup=500, 
                   chains=4, 
                   cores=4, 
                   control=list(adapt_delta=0.9) , 
                   pars=c("phi" , "lambda" , "A" , "phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
                   refresh=100,
                   seed=as.integer(20)
)

fit_i_bias_as = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_as.stan', 
                     data = datalist_i ,
                     iter = 1000, 
                     warmup=500, 
                     chains=4, 
                     cores=4, 
                     control=list(adapt_delta=0.9) , 
                     pars=c("phi" , "lambda" , "A" , "S", "phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
                     refresh=100,
                     seed=as.integer(20)
)

fit_i2 = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_as_int.stan', 
                      data = datalist_i ,
                      iter = 1000, 
                      warmup=500, 
                      chains=4, 
                      cores=4, 
                      control=list(adapt_delta=0.9) , 
                      pars=c("logit_phi" , "log_lambda" , "phi_i" , "psi" , "lambda_i" ,"I" , "G","sigma_i" ,"Rho_i","lambda_g" , "sigma_g" ,"Rho_g","log_lik" ,"PrPreds" ), 
                      refresh=50,
                      seed=as.integer(20)
)

fit_i_bias_a2 = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_a2.stan', 
                     data = datalist_i ,
                     iter = 1000, 
                     warmup=500, 
                     chains=4, 
                     cores=4, 
                     control=list(adapt_delta=0.9) , 
                     pars=c( "A" , "phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
                     refresh=100,
                     seed=as.integer(20)
)

fit_i_bias_as2 = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_as2.stan', 
                      data = datalist_i ,
                      iter = 1000, 
                      warmup=500, 
                      chains=4, 
                      cores=4, 
                      control=list(adapt_delta=0.9) , 
                      pars=c( "A" , "S" ,"phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
                      refresh=100,
                      seed=as.integer(20)
)

fit_i = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias_as_int.stan', 
                       data = datalist_i ,
                       iter = 1000, 
                       warmup=500, 
                       chains=4, 
                       cores=4, 
                       control=list(adapt_delta=0.9) , 
                       pars=c("log_lambda", "logit_phi", "psi" , "lambda_i" ,"phi_i" , "sigma_i" ,"Rho_i", "sigma_g" ,"Rho_g" , "G" , "I" ,"log_lik" ,"PrPreds" ), 
                       refresh=100,
                       seed=as.integer(20)
)

post <- extract(fit_i)

file <- file.path("cockatoo_data/stan_code/ewa_ind_bias_as_int.stan")
mod <- cmdstan_model(file)

fit_i <- mod$sample(
  data = datalist_i,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 200,
  iter_sampling = 1000,
  iter_warmup = 500,
  threads_per_chain = 4
)


draws_arr <- fit$draws()
x1 <- as_draws_matrix(fit_i2$draws())

# str(fit_i)
draws_i <- fit_i$draws()
# mcmc_dens(fit_i$draws(c("G")))
fit_i2$summary( "log_lambda" )
fit_i2$summary( "logit_phi" )
fit_i2$summary( "G" )

#dmat or dlist are best
#df <-fit_i2$draws(format = "draws_df")
#dmat <-fit_i2$draws(format = "draws_matrix") 
#darr <-fit_i2$draws(format = "draws_array")
#dlist <-fit_i2$draws(format = "draws_list")

precis(fit_i_bias_a)
fit_i_bias_a = stan( file = 'cockatoo_data/stan_code/ewa_ind_bias.stan', 
                   data = datalist_i ,
                   iter = 1200, 
                   warmup=600, 
                   chains=4, 
                   cores=4, 
                   control=list(adapt_delta=0.9) , 
                   pars=c("phi" , "lambda" ,"phi_i" , "psi" , "lambda_i" , "sigma_i" ,"Rho_i", "log_lik" ,"PrPreds" ), 
                   refresh=100,
                   seed=as.integer(20)
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

fit_freq_bias = stan( file = 'cockatoo_data/stan_code/ewa_freq_slu_bias.stan', 
                 data = datalist_s ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","fc", "psi","phi_i","lambda_i","gamma_i","fc_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(567)
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

fit_male_bias= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu_bias.stan', 
                data = datalist_s_male ,
                iter = 1200, 
                warmup=600, 
                chains=4, 
                cores=4, 
                control=list(adapt_delta=0.99) , 
                pars=c("phi","lambda","gamma","betaq","psi" ,"phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                refresh=100,
                init=0,
                seed=as.integer(98)
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

fit_adult_bias= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu_bias.stan', 
                 data = datalist_s_adult ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","betaq","phi_i", "psi" ,"lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                 refresh=100,
                 init=0,
                 seed=as.integer(93)
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

fit_roost_bias= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu_bias.stan', 
                 data = datalist_s_roost ,
                 iter = 1200, 
                 warmup=600, 
                 chains=4, 
                 cores=4, 
                 control=list(adapt_delta=0.99) , 
                 pars=c("phi","lambda","gamma","betaq","psi","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
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

fit_rank_bias= stan( file = 'cockatoo_data/stan_code/ewa_cue_slu_bias.stan', 
                data = datalist_s_rank ,
                iter = 1200, 
                warmup=600, 
                chains=4, 
                cores=4, 
                control=list(adapt_delta=0.99) , 
                pars=c("phi","lambda","gamma","betaq","psi","phi_i","lambda_i","gamma_i","betaq_i","sigma_i","Rho_i","log_lik","PrPreds"), 
                refresh=100,
                init=0,
                seed=as.integer(183)
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
                     control=list(adapt_delta=0.99) , 
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
