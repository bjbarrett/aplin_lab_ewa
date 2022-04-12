library(rstan)
library(rethinking)
library(cmdstanr)
options(mc.cores=32) 

#assuming previous cleaning code is run, which we need to add to github
#d <- read.csv("cockatoo_data/BA_Almonds_cockatoo_60s.csv")
d <- read.csv("ALL_ROOSTS_Almonds_cockatoo_60s.csv")


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
# 
# datalist_i_bluepref <- list(
#   N = nrow(d),                                  #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   L = length( unique(d$group_index) ),       #number of individuals
#   K = 2,                   #number of processing techniques
#   tech = d$tech_index,                     #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),  #individual payoff at timestep (1 if succeed, 0 is fail)
#   bout = d$bout,                          #processing bout unique to individual J
#   id = d$subject_index ,                      #individual ID
#   N_effects=2 ,                               #number of parameters to estimates
#   sex_index=d$sex_index,
#   age_index=d$age_index,
#   group_index=d$group_index,
#   ac_init = cbind( d$ac_b_init , d$ac_r_init ),
#   pref = d$choose_blue
# )
# 
# datalist_i_redpref <- list(
#   N = nrow(d),                                  #length of dataset
#   J = length( unique(d$subject_index) ),       #number of individuals
#   L = length( unique(d$group_index) ),       #number of individuals
#   K = 2,                   #number of processing techniques
#   tech = d$tech_index,                     #technique index
#   pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),  #individual payoff at timestep (1 if succeed, 0 is fail)
#   bout = d$bout,                          #processing bout unique to individual J
#   id = d$subject_index ,                      #individual ID
#   N_effects=2 ,                               #number of parameters to estimates
#   sex_index=d$sex_index,
#   age_index=d$age_index,
#   group_index=d$group_index,
#   ac_init = cbind( d$ac_b_init , d$ac_r_init ),
#   pref = d$choose_red
# )
# 


#freq dep
datalist_s <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques,
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$f_obs_blue,d$f_obs_red), #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4,
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )       #number of groups
)

###male bias
datalist_s_male <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$subject_index) ),       #number of individuals
  K = 2,         #number of processing techniques
  tech = d$tech_index,           #technique index
  pay_i = cbind( d$choose_blue*d$open , d$choose_red*d$open ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$f_obs_blue,d$f_obs_red), #observed counts of all K techniques to individual J
  q = cbind(d$s_male_blue,d$s_male_red), 
  bout = d$bout,
  id = d$subject_index ,                                           #individual ID
  N_effects=4,                                                                        #number of parameters to estimates
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  ac_init = cbind( d$ac_b_init , d$ac_r_init ),
  L = length( unique(d$group_index) )  #number of groups
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
  s = cbind(d$f_obs_blue,d$f_obs_red), #observed counts of all K techniques to individual J
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
  s = cbind(d$f_obs_blue,d$f_obs_red), #observed counts of all K techniques to individual J
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
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  adapt_delta = 0.9,
)


stanfit <- rstan::read_stan_csv(fit_i$output_files())
#post_i <- extract(stanfit)
save(stanfit , file="ind.rds")
# 
# file <- file.path("cockatoo_data/stan_code/ewa_ind_hw_pref.stan")
# mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
# fit_i_blue <- mod$sample(
#   data = datalist_i_bluepref,
#   seed = 123,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 100,
#   iter_sampling = 1000,
#   iter_warmup = 1000,
#   threads=8,
#   adapt_delta = 0.9,
# )
# 
# file <- file.path("cockatoo_data/stan_code/ewa_ind_hw_pref.stan")
# mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
# fit_i_red <- mod$sample(
#   data = datalist_i_redpref,
#   seed = 123,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 100,
#   iter_sampling = 1000,
#   iter_warmup = 1000,
#   threads=8,
#   adapt_delta = 0.9,
# )
# 
# fit_i_red$summary( "log_lambda" )
# fit_i_red$summary( "logit_phi" )
# fit_i_blue$summary( "log_lambda" )
# fit_i_blue$summary( "logit_phi" )

# fit_i$summary( "lambda_i" )
# fit_i$summary( "phi_i" )
# fit_i$summary( "G" )
# fit_i$summary( "I" )
# fit_i$summary( "psi" )

stanfit <- rstan::read_stan_csv(fit_i$output_files())
#post_i <- extract(stanfit)
save(stanfit , file="ind.rds")

######
file <- file.path("cockatoo_data/stan_code/ewa_freq2.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_freq <- mod$sample(
  data = datalist_s,
  seed = 153,
  adapt_delta = 0.99,
  init = 0.01,
  chains = 4,
  parallel_chains = 4, 
  refresh = 10,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 14
  )

# #draws_f <- fit_freq$draws()
# #mcmc_dens(fit_freq$draws(c("G")))
# # fit_freq$summary( "log_lambda" )
# # fit_freq$summary( "logit_phi" )
# # fit_freq$summary( "logit_gamma" )
# fit_freq$summary( "log_f" )
# #fit_freq$summary( "lambda_i" )
# #fit_freq$summary( "phi_i" )
# fit_freq$summary( "fc_i" )
# # fit_freq$summary( "gamma_i" )
# fit_freq$summary( "G" )
# # fit_freq$summary( "psi" )
# # mcmc_trace(fit_freq, pars = "sigma_i")

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
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

# fit_male$summary( "log_lambda" )
# # fit_male$summary( "logit_phi" )
# fit_male$summary( "logit_gamma" )
#  fit_male$summary( "betaq" )
# # fit_male$summary( "lambda_i" )
# # fit_male$summary( "phi_i" )
# # fit_male$summary( "gamma_i" )
# # fit_male$summary( "betaq_i" )
# # fit_male$summary( "logit_phi" )

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
  chains = 4,
  parallel_chains = 4,
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
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)
stanfit <- rstan::read_stan_csv(fit_roost$output_files())
save(stanfit , file="roost_cue.rds")

fit_roost$summary( "log_lambda" )
# fit_male$summary( "logit_phi" )
# fit_male$summary( "logit_gamma" )
fit_roost$summary( "betaq" )
# fit_male$summary( "lambda_i" )
# fit_male$summary( "phi_i" )
# fit_male$summary( "gamma_i" )
fit_roost$summary( "betaq_i"  , depth=2)
# fit_male$summary( "logit_phi" )
plot(precis(stanfit, pars="betaq" , depth=3))
plot(precis(stanfit, pars="betaq_i" , depth=3))
plot(precis(stanfit, pars="I" , depth=3))
#male proportional copying

file <- file.path("cockatoo_data/stan_code/ewa_linear.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_male_lin <- mod$sample(
  data = datalist_s_male,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

stanfit <- rstan::read_stan_csv(fit_male_lin$output_files())
save(stanfit , file="male_lin.rds")

#adult proporional copying
file <- file.path("cockatoo_data/stan_code/ewa_linear.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_adult_lin <- mod$sample(
  data = datalist_s_adult,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

stanfit <- rstan::read_stan_csv(fit_adult_lin$output_files())
save(stanfit , file="adult_lin.rds")
# 

#roost
file <- file.path("cockatoo_data/stan_code/ewa_linear.stan")
mod <- cmdstan_model(file , cpp_options = list(stan_threads = TRUE) )
fit_roost_lin <- mod$sample(
  data = datalist_s_roost,
  seed = 113,
  adapt_delta = 0.95,
  init = 0.1,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 1000,
  threads=8,
  max_treedepth = 12
)

stanfit <- rstan::read_stan_csv(fit_roost_lin$output_files())
save(stanfit , file="roost_lin.rds")