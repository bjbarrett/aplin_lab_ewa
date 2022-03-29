#load model outpout
#load("/Users/bbarrett/Downloads/fit_rank_freq_60s_slu_all.rds")
####################individual learning
#load("/Users/bbarrett/Downloads/ind.rds")
load("/Users/bbarrett/Documents/bias_i_sa_60/ind.rds")
fit_i <- stanfit
post_ind <- extract(fit_i)
precis(fit_i , pars="log_lambda" , depth=3)
precis(fit_i , pars='lambda_i' , depth=2)
precis(fit_i , pars='phi_i' , depth=2)
# plot(precis(fit_i , pars='G' , depth=3))
plot(precis(fit_i , pars='psi' , depth=2))
plot(precis(fit_i , pars='phi_i' , depth=2))
plot(precis(fit_i , pars='lambda_i' , depth=2))

DensLambda(post_ind)
DensLambda(post_ind , individual=TRUE , age_sex = FALSE)
#DensLambda(post_ind , prior=FALSE , group=TRUE , age_sex = FALSE)

DensPhi(post_ind)
DensPhi(post_ind , individual=TRUE , age_sex = FALSE)

traceplot(fit_i , pars='sigma_i')
traceplot(fit_i , pars='log_lambda')
traceplot(fit_i , pars='logit_phi')
traceplot(fit_i , pars='Rho_i')
traceplot(fit_i , pars='G')
traceplot(fit_i , pars='psi')

# 
# #bias
# post_ind_bias <- extract(fit_i_bias)
# 
# precis(fit_i_bias)
# precis(fit_i_bias , pars='lambda_i' , depth=2)
# precis(fit_i_bias , pars='phi_i' , depth=2)
# precis(fit_i_bias , pars='psi' , depth=2)
# DensLambda(post_ind_bias )
# DensPhi(post_ind_bias )
# dens(post_ind_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_ind_bias$psi[,2] , col="red" , add=TRUE)
# 
# traceplot(fit_i_bias  , pars='sigma_i')
# traceplot(fit_i_bias  , pars='lambda')
# traceplot(fit_i_bias  , pars='phi')
# traceplot(fit_i_bias  , pars='psi')
# ###a
# post_ind_bias_a <- extract(fit_i_bias_a)
# 
# precis(fit_i_bias_a , pars='lambda_i' , depth=2)
# precis(fit_i_bias_a , pars='phi_i' , depth=2)
# precis(fit_i_bias_a , pars='psi' , depth=2)
# plot(precis(fit_i_bias_a , pars='A' , depth=3))
# DensLambda(post_ind_bias_a )
# DensPhi(post_ind_bias_a )
# 
# dens(post_ind_bias_a$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_ind_bias_a$psi[,2] , col="red" , add=TRUE)
# #lambda
# dens(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,1]) , col="red" , xlim=c(0,5))
# abline(v=median(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,1])), col="red")
# dens(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,2]) , col="green" , add=TRUE)
# abline(v=median(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,2])), col="green")
# dens(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,3]) , col="blue" , add=TRUE)
# abline(v=median(exp(post_ind_bias_a$mu[,1]  + post_ind_bias_a$A[,1,3])), col="blue")
# #phi
# dens(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,1]) , col="red" , xlim=c(0,1))
# abline(v=median(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,1])), col="red")
# dens(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,2]) , col="green" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,2])), col="green")
# dens(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,3]) , col="blue" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a$mu[,2]  + post_ind_bias_a$A[,2,3])), col="blue")
# 
# dens(post_ind_bias_a$A[,1,2] , add=TRUE , col="green")
# dens(post_ind_bias_a$A[,1,3] , add=TRUE , col="blue")
# ## as
# post_ind_bias_as <- extract(fit_i_bias_as)
# #lambda
# dens(exp( post_ind_bias_as$A[,1,1]) , col="red" , xlim=c(0,5))
# abline(v=median(exp( post_ind_bias_as$A[,1,1])), col="red")
# dens(exp(post_ind_bias_as$A[,1,2]) , col="green" , add=TRUE)
# abline(v=median(exp(post_ind_bias_as$A[,1,2])), col="green")
# dens(exp(post_ind_bias_as$A[,1,3]) , col="blue" , add=TRUE)
# abline(v=median(exp( post_ind_bias_as$A[,1,3])), col="blue")
# #phi
# dens(logistic( post_ind_bias_a2$A[,2,1]) , col="red" , xlim=c(0,1))
# abline(v=median(logistic( post_ind_bias_a2$A[,2,1])), col="red")
# dens(logistic(post_ind_bias_a2$A[,2,2]) , col="green" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a2$A[,2,2])), col="green")
# dens(logistic(post_ind_bias_a2$A[,2,3]) , col="blue" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a2$A[,2,3])), col="blue")
# 
# ###a2
# post_ind_bias_a2 <- extract(fit_i_bias_a2)
# str(post_ind_bias_a2)
# #lambda
# dens(exp( post_ind_bias_a2$A[,1,1]) , col="red" , xlim=c(0,5))
# abline(v=median(exp( post_ind_bias_a2$A[,1,1])), col="red")
# dens(exp(post_ind_bias_a2$A[,1,2]) , col="green" , add=TRUE)
# abline(v=median(exp(post_ind_bias_a2$A[,1,2])), col="green")
# dens(exp(post_ind_bias_a2$A[,1,3]) , col="blue" , add=TRUE)
# abline(v=median(exp( post_ind_bias_a2$A[,1,3])), col="blue")
# #phi
# dens(logistic( post_ind_bias_a2$A[,2,1]) , col="red" , xlim=c(0,1))
# abline(v=median(logistic( post_ind_bias_a2$A[,2,1])), col="red")
# dens(logistic(post_ind_bias_a2$A[,2,2]) , col="green" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a2$A[,2,2])), col="green")
# dens(logistic(post_ind_bias_a2$A[,2,3]) , col="blue" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_a2$A[,2,3])), col="blue")
# 
# ##as2 does the weird additive thing 
# post_ind_bias_as2 <- extract(fit_i_bias_as2)
# str(post_ind_bias_as2)
# dens(exp( post_ind_bias_as2$A[,1,1]) , col="red" , xlim=c(0,5))
# abline(v=median(exp( post_ind_bias_as2$A[,1,1])), col="red")
# dens(exp(post_ind_bias_as2$A[,1,2]) , col="green" , add=TRUE)
# abline(v=median(exp(post_ind_bias_as2$A[,1,2])), col="green")
# dens(exp(post_ind_bias_as2$A[,1,3]) , col="blue" , add=TRUE)
# abline(v=median(exp( post_ind_bias_as2$A[,1,3])), col="blue")
# #phi
# dens(logistic( post_ind_bias_as2$A[,2,1]) , col="red" , xlim=c(0,1))
# abline(v=median(logistic( post_ind_bias_as2$A[,2,1])), col="red")
# dens(logistic(post_ind_bias_as2$A[,2,2]) , col="green" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_as2$A[,2,2])), col="green")
# dens(logistic(post_ind_bias_as2$A[,2,3]) , col="blue" , add=TRUE)
# abline(v=median(logistic(post_ind_bias_as2$A[,2,3])), col="blue")
# 
# ##int
# post_ind_bias_as_int <- extract(fit_i_bias_as_int)
# dens(post_ind_bias_as_int$logit_phi[,1,])
# dens(post_ind_bias_as_int$logit_phi[,2,] , add=TRUE , col="blue")
# 
# dens(post_ind_bias_as_int$logit_phi[,,1])
# dens(post_ind_bias_as_int$logit_phi[,,2] , add=TRUE , col="blue")
# str(post_ind_bias_as_int)
# plot(precis(fit_i_bias_as_int, depth=3 , pars=c('log_lambda') ) )
# plot(precis(fit_i_bias_as_int, depth=3 , pars=c('logit_phi') ) )
# 
# ####final bposs?
# post_i2 <- extract(fit)
# 
# precis(fit)
# precis(fit , pars='lambda_i' , depth=2)
# precis(fit , pars='phi_i' , depth=2)
# plot(precis(fit , pars='phi_i' , depth=2))
# plot(precis(fit , pars='lambda_i' , depth=2))
# plot(precis(fit , pars='psi' , depth=2))
# 
# precis(fit , pars='G' , depth=3))
# precis(fit , pars='log_lambda' , depth=3))
# precis(fit , pars='logit_phi' , depth=3))
# 
# dens(post_i2$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_i2$psi[,2] , col="red" , add=TRUE)
# 
# 

#####################freq dep
# load("/Users/bbarrett/Downloads/freq.rds")
load("/Users/bbarrett/Documents/bias_i_sa_60/freq.rds")

fit_freq <- stanfit
post_freq <- extract(fit_freq)
str(post_freq)

precis(fit_freq)
precis(fit_freq , pars='lambda_i' , depth=2)
precis(fit_freq , pars='phi_i' , depth=2)
precis(fit_freq , pars='gamma_i' , depth=2)
precis(fit_freq , pars='fc_i' , depth=2)
precis(fit_freq , pars='sigma_i' , depth=2)
traceplot(fit_freq , pars='sigma_i')
traceplot(fit_freq , pars='sigma_g')
traceplot(fit_freq , pars='logit_phi')
traceplot(fit_freq , pars='logit_gamma')
traceplot(fit_freq , pars='log_f')

DensLambda(post_freq)
DensLambda(post_freq , prior=FALSE , individual=TRUE , age_sex=FALSE)
DensLambda(post_freq , group=TRUE , prior=FALSE , age_sex=FALSE)

DensPhi(post_freq)
DensGamma(post_freq)
DensFc(post_freq)
DensSigma(post_freq$sigma_i)
DensSigma(post_freq$sigma_g)


# #bias
# post_freq_bias <- extract(fit_freq_bias)
# str(post_freq_bias)
# 
# precis(fit_freq_bias)
# precis(fit_freq_bias , pars='lambda_i' , depth=2)
# precis(fit_freq_bias , pars='phi_i' , depth=2)
# precis(fit_freq_bias , pars='gamma_i' , depth=2)
# precis(fit_freq_bias , pars='fc_i' , depth=2)
# precis(fit_freq_bias , pars='sigma_i' , depth=2)
# precis(fit_freq_bias , pars='psi' , depth=2)
# 
# DensLambda(post_freq_bias)
# DensPhi(post_freq_bias)
# DensGamma(post_freq_bias)
# DensFc(post_freq_bias)
# dens(post_freq_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_freq_bias$psi[,2] , col="red" , add=TRUE)
# 
# traceplot(fit_freq_bias , pars='sigma_i')
# traceplot(fit_freq_bias , pars='lambda')
# traceplot(fit_freq_bias , pars='phi')
# traceplot(fit_freq_bias , pars='gamma')
# traceplot(fit_freq_bias , pars='fc')
# traceplot(fit_freq_bias , pars='psi')

################# male
#load("/Users/bbarrett/Downloads/male_cue.rds")
load("/Users/bbarrett/Documents/bias_i_sa_60/male_cue.rds")

fit_male <- stanfit
post_male <- extract(fit_male)
str(post_male)

precis(fit_male , pars=c('lambda' , 'phi' , 'gamma' , 'betaq') , depth=3)
precis(fit_male , pars='lambda_i' , depth=2)
precis(fit_male , pars='phi_i' , depth=2)
precis(fit_male , pars='gamma_i' , depth=2)
precis(fit_male , pars='betaq_i' , depth=2)

DensLambda(post_male)
DensPhi(post_male)
DensGamma(post_male)
DensBetaq(post_male)

traceplot(fit_male , pars='sigma_i')
traceplot(fit_male , pars='lambda')
traceplot(fit_male , pars='phi')
traceplot(fit_male , pars='gamma')
traceplot(fit_male , pars='betaq')

#bias
# post_male_bias <- extract(fit_male_bias)
# #str(post_male_bias)
# 
# precis(fit_male_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq') , depth=3)
# precis(fit_male_bias , pars='lambda_i' , depth=2)
# precis(fit_male_bias , pars='phi_i' , depth=2)
# precis(fit_male_bias , pars='gamma_i' , depth=2)
# precis(fit_male_bias , pars='betaq_i' , depth=2)
# 
# DensLambda(post_male_bias)
# DensPhi(post_male_bias)
# DensGamma(post_male_bias)
# DensBetaq(post_male_bias)
# dens(post_male_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_male_bias$psi[,2] , col="red" , add=TRUE)
# 
# traceplot(fit_male_bias , pars='sigma_i')
# traceplot(fit_male_bias , pars='lambda')
# traceplot(fit_male_bias , pars='phi')
# traceplot(fit_male_bias , pars='gamma')
# traceplot(fit_male_bias , pars='betaq')

#####################adult-bias
# load("/Users/bbarrett/Downloads/adult_cue.rds")
load("/Users/bbarrett/Documents/bias_i_sa_60/adult_cue.rds")

fit_adult <- stanfit
post_adult <- extract(fit_adult)
str(post_adult)

precis(fit_adult)
precis(fit_adult , pars='lambda_i' , depth=2)
precis(fit_adult , pars='phi_i' , depth=2)
precis(fit_adult , pars='gamma_i' , depth=2)
precis(fit_adult , pars='betaq_i' , depth=2)

DensLambda(post_adult)
DensPhi(post_adult)
DensGamma(post_adult)
DensBetaq(post_adult)

traceplot(fit_adult , pars='sigma_i')
traceplot(fit_adult , pars='lambda')
traceplot(fit_adult , pars='phi')
traceplot(fit_adult , pars='gamma')
traceplot(fit_adult , pars='betaq')

##bias
# 
# post_adult_bias <- extract(fit_adult_bias)
# #str(post_adult_bias)
# 
# precis(fit_adult_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
# precis(fit_adult_bias , pars='lambda_i' , depth=2)
# precis(fit_adult_bias , pars='phi_i' , depth=2)
# precis(fit_adult_bias , pars='gamma_i' , depth=2)
# precis(fit_adult_bias , pars='betaq_i' , depth=2)
# 
# DensLambda(post_adult_bias)
# DensPhi(post_adult_bias)
# DensGamma(post_adult_bias)
# DensBetaq(post_adult_bias)
# dens(post_adult_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
# dens(post_adult_bias$psi[,2] , col="red" , add=TRUE)
# 
# traceplot(fit_adult_bias , pars='sigma_i')
# traceplot(fit_adult_bias , pars='lambda')
# traceplot(fit_adult_bias , pars='phi')
# traceplot(fit_adult_bias , pars='gamma')
# traceplot(fit_adult_bias , pars='betaq')

#######################roost-bias
load("/Users/bbarrett/Documents/bias_i_sa_60/roost_cue.rds")
fit_roost<- stanfit
post_roost <- extract(fit_roost)
str(post_roost)

precis(fit_roost , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_roost , pars='lambda_i' , depth=2)
precis(fit_roost , pars='phi_i' , depth=2)
precis(fit_roost , pars='gamma_i' , depth=2)
precis(fit_roost , pars='betaq_i' , depth=2)

DensLambda(post_roost)
DensPhi(post_roost)
DensGamma(post_roost)
DensBetaq(post_roost)

traceplot(fit_roost , pars='sigma_i')
traceplot(fit_roost , pars='lambda')
traceplot(fit_roost , pars='phi')
traceplot(fit_roost , pars='gamma')
traceplot(fit_roost , pars='betaq')

##bias
post_roost_bias <- extract(fit_roost_bias)
#str(post_roost_bias)

precis(fit_roost_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_roost_bias , pars='lambda_i' , depth=2)
precis(fit_roost_bias , pars='phi_i' , depth=2)
precis(fit_roost_bias , pars='gamma_i' , depth=2)
precis(fit_roost_bias , pars='betaq_i' , depth=2)

DensLambda(post_roost_bias)
DensPhi(post_roost_bias)
DensGamma(post_roost_bias)
DensBetaq(post_roost_bias)
dens(post_roost_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_roost_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_roost_bias , pars='sigma_i')
traceplot(fit_roost_bias , pars='lambda')
traceplot(fit_roost_bias , pars='phi')
traceplot(fit_roost_bias , pars='gamma')
traceplot(fit_roost_bias , pars='betaq')

#################rank-bias
post_rank <- extract(fit_rank)

precis(fit_rank , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_rank , pars='lambda_i' , depth=2)
precis(fit_rank , pars='phi_i' , depth=2)
precis(fit_rank , pars='gamma_i' , depth=2)
plot(precis(fit_rank , pars='betaq_i' , depth=2))

DensLambda(post_rank)
DensPhi(post_rank)
DensGamma(post_rank)
DensBetaq(post_rank)

traceplot(fit_rank , pars='sigma_i')
traceplot(fit_rank , pars='lambda')
traceplot(fit_rank , pars='phi')
traceplot(fit_rank , pars='gamma')
traceplot(fit_rank , pars='betaq')

##bias
post_rank_bias <- extract(fit_rank_bias)
#str(post_rank_bias)

precis(fit_rank_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_rank_bias , pars='lambda_i' , depth=2)
precis(fit_rank_bias , pars='phi_i' , depth=2)
precis(fit_rank_bias , pars='gamma_i' , depth=2)
precis(fit_rank_bias , pars='betaq_i' , depth=2)

DensLambda(post_rank_bias)
DensPhi(post_rank_bias)
DensGamma(post_rank_bias)
DensBetaq(post_rank_bias)
dens(post_rank_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_rank_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_rank_bias , pars='sigma_i')
traceplot(fit_rank_bias , pars='lambda')
traceplot(fit_rank_bias , pars='phi')
traceplot(fit_rank_bias , pars='gamma')
traceplot(fit_rank_bias , pars='betaq')


#####male linear
load("/Users/bbarrett/Downloads/male_lin.rds")
fit_male_lin <- stanfit
post_male_lin <- extract(fit_male_lin)

DensLambda(post_male_lin)
DensPhi(post_male_lin)
DensGamma(post_male_lin)
############git freq_adult

traceplot(fit_adult_freq , pars='sigma_i')
traceplot(fit_adult_freq , pars='lambda')
traceplot(fit_adult_freq , pars='phi')
traceplot(fit_adult_freq , pars='gamma')
traceplot(fit_adult_freq , pars='fc')
traceplot(fit_adult_freq , pars='betaq')


###model comparison
compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost , fit_rank)

WAICtab <- compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost ,fit_i_bias , fit_freq_bias, fit_male_bias , fit_adult_bias , fit_roost_bias)



