#load model outpout
load("/Users/bbarrett/Downloads/fit_rank_freq_60s_slu_all.rds")
####################individual learning 
post_ind <- extract(fit_i)

precis(fit_i)
precis(fit_i , pars='lambda_i' , depth=2)
precis(fit_i , pars='phi_i' , depth=2)
plot(precis(fit_i , pars='phi_i' , depth=2))
DensLambda(post_ind)
DensPhi(post_ind)

traceplot(fit_i , pars='sigma_i')
traceplot(fit_i , pars='lambda')
traceplot(fit_i , pars='phi')

#bias
post_ind_bias <- extract(fit_i_bias)

precis(fit_i_bias)
precis(fit_i_bias , pars='lambda_i' , depth=2)
precis(fit_i_bias , pars='phi_i' , depth=2)
precis(fit_i_bias , pars='psi' , depth=2)
DensLambda(post_ind_bias )
DensPhi(post_ind_bias )
dens(post_ind_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_ind_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_i_bias  , pars='sigma_i')
traceplot(fit_i_bias  , pars='lambda')
traceplot(fit_i_bias  , pars='phi')
traceplot(fit_i_bias  , pars='psi')

#####################freq dep
fit_freq <- readRDS("/Users/bbarrett/Downloads/fit_freq_60s_slu_all.rds")

post_freq <- extract(fit_freq)
str(post_freq)

precis(fit_freq)
precis(fit_freq , pars='lambda_i' , depth=2)
precis(fit_freq , pars='phi_i' , depth=2)
precis(fit_freq , pars='gamma_i' , depth=2)
precis(fit_freq , pars='fc_i' , depth=2)
precis(fit_freq , pars='sigma_i' , depth=2)

DensLambda(post_freq)
DensPhi(post_freq)
DensGamma(post_freq)
DensFc(post_freq)
traceplot(fit_freq , pars='sigma_i')
traceplot(fit_freq , pars='lambda')
traceplot(fit_freq , pars='phi')
traceplot(fit_freq , pars='gamma')
traceplot(fit_freq , pars='fc')

apply(post_freq$fc_i , 2, median)

#bias
post_freq_bias <- extract(fit_freq_bias)
str(post_freq_bias)

precis(fit_freq_bias)
precis(fit_freq_bias , pars='lambda_i' , depth=2)
precis(fit_freq_bias , pars='phi_i' , depth=2)
precis(fit_freq_bias , pars='gamma_i' , depth=2)
precis(fit_freq_bias , pars='fc_i' , depth=2)
precis(fit_freq_bias , pars='sigma_i' , depth=2)
precis(fit_freq_bias , pars='psi' , depth=2)

DensLambda(post_freq_bias)
DensPhi(post_freq_bias)
DensGamma(post_freq_bias)
DensFc(post_freq_bias)
dens(post_freq_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_freq_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_freq_bias , pars='sigma_i')
traceplot(fit_freq_bias , pars='lambda')
traceplot(fit_freq_bias , pars='phi')
traceplot(fit_freq_bias , pars='gamma')
traceplot(fit_freq_bias , pars='fc')
traceplot(fit_freq_bias , pars='psi')

################# male
post_male <- extract(fit_male)
#str(post_male)

precis(fit_male , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
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
post_male_bias <- extract(fit_male_bias)
#str(post_male_bias)

precis(fit_male_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_male_bias , pars='lambda_i' , depth=2)
precis(fit_male_bias , pars='phi_i' , depth=2)
precis(fit_male_bias , pars='gamma_i' , depth=2)
precis(fit_male_bias , pars='betaq_i' , depth=2)

DensLambda(post_male_bias)
DensPhi(post_male_bias)
DensGamma(post_male_bias)
DensBetaq(post_male_bias)
dens(post_male_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_male_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_male_bias , pars='sigma_i')
traceplot(fit_male_bias , pars='lambda')
traceplot(fit_male_bias , pars='phi')
traceplot(fit_male_bias , pars='gamma')
traceplot(fit_male_bias , pars='betaq')

#####################adult-bias
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

post_adult_bias <- extract(fit_adult_bias)
#str(post_adult_bias)

precis(fit_adult_bias , pars=c('lambda' , 'phi' , 'gamma' , 'betaq'))
precis(fit_adult_bias , pars='lambda_i' , depth=2)
precis(fit_adult_bias , pars='phi_i' , depth=2)
precis(fit_adult_bias , pars='gamma_i' , depth=2)
precis(fit_adult_bias , pars='betaq_i' , depth=2)

DensLambda(post_adult_bias)
DensPhi(post_adult_bias)
DensGamma(post_adult_bias)
DensBetaq(post_adult_bias)
dens(post_adult_bias$psi[,1] , col="blue" , xlim=c(.4,.6))
dens(post_adult_bias$psi[,2] , col="red" , add=TRUE)

traceplot(fit_adult_bias , pars='sigma_i')
traceplot(fit_adult_bias , pars='lambda')
traceplot(fit_adult_bias , pars='phi')
traceplot(fit_adult_bias , pars='gamma')
traceplot(fit_adult_bias , pars='betaq')

#######################roost-bias
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



