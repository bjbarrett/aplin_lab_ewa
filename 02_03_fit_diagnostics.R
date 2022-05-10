#load model outpout
#load("/Users/bbarrett/Downloads/fit_rank_freq_60s_slu_all.rds")
load("~/Dropbox/bias_i_sa_60/ind.rds")
fit_i <- stanfit
post_ind <- extract(fit_i)

load("~/Dropbox/bias_i_sa_60/freq.rds")
fit_freq <- stanfit
post_freq <- extract(fit_freq)

load("~/Dropbox/bias_i_sa_60/male_cue.rds")
fit_male <- stanfit
post_male <- extract(fit_male)
dens(post_freq$log_f)
dens(post_freq$logit_gamma)
dens(post_freq$logit_phi)
dens(post_freq$log_lambda)
dens(post_freq$log_lambda)

load("~/Dropbox/bias_i_sa_60/adult_cue.rds")
fit_adult <- stanfit
post_adult <- extract(fit_adult)

load("~/Dropbox/bias_i_sa_60/roost_cue.rds")
fit_roost<- stanfit
post_roost <- extract(fit_roost)

load("~/Dropbox/bias_i_sa_60/male_lin.rds")
fit_male_lin <- stanfit
post_male_lin <- extract(fit_male_lin)

load("~/Dropbox/bias_i_sa_60/adult_lin.rds")
fit_adult_lin <- stanfit
post_adult_lin <- extract(fit_adult_lin)


####################individual learning
#load("/Users/bbarrett/Downloads/ind.rds")


DensLambda(post_ind)
DensLambda(post_ind , individual=TRUE , age_sex = FALSE)
DensLambda(post_ind , group=TRUE , age_sex = FALSE)

z <- precis(fit_i , pars="log_lambda" , depth=3)
z@row.names <- PlotRenameAgeSex(z@row.names)
plot(z)

DensPhi(post_ind)
DensPhi(post_ind , individual=TRUE , age_sex = FALSE)
DensPhi(post_ind , group=TRUE , age_sex = FALSE , prior=FALSE)

z <- precis(fit_i , pars="logit_phi" , depth=3)
z@row.names <- PlotRenameAgeSex(z@row.names)
plot(z)

plot(precis(fit_i , pars="sigma_i" , depth=3))
plot(precis(fit_i , pars="Rho_i" , depth=3))

#DensLambda(post_ind , prior=FALSE , group=TRUE , age_sex = FALSE)


# precis(fit_i , pars="log_lambda" , depth=3)
# precis(fit_i , pars='lambda_i' , depth=2)
# precis(fit_i , pars='phi_i' , depth=2)
# plot(precis(fit_i , pars='G' , depth=3))
# plot(precis(fit_i , pars='psi' , depth=2))
# plot(precis(fit_i , pars='phi_i' , depth=2))
# plot(precis(fit_i , pars='lambda_i' , depth=2))
precis(fit_i , pars='Rho_i' , depth=3)

DensLambda(post_ind)
DensLambda(post_ind , individual=TRUE , age_sex = FALSE)

DensPhi(post_ind)
DensPhi(post_ind , individual=TRUE , age_sex = FALSE)
#DensLambda(post_ind , prior=FALSE , group=TRUE , age_sex = FALSE)

DensSigma(post_ind$sigma_i , maintext = "Sigma Individuals")

# traceplot(fit_i , pars='sigma_i')
# traceplot(fit_i , pars='log_lambda')
# traceplot(fit_i , pars='logit_phi')
# traceplot(fit_i , pars='Rho_i')
# traceplot(fit_i , pars='G')
# traceplot(fit_i , pars='psi')

#####################freq dep


# precis(fit_freq)
# precis(fit_freq , pars='lambda_i' , depth=2)
# precis(fit_freq , pars='phi_i' , depth=2)
# precis(fit_freq , pars='gamma_i' , depth=2)
# precis(fit_freq , pars='fc_i' , depth=2)
# precis(fit_freq , pars='sigma_i' , depth=2)
traceplot(fit_freq , pars='sigma_i')
# traceplot(fit_freq , pars='sigma_g')
traceplot(fit_freq , pars='logit_phi')
# traceplot(fit_freq , pars='logit_gamma')
# traceplot(fit_freq , pars='log_f')
precis(fit_freq , pars='Rho_i' , depth=3)

DensLambda(post_freq)
DensLambda(post_freq , prior=FALSE , individual=TRUE , age_sex=FALSE)
#DensLambda(post_freq , group=TRUE , prior=FALSE , age_sex=FALSE)

DensPhi(post_freq)
DensPhi(post_freq , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensGamma(post_freq)
DensGamma(post_freq , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensFc(post_freq)
DensFc(post_freq , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensSigma(post_freq$sigma_i , main="Sigma Individual")
DensSigma(post_freq$sigma_g)


#caalculate % of posterior greater than 1 or 0 on log scale
str(post_freq)
dem <- length(post_freq$log_f[,1,1])

length(which(post_freq$log_f[,1,1] > 0)) / dem
length(which(post_freq$log_f[,2,1] > 0)) / dem
length(which(post_freq$log_f[,3,1] > 0)) / dem

length(which(post_freq$log_f[,1,2] > 0)) / dem
length(which(post_freq$log_f[,2,2] > 0)) / dem
length(which(post_freq$log_f[,3,2] > 0)) / dem

length(which(post_freq$log_f[,1,3] > 0)) / dem
length(which(post_freq$log_f[,2,3] > 0)) / dem
length(which(post_freq$log_f[,3,3] > 0)) / dem

################# male
#load("/Users/bbarrett/Downloads/male_cue.rds")


DensLambda(post_male)
DensLambda(post_male , prior=FALSE , individual=TRUE , age_sex=FALSE)
DensLambda(post_male , prior=FALSE , group=TRUE , age_sex=FALSE)

#DensLambda(post_freq , group=TRUE , prior=FALSE , age_sex=FALSE)

DensPhi(post_male)
DensPhi(post_male , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensGamma(post_male)
DensGamma(post_male , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensBetaq(post_male)
DensBetaq(post_male , prior=FALSE , individual=TRUE , age_sex=FALSE)

DensSigma(post_male$sigma_i)

precis(fit_male , pars=c('log_lambda' , 'logit_phi' , 'logit_gamma' , 'betaq') , depth=3)
# precis(fit_male , pars='lambda_i' , depth=2)
# precis(fit_male , pars='phi_i' , depth=2)
# precis(fit_male , pars='gamma_i' , depth=2)
# precis(fit_male , pars='betaq_i' , depth=2)
precis(fit_male , pars='Rho_i' , depth=3)

DensLambda(post_male)
DensPhi(post_male)
DensGamma(post_male)
DensBetaq(post_male)

traceplot(fit_male , pars='sigma_i')
traceplot(fit_male , pars='lambda')
traceplot(fit_male , pars='phi')
traceplot(fit_male , pars='gamma')
traceplot(fit_male , pars='betaq')

#####################adult-bias


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


DensLambda(post_male_lin)
DensPhi(post_male_lin)
DensGamma(post_male_lin)

#####adult linear

DensLambda(post_adult_lin)
DensPhi(post_adult_lin)
DensGamma(post_adult_lin)
############git freq_adult

traceplot(fit_adult_freq , pars='sigma_i')
traceplot(fit_adult_freq , pars='lambda')
traceplot(fit_adult_freq , pars='phi')
traceplot(fit_adult_freq , pars='gamma')
traceplot(fit_adult_freq , pars='fc')
traceplot(fit_adult_freq , pars='betaq')


###model comparison
compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost , fit_rank)
compare(fit_i , fit_freq, fit_male_lin,fit_adult_lin)

WAICtab <- compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost ,fit_i_bias , fit_freq_bias, fit_male_bias , fit_adult_bias , fit_roost_bias)


DensLambda(post_freq , unknown=TRUE)
DensLambda(post_freq , unknown=FALSE)

