##individual learning 

post_i <- extract(fit_i)

precis(fit_i)
precis(fit_i , pars='lambda_i' , depth=2)
precis(fit_i , pars='phi_i' , depth=2)
plot(precis(fit_i , pars='phi_i' , depth=2))
DensLambda(post_i)
DensPhi(post_i)

traceplot(fit_i , pars='sigma_i')
traceplot(fit_i , pars='lambda')
traceplot(fit_i , pars='phi')

##freq dep
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
PCI(post_freq$fc , prob=.55)
length(which(post_freq$fc>1))/length(post_freq$fc>1) #prob mass suggesting conformist transmission

## male
post_male <- extract(fit_male)
str(post_male)

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

##adult-bias
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

##roost-bias
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


##rank-bias
post_rank <- extract(fit_rank)
str(post_rank)

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

###model comparison
compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost , fit_rank)
