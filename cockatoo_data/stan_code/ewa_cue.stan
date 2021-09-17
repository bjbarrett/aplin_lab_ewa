
data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real pay_i[N,K];    // observed personal yields of techs (1/0)
    real q[N,K];        // observed cue social variables of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
}

parameters {
    vector[N_effects] mu;                     //population means
    vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
    matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
    cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
}
transformed parameters{
    matrix[J,N_effects] I;              //define varying effects for individuals
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
}
model {
    vector[K] AC;       // attraction scores
    real logPrA;        // asocial learning logprob
    real PrS;           // social learning prob
    vector[K] lin_mod;  // loglinear model cues
    real lambda;        // sensitivity to attraction scores
    real phi;           // stickiness parameter to recent experience
    real gamma;         // social weight
    real beta;          // cue-bias

    //priors
    mu[1] ~ normal(1,0.6);
    mu[2] ~ normal(0,1);
    mu[3] ~ normal(0,1);
    mu[4] ~ normal(0,0.8);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,1);
    L_Rho_i ~ lkj_corr_cholesky(3);

    for ( i in 1:N ) {
        //update attractions + reinforcement learning
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j
        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( mu[1] + I[id[i],1] ) ;
            phi = inv_logit(  mu[2] + I[id[i],2] );
            gamma = inv_logit(mu[3] + I[id[i],3]  );
            beta = mu[4] + I[id[i],4];
        }
        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
        
        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta*q[i,j]);                 // compute non-frequency cue as log-linear model
                }
                lin_mod[1] = 1; // aliased outcome
                PrS = lin_mod[tech[i]]/sum(lin_mod);
                target += ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );
            } else {
                target += ( logPrA );
            }
        } else {
            target += ( logPrA );
         }
     }//i  

}//end of model

generated quantities{
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning logPr
    real PrS;        // social learning prob
    vector[K] lin_mod;
    real lambda[J];           // stickiness parameter
    real phi[J];           // stickiness parameter
    real gamma[J];         // social weight
    real beta[J];     // cue-bias
    matrix[N_effects,N_effects] Rho_i;
    matrix[N,K] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);


    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi[id[i]])*AC[j] + phi[id[i]]*pay_i[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda[id[i]] = exp( mu[1] + I[id[i],1] ) ;
            phi[id[i]]= inv_logit(  mu[2] + I[id[i],2] );
            gamma[id[i]] = inv_logit(mu[3] + I[id[i],3]  );
            beta[id[i]] = mu[4] + I[id[i],4];
        }

        logPrA = lambda[id[i]]*AC[tech[i]] - log_sum_exp( lambda[id[i]]*AC );

        //only socially learn if there is social info
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta[id[i]]*q[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome
                // compute frequency cue
                PrS = lin_mod[tech[i]]/sum(lin_mod);

                log_lik[i] =  log( (1-gamma[id[i]])*exp(logPrA) + gamma[id[i]]*PrS )  ; 
                
                for(j in 1:K){
                PrPreds[i,j] = (1-gamma[id[i]])*exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) ) + gamma[id[i]]*(lin_mod[j]/sum(lin_mod)) ;
                }
                
            } else {
                 log_lik[i] = (logPrA);
                 
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) );
                 }
            }
        } else {
                 log_lik[i] = (logPrA);
                 
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda[id[i]]*AC[j] - log_sum_exp( lambda[id[i]]*AC) );
                }
            }
     }//i  

}//end of model