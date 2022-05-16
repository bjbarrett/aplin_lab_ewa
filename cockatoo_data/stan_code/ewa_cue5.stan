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
    int N_effects;      // number of learning parameters to estimatåe
    int age_index[N];   // age index
    int sex_index[N];   // sex index
    int group_index[N];   // group index
    real ac_init[N,K];     // initil attraction scores
    int L;              // num groups
}

parameters {
  vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
  matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
  cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
  vector<lower=0>[N_effects] sigma_g;       // standard deviations of varying effects
  matrix[N_effects,L] zed_g;                // individual z-scores for cholesky decomp
  cholesky_factor_corr[N_effects] L_Rho_g;  // correlation matrix
  //simplex[K] psi;                           // simplex for bias
  matrix[3,3] ASl;                    
  matrix[3,3] ASp;                    
  matrix[3,3] ASg;                    
  matrix[3,3] ASb;    
  vector[N_effects] mu;                // individual z-scores for cholesky decomp
}

transformed parameters{
  matrix[J,N_effects] I;              //define varying effects for individuals
  I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
  matrix[L,N_effects] G;              //define varying effects for individuals
  G = (diag_pre_multiply(sigma_g,L_Rho_g) * zed_g)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;        // asocial learning logprob
    real PrS;           // social learning prob
    vector[K] lin_mod;  // loglinear model cues
    real lambda;        // sensitivity to attraction scores
    real phi;           // stickiness parameter to recent experience
    real gamma;         // social weight
    real betaq;          // cue-bias

    //priors
    mu[1] ~  normal(1,0.6);
    mu[2] ~  normal(0,1);
    mu[3] ~  normal(0,1);
    mu[4] ~  normal(0,1);
    to_vector(ASl) ~  normal(mu[1],0.5);
    to_vector(ASp) ~  normal(mu[2],0.5);
    to_vector(ASg) ~  normal(mu[3],0.5);
    to_vector(ASb) ~  normal(mu[4],0.5);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,0.8);
    L_Rho_i ~ lkj_corr_cholesky(4);
    sigma_g ~ exponential(1);
    to_vector(zed_g) ~ normal(0,0.8);
    L_Rho_g ~ lkj_corr_cholesky(4);

    for ( i in 1:N ) {
        //update attractions + reinforcement learning
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*(pay_i[i-1,j]);
            } else {
                AC[j] = ac_init[i,j];
            }
        }//j
        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( I[id[i],1] + G[group_index[i],1] + ASl[age_index[i],sex_index[i]] );
            phi= inv_logit( I[id[i],2] + G[group_index[i],2] + ASp[age_index[i],sex_index[i]] );
            gamma = inv_logit(I[id[i],3] + G[group_index[i],3] + ASg[age_index[i],sex_index[i]] );
            betaq = I[id[i],4] + G[group_index[i],4] + ASb[age_index[i],sex_index[i]];
            logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
        
        //social learning below
        if (sum( q[i] ) > 0 ) { // only socially learn if there is social info
                for ( j in 2:K ) {
                    lin_mod[j] = exp( betaq*q[i,j]);                 // compute non-frequency cue as log-linear model
                }
        lin_mod[1] = 1; // aliased outcome
        PrS = lin_mod[tech[i]]/sum(lin_mod);
        target += ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );
            } else {
                target += ( logPrA );
            }
    }//i  
}//end of model
}

generated quantities{
    vector[N] log_lik;
    vector[K] lin_mod;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] s_temp;        
    real lambda_i[J];           // stickiness parameter
    real phi_i[J];           // stickiness parameter
    real gamma_i[J];         // social weight
    real betaq_i[J];     // conform exponent
    matrix[N_effects,N_effects] Rho_i;
    matrix[N_effects,N_effects] Rho_g;
    matrix[N,K] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
    

    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi_i[id[i]])*AC[j] + phi_i[id[i]]*pay_i[i-1,j];
            } else {
                AC[j] = ac_init[i,j];
            }
        }//j

        if ( bout[i]==1 ) {
            lambda_i[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + ASl[age_index[i],sex_index[i]] );
            phi_i[id[i]] = inv_logit( I[id[i],2] + G[group_index[i],2] + ASp[age_index[i],sex_index[i]] );
            gamma_i[id[i]] = inv_logit(I[id[i],3] + G[group_index[i],3] + ASg[age_index[i],sex_index[i]] );
            betaq_i[id[i]] = I[id[i],4] + G[group_index[i],4] + ASb[age_index[i],sex_index[i]];
        }
        logPrA = lambda_i[id[i]]*AC[tech[i]] - log_sum_exp( lambda_i[id[i]]*AC );
        //only socially learn if there is social info
             if (sum( q[i] ) > 0 ) {
                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( betaq_i[id[i]]*q[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome
                // compute frequency cue
                PrS = lin_mod[tech[i]]/sum(lin_mod);
                log_lik[i] =  log( (1-gamma_i[id[i]])*exp(logPrA) + gamma_i[id[i]]*PrS )  ; 
                //pr gq
                for(j in 1:K){
                    PrPreds[i,j] = (1-gamma_i[id[i]])*exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) ) + gamma_i[id[i]]*(lin_mod[j]/sum(lin_mod)) ;
                }
            } else {
                 log_lik[i] = (logPrA);
                 
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) );
                 }
            }
     }//i  

}//end of model
