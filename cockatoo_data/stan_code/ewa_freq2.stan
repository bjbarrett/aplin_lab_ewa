data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int L;              // num groups
    int tech[N];        // techique chosen
    real pay_i[N,K];    // observed personal yields of techs (1/0)
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
    int age_index[N];   // age index
    int sex_index[N];   // sex index
    int group_index[N];   // group index
    real ac_init[N,K];     // initil attraction scores
}

parameters {
  vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
  matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
  cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
/*  vector<lower=0>[N_effects] sigma_g;       // standard deviations of varying effects
  matrix[N_effects,L] zed_g;                // individual z-scores for cholesky decomp
  cholesky_factor_corr[N_effects] L_Rho_g;  // correlation matrix*/
  //simplex[K] psi;                           // simplex for bias
  matrix[3,3] log_lambda;                    
  matrix[3,3] logit_phi;                    
  matrix[3,3] logit_gamma;                    
  matrix[3,3] log_f;                    
}

transformed parameters{
  matrix[J,N_effects] I;              //define varying effects for individuals
  I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
  // matrix[L,N_effects] G;              //define varying effects for individuals
  // G = (diag_pre_multiply(sigma_g,L_Rho_g) * zed_g)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp       
    real phi;           // stickiness parameter to recent experience
    real lambda;        // sensitivity to attraction scores
    real gamma;         // social weight
    real fc;            // conform exponent

   //priors
    to_vector(log_lambda) ~  normal(1,0.6);
    to_vector(logit_phi) ~  normal(0,1);
    to_vector(logit_gamma) ~  normal(0,1);
    to_vector(log_f) ~  normal(0,0.8);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,1);
    L_Rho_i ~ lkj_corr_cholesky(3);
    // sigma_g ~ exponential(1);
    // to_vector(zed_g) ~ normal(0,0.5);
    // L_Rho_g ~ lkj_corr_cholesky(3);
    //psi ~ dirichlet(rep_vector(3,K));
    
    //likelihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j];
            } else {
                AC[j] = ac_init[i,j];
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( I[id[i],1]  + log_lambda[age_index[i],sex_index[i]] );
            phi= inv_logit( I[id[i],2] + logit_phi[age_index[i],sex_index[i]] );
            gamma = inv_logit(I[id[i],3]  + logit_gamma[age_index[i],sex_index[i]] );
            fc = exp(I[id[i],4] + log_f[age_index[i],sex_index[i]]);
            // lambda = exp( I[id[i],1] + G[group_index[i],1] + log_lambda[age_index[i],sex_index[i]] );
            // phi= inv_logit( I[id[i],2] + G[group_index[i],2] + logit_phi[age_index[i],sex_index[i]] );
            // gamma = inv_logit(I[id[i],3] + G[group_index[i],3] + logit_gamma[age_index[i],sex_index[i]] );
            // fc = exp(I[id[i],4] + G[group_index[i],4] + log_f[age_index[i],sex_index[i]]);
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
            if (sum( s[i] ) > 0 ) {
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                PrS = s_temp[tech[i]]/sum(s_temp);
                target += log( (1-gamma)*exp(logPrA) + gamma*PrS ) ;
            } else {
                target += logPrA;
            }
     }//i  

}//end of model

generated quantities{
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] s_temp;        
    real lambda_i[J];           // stickiness parameter
    real phi_i[J];           // stickiness parameter
    real gamma_i[J];         // social weight
    real fc_i[J];     // conform exponent
    matrix[N_effects,N_effects] Rho_i;
    // matrix[N_effects,N_effects] Rho_g;
    matrix[N,K] PrPreds;     

    Rho_i = multiply_lower_tri_self_transpose(L_Rho_i);
    // Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
    
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
            lambda_i[id[i]] = exp( I[id[i],1]  + log_lambda[age_index[i],sex_index[i]] );
            phi_i[id[i]] = inv_logit( I[id[i],2] + logit_phi[age_index[i],sex_index[i]] );
            gamma_i[id[i]] = inv_logit(I[id[i],3]  + logit_gamma[age_index[i],sex_index[i]] );
            fc_i[id[i]] = exp(I[id[i],4] + log_f[age_index[i],sex_index[i]]);
            // lambda_i[id[i]] = exp( I[id[i],1] + G[group_index[i],1] + log_lambda[age_index[i],sex_index[i]] );
            // phi_i[id[i]] = inv_logit( I[id[i],2] + G[group_index[i],2] + logit_phi[age_index[i],sex_index[i]] );
            // gamma_i[id[i]] = inv_logit(I[id[i],3] + G[group_index[i],3] + logit_gamma[age_index[i],sex_index[i]] );
            // fc_i[id[i]] = exp(I[id[i],4] + G[group_index[i],4] + log_f[age_index[i],sex_index[i]]);
        }

        logPrA = lambda_i[id[i]]*AC[tech[i]] - log_sum_exp( lambda_i[id[i]]*AC );

        //conformity aspect below
            if (sum( s[i] ) > 0 ) { //only socially learn when there is social information
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc_i[id[i]]);
                PrS = s_temp[tech[i]]/sum(s_temp);
                log_lik[i] =  log( (1-gamma_i[id[i]])*exp(logPrA) + gamma_i[id[i]]*PrS )  ; 
                for(j in 1:K){
                    PrPreds[i,j] = (1-gamma_i[id[i]])*exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) ) + gamma_i[id[i]]*(s_temp[j]/sum(s_temp)) ;
                }
            } else {
                 log_lik[i] = (logPrA);
                 for(j in 1:K){
                    PrPreds[i,j] = exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) );
                 }
            }
     }//i  
}//end of model
