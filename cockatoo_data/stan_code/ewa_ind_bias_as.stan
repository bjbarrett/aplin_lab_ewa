data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real pay_i[N,K];    // observed personal yields of techs (1/0)
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
    int age_index[N];   // age index
    int sex_index[N];   // sex index
}

parameters {
    vector[N_effects] mu;                     //population means
    vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
    matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
    cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
    simplex[K] psi;                           // simplex for bias
    matrix[N_effects,3] A;                    //age means
    matrix[N_effects,3] S;                    //sex  means
}

transformed parameters{
    matrix[J,N_effects] I;              //define varying effects for individuals
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr
    real phi;           // stickiness parameter to recent experience
    real lambda;        // sensitivity to attraction scores


   //priors
    to_vector(mu) ~ normal(0,1);
    //mu[2] ~ normal(0,1);
    to_vector(A) ~ normal(0,1);
    to_vector(S) ~ normal(0,1);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,1);
    L_Rho_i ~ lkj_corr_cholesky(3);
    psi ~ dirichlet(rep_vector(4,K));
    //liklihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*pay_i[i-1,j] + psi[j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( mu[1] + I[id[i],1] + A[1,age_index[i]] + S[1,sex_index[i]]) ;
            phi= inv_logit(  mu[2] + I[id[i],2] + A[2,age_index[i]] + S[2,age_index[i]]);
            //lambda = exp( I[id[i],1] + G[group_index[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
        target += logPrA;
     }//i  

}//end of model

generated quantities{
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    vector[J] lambda_i;           // stickiness parameter
    vector[J] phi_i;           // stickiness parameter
    real lambda;
    real phi;
    matrix[N_effects,N_effects] Rho_i;
    matrix[N,K] PrPreds;

    Rho_i = L_Rho_i * L_Rho_i';
    lambda = exp(mu[1]);
    phi = inv_logit(mu[2]);
    
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi_i[id[i]])*AC[j] + phi_i[id[i]]*pay_i[i-1,j] + psi[j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            lambda_i[id[i]] = exp( mu[1] + I[id[i],1] + A[1,age_index[i]] + S[1,sex_index[i]]) ;
            phi_i[id[i]] = inv_logit(  mu[2] + I[id[i],2] + A[2,age_index[i]] + S[2,sex_index[i]]);
        }

        logPrA = lambda_i[id[i]]*AC[tech[i]] - log_sum_exp( lambda_i[id[i]]*AC );

         log_lik[i] = (logPrA);
         for(j in 1:K){
            PrPreds[i,j] = exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) );
        }
     }//i  
}//end of model
