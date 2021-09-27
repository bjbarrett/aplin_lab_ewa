data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real s[N,K];        // observed number of times observing behaviors
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
}

parameters {
    real mu;                     //population means
    real<lower=0> sigma_i;       // standard deviations of varying effects
    vector[J] I;              //define varying effects for individuals

}

transformed parameters{
}

model {
    
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp       
    real fc;            // conform exponent

   //priors
    mu ~ normal(0,0.8);
    sigma_i ~ exponential(1);
    I ~ normal(0,sigma_i);
    //likelihood loop
    for ( i in 1:N ) {
        if ( bout[i]==1 ) {
            fc = exp(mu + I[id[i]]);
        }
        //conformity aspect below
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                PrS = s_temp[tech[i]]/sum(s_temp);
                target += log( PrS ) ;
            
     }//i  

}//end of model
