

// The input data 
data {
  int N_obs; //total rows (number of time points observed)
  int <lower=0> T[N_obs]; //time point (observed)
  int row_model[N_obs];  // row of model output that will correspond to time point T in data (pre computed)
  real dt;  /// time step  
  int <lower=0> N;///number of time pts for the model
  int N_outcome[(N_obs-1),4];
  

}



// Model parameters 

parameters {
  real<lower=0> inc;      ///background infection rate (same for both groups)
  real<lower=1> w_R;           ///shape parameter Resistant
  real<lower=1> w_S;           /// shape parameter Sensitive
  real<lower=1> lambda_R;      ///scale parameter Resistant
  real<lower=1> lambda_S;      /// scale parameter Sensitive
  real<lower=0, upper=1> freq; /// frequency of resistant parasite 
  real<lower=0, upper=1> prob_determ; /// probability of determining resistance genotype in a confirmed infection

}



///transformed parameters

transformed parameters {

  real time[N];
  real<lower=0, upper=1> pred_treated_I_S[N];     // predicted total proportion infected with S among treated by time t (cumulative)
  real<lower=0, upper=1> pred_treated_I_R[N];     // predicted total proportion infected with R among treated by time t (cumulative)
  real<lower=0, upper=1> pred_treated_uninf[N];   // predicted proportion not infected among treated by time t (and are at risk in the next time step)
  real<lower=0, upper=1> pred_treated_I_S_new[N]; // predicted proportion of new infections with S among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_treated_I_R_new[N]; // predicted proportion of new infections with R among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_treated_I_undeterm_new[N]; // predicted proportion of new infections with undetermined resistance among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_treated_I_undeterm[N]; 
  
  real<lower=0, upper=1> p_protect_S[N];  // Protective efficacy at each time pt against S
  real<lower=0, upper=1> p_protect_R[N];  // Protective efficacy at each time pt against R 
  
  
  real <lower=0, upper=1> prob_inf;  // Probability of infection at each time pt
  
  prob_inf= 1-exp(-inc*dt) ; // prob of infection at each time step (constant)
  
  
  
  // everyone is susceptible in the first time point
  
  pred_treated_uninf[1]=1; 
  
  // nobody is infected in the first time point
  
  pred_treated_I_S[1]=0; 
  pred_treated_I_R[1]=0;
  pred_treated_I_S_new[1]=0;
  pred_treated_I_R_new[1]=0;
  pred_treated_I_undeterm_new[1]=0;
  pred_treated_I_undeterm[1]=0;

   p_protect_S[1]=1;
   p_protect_R[1]=1;
   
   time[1]=0;
  
  for(i in 2:N) {
    
    time[i]=time[i-1]+dt;
    
    //protection against each strain, evaluated for each time step
    p_protect_S[i]= exp(-((time[i])/lambda_S)^w_S);
    p_protect_R[i]= exp(-((time[i])/lambda_R)^w_R);
    
    pred_treated_I_S_new[i]=  (pred_treated_uninf[i-1]*prob_inf*(1-freq)*(1-p_protect_S[i]))*prob_determ;  // proportion of new infections with S in treated group between time t and t-1
    pred_treated_I_R_new[i]=  (pred_treated_uninf[i-1]*prob_inf*(freq)*(1-p_protect_R[i]))*prob_determ;     // proportion of new infections with R in treated group between time t and t-1
    pred_treated_I_undeterm_new[i]= ((pred_treated_uninf[i-1]*prob_inf*(1-freq)*(1-p_protect_S[i])) + (pred_treated_uninf[i-1]*prob_inf*(freq)*(1-p_protect_R[i])))*(1-prob_determ);
    
    
    pred_treated_I_S[i]= pred_treated_I_S[i-1]+ pred_treated_I_S_new[i];  // proportion of total cumul infections with S in treated group by time t
    pred_treated_I_R[i]= pred_treated_I_R[i-1]+ pred_treated_I_R_new[i];     // proportion of total cumul infections with R in treated group by time t
    pred_treated_uninf[i]=pred_treated_uninf[i-1] -  pred_treated_I_S_new[i] - pred_treated_I_R_new[i] - pred_treated_I_undeterm_new[i] ;        // proportion without an infection in treated group by time t
    
    pred_treated_I_undeterm[i]=pred_treated_I_undeterm[i-1] + pred_treated_I_undeterm_new[i];
    
    }



}


// The model to be estimated. 

model{ 
  
  // intermediate variables for computing probability of infection between this time and the previous in each row of data.
  
matrix[(N_obs-1),4] probabilities_multi;

// use model block to declare priors &  likelihood 
// priors
inc    ~ gamma(0.001,0.001);
lambda_R ~ gamma(5,0.2);
w_R      ~ gamma(4,0.8);
lambda_S ~ gamma(5,0.2);
w_S      ~ gamma(4,0.8);
///not specifying freq- assuming uninformative uniform(0,1)

  
// likelihood ---- edit
for(j in 2:N_obs) {


probabilities_multi[j-1,1]=(pred_treated_I_R[row_model[j]]- pred_treated_I_R[row_model[j-1]])/pred_treated_uninf[row_model[j-1]];
probabilities_multi[j-1,2]=(pred_treated_I_S[row_model[j]]- pred_treated_I_S[row_model[j-1]])/pred_treated_uninf[row_model[j-1]];
probabilities_multi[j-1,3]=pred_treated_uninf[row_model[j]]/pred_treated_uninf[row_model[j-1]];
probabilities_multi[j-1,4]=(pred_treated_I_undeterm[row_model[j]]- pred_treated_I_undeterm[row_model[j-1]])/pred_treated_uninf[row_model[j-1]];

}

for (time_pt in 1:(N_obs-1)){

N_outcome[time_pt]~multinomial(to_vector(probabilities_multi[time_pt]));
}


}




