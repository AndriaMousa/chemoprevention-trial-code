
// The input data 
data {
  int N_obs; //total rows (number of time points observed)
  int <lower=0> T[N_obs]; //time point (observed)
  int row_model[N_obs];  // row of model output that will correspond to time point T in data (pre computed)
  real dt;  /// time step  
  int <lower=0> N;///number of time pts for the model
  int N_outcome[12,5];
  

}



// Model parameters 

parameters {
  real<lower=0> inc;      ///background infection rate (same for both groups)
  real<lower=1> w_R1;           ///shape parameter Resistant eg. 540 only
  real<lower=1> w_R2;           ///shape parameter Resistant eg. 540+581
  real<lower=1> w_S;           /// shape parameter Sensitive
  real<lower=1> lambda_R1;      ///scale parameter Resistant 540 only
  real<lower=1> lambda_R2;      ///scale parameter Resistant 540+581
  real<lower=1> lambda_S;      /// scale parameter Sensitive
  real<lower=0, upper=1> prob_determ; /// 
  simplex[3] freq;      /// so that all frequencies add up to 1 (freq_S+freq_R1+freq_R2)
}



///transformed parameters

transformed parameters {

  real time[N];
  real<lower=0, upper=1> pred_I_S[N];     // predicted total proportion infected with S among treated by time t (cumulative)
  real<lower=0, upper=1> pred_I_R1[N];     // predicted total proportion infected with R (540 only) among treated by time t (cumulative)
  real<lower=0, upper=1> pred_I_R2[N];     // predicted total proportion infected with R (540+581) among treated by time t (cumulative)
  real<lower=0, upper=1> pred_uninf[N];   // predicted proportion not infected among treated by time t (and are at risk in the next time step)
 
  real<lower=0, upper=1> pred_I_S_new[N]; // predicted proportion of new infections with S among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_I_R1_new[N]; // predicted proportion of new infections with R(540 only) among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_I_R2_new[N]; // predicted proportion of new infections with R(540+581) among treated group between the interval t and t-1 
 
  real<lower=0, upper=1> pred_I_undeterm_new[N]; // predicted proportion of new infections with undetermined resistance among treated group between the interval t and t-1 
  real<lower=0, upper=1> pred_I_undeterm[N]; 
  
  real<lower=0, upper=1> p_protect_S[N];  // Protective efficacy at each time pt against S
  real<lower=0, upper=1> p_protect_R1[N];  // Protective efficacy at each time pt against R (540)
  real<lower=0, upper=1> p_protect_R2[N];  // Protective efficacy at each time pt against R (540+581)
  
  
  real <lower=0, upper=1> prob_inf;  // Probability of infection at each time pt
  
  prob_inf= 1-exp(-inc*dt) ; // prob of infection at each time step (constant)

  // everyone is susceptible in the first time point
  
  pred_uninf[1]=1; 
  
  // nobody is infected in the first time point
  
  pred_I_S[1]=0; 
  pred_I_R1[1]=0;
  pred_I_R2[1]=0;
  
  pred_I_S_new[1]=0;
  pred_I_R1_new[1]=0;
  pred_I_R2_new[1]=0;
  
  pred_I_undeterm_new[1]=0;
  pred_I_undeterm[1]=0;

   p_protect_S[1]=1;
   p_protect_R1[1]=1;
   p_protect_R2[1]=1;

   time[1]=0;
  
  for(i in 2:N) {
    
    time[i]=time[i-1]+dt;
    
    //protection against each strain, evaluated for each time step
    p_protect_S[i]= exp(-((time[i])/lambda_S)^w_S);
    p_protect_R1[i]= exp(-((time[i])/lambda_R1)^w_R1);
    p_protect_R2[i]= exp(-((time[i])/lambda_R2)^w_R2);

  pred_I_S_new[i]=  (pred_uninf[i-1]*prob_inf*(freq[1])*(1-p_protect_S[i]))*prob_determ;  // proportion of new infections with S in treated group between time t and t-1
  pred_I_R1_new[i]=  (pred_uninf[i-1]*prob_inf* (freq[2]) *(1-p_protect_R1[i]))*prob_determ;     // proportion of new infections with R in treated group between time t and t-1
  pred_I_R2_new[i]=  (pred_uninf[i-1]*prob_inf* (freq[3] )*(1-p_protect_R2[i]))*prob_determ;     // proportion of new infections with R in treated group between time t and t-1
  pred_I_undeterm_new[i]= (1-prob_determ)*((pred_uninf[i-1]*prob_inf*(freq[1])*(1-p_protect_S[i])) + (pred_uninf[i-1]*prob_inf* freq[2] *(1-p_protect_R1[i]))  + (pred_uninf[i-1]*prob_inf* freq[3] *(1-p_protect_R2[i])));
 
    pred_I_S[i]= pred_I_S[i-1]+ pred_I_S_new[i];  // proportion of total cumul infections with S in treated group by time t
    pred_I_R1[i]= pred_I_R1[i-1]+ pred_I_R1_new[i];     // proportion of total cumul infections with R in treated group by time t
    pred_I_R2[i]= pred_I_R2[i-1]+ pred_I_R2_new[i];     // proportion of total cumul infections with R in treated group by time t
    pred_uninf[i]=pred_uninf[i-1] -  pred_I_S_new[i] - pred_I_R1_new[i] - pred_I_R2_new[i] - pred_I_undeterm_new[i] ;        // proportion without an infection in treated group by time t
    
    pred_I_undeterm[i]=pred_I_undeterm[i-1] + pred_I_undeterm_new[i];
    
    }



}


// The model to be estimated. 

model{ 
  
  // intermediate variables for computing probability of infection between this time and the previous in each row of data.
  
matrix[12,5] probabilities_multi;
// use model block to declare priors &  likelihood 
// priors
inc    ~ gamma(0.001,0.001);
lambda_R1 ~ gamma(5,0.2);
w_R1     ~ gamma(4,0.8);
lambda_R2 ~ gamma(5,0.2);
w_R2     ~ gamma(4,0.8);
lambda_S ~ gamma(5,0.2);
w_S      ~ gamma(4,0.8);

///not specifying freq- assuming uninformative uniform(0,1)

  
// likelihood ---- edit
for(j in 2:N_obs) {


probabilities_multi[j-1,1]=(pred_I_R1[row_model[j]]- pred_I_R1[row_model[j-1]])/pred_uninf[row_model[j-1]];
probabilities_multi[j-1,2]=(pred_I_R2[row_model[j]]- pred_I_R2[row_model[j-1]])/pred_uninf[row_model[j-1]];
probabilities_multi[j-1,3]=(pred_I_S[row_model[j]]- pred_I_S[row_model[j-1]])/pred_uninf[row_model[j-1]];
probabilities_multi[j-1,4]=pred_uninf[row_model[j]]/pred_uninf[row_model[j-1]];
probabilities_multi[j-1,5]=(pred_I_undeterm[row_model[j]]- pred_I_undeterm[row_model[j-1]])/pred_uninf[row_model[j-1]];

}

for (time_pt in 1:12){

N_outcome[time_pt]~multinomial(to_vector(probabilities_multi[time_pt]));
}


}


generated quantities {

real pr_time[N,1];
real<lower=0, upper=1> pr_I_S[N,1];     // predicted total proportion infected with S among treated by time t (cumulative)
real<lower=0, upper=1> pr_I_R1[N,1];     // predicted total proportion infected with R among treated by time t (cumulative)
real<lower=0, upper=1> pr_I_R2[N,1];     // predicted total proportion infected with R among treated by time t (cumulative)
real<lower=0, upper=1> pr_uninf[N,1];   // predicted proportion not infected among treated by time t (and are at risk in the next time step)
real<lower=0, upper=1> pr_I_S_new[N,1]; // predicted proportion of new infections with S among treated group between the interval t and t-1
real<lower=0, upper=1> pr_I_R1_new[N,1]; // predicted proportion of new infections with R among treated group between the interval t and t-1
real<lower=0, upper=1> pr_I_R2_new[N,1]; // predicted proportion of new infections with R among treated group between the interval t and t-1
real<lower=0, upper=1> pr_I_undeterm_new[N,1];
real<lower=0, upper=1> pr_I_undeterm[N,1];


real<lower=0, upper=1> pr_protect_S[N,1];  // Protective efficacy at each time pt against S
real<lower=0, upper=1> pr_protect_R1[N,1];  // Protective efficacy at each time pt against R
real<lower=0, upper=1> pr_protect_R2[N,1];  // Protective efficacy at each time pt against R

real <lower=0, upper=1> pr_prob_inf;  // Probability of infection at each time pt

pr_prob_inf= 1-exp(-inc*dt) ; // prob of infection at each time step (constant)


// everyone is susceptible in the first time point

pr_uninf[1,1]=1;

// nobody is infected in the first time point

pr_I_S[1,1]=0;
pr_I_R1[1,1]=0;
pr_I_R2[1,1]=0;
pr_I_S_new[1,1]=0;
pr_I_R1_new[1,1]=0;
pr_I_R2_new[1,1]=0;
pr_I_undeterm[1,1]=0;
pr_I_undeterm_new[1,1]=0;

 pr_protect_S[1,1]=1;
 pr_protect_R1[1,1]=1;
 pr_protect_R2[1,1]=1;

 pr_time[1,1]=0;



for(i in 2:N) {

  pr_time[i,1]=pr_time[i-1,1]+dt;

  //protection against each strain, evaluated for each time step
  pr_protect_S[i,1]= exp(-((pr_time[i,1])/lambda_S)^w_S);
  pr_protect_R1[i,1]= exp(-((pr_time[i,1])/lambda_R1)^w_R1);
  pr_protect_R2[i,1]= exp(-((pr_time[i,1])/lambda_R2)^w_R2);

  pr_I_S_new[i,1]=  (pr_prob_inf*freq[1]*pr_uninf[i-1,1]*(1-pr_protect_S[i,1])*prob_determ);  // proportion of new infections with S in treated group between time t and t-1
  pr_I_R1_new[i,1]=  (pr_prob_inf*freq[2]*pr_uninf[i-1,1]*(1-pr_protect_R1[i,1])*prob_determ);     // proportion of new infections with R in treated group between time t and t-1
  pr_I_R2_new[i,1]=  (pr_prob_inf*freq[3]*pr_uninf[i-1,1]*(1-pr_protect_R2[i,1])*prob_determ);     // proportion of new infections with R in treated group between time t and t-1
  pr_I_undeterm_new[i,1]=((pr_prob_inf*freq[1]*pr_uninf[i-1,1]*(1-pr_protect_S[i,1]) )+ (pr_prob_inf*freq[2]*pr_uninf[i-1,1]*(1-pr_protect_R1[i,1]))+ (pr_prob_inf*freq[3]*pr_uninf[i-1,1]*(1-pr_protect_R2[i,1])))*(1-prob_determ);

  pr_I_S[i,1]= pr_I_S[i-1,1]+  pr_I_S_new[i,1];  // proportion of total cumul infections with S in treated group by time t
  pr_I_R1[i,1]= pr_I_R1[i-1,1]+  pr_I_R1_new[i,1];     // proportion of total cumul infections with R in treated group by time t
  pr_I_R2[i,1]= pr_I_R2[i-1,1]+  pr_I_R2_new[i,1];     // proportion of total cumul infections with R in treated group by time t
  pr_I_undeterm[i,1]= pr_I_undeterm[i-1,1] +pr_I_undeterm_new[i,1];

  pr_uninf[i,1]=pr_uninf[i-1,1] -   pr_I_S_new[i,1]- pr_I_R1_new[i,1] - pr_I_R2_new[i,1]-  pr_I_undeterm_new[i,1];// proportion without an infection in treated group by time t

}
}


