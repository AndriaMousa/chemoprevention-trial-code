

// The input data 
data {
  int N_obs; //total rows (number of time points observed)
  int <lower=0> T[N_obs]; //time point (observed)
  int row_model[N_obs];  // row of model output that will correspond to time point T in data (pre computed)
  real dt;  /// time step  
  int <lower=0> N;///number of time pts for the model
  int N_outcome[(N_obs-1),2];
  int N_outcome_control[(N_obs-1),2];


}



// Model parameters 

parameters {
  real<lower=0> inc;      ///background infection rate (same for both groups)
  real<lower=1> w;           ///shape parameter 
  real<lower=1> lambda;      ///scale parameter 
}



///transformed parameters

transformed parameters {

  real time[N];
  real<lower=0, upper=1> pred_treated_I[N];     // predicted total proportion infected among treated by time t (cumulative)
  real<lower=0, upper=1> pred_treated_uninf[N];   // predicted proportion not infected among treated by time t (and are at risk in the next time step)
  real<lower=0, upper=1> pred_treated_I_new[N]; // predicted proportion of new infections among treated group between the interval t and t-1 

  real<lower=0, upper=1> p_protect[N];  // Protective efficacy at each time pt

  real<lower=0, upper=1> pred_control_I[N];     // predicted total proportion infected among control by time t (cumulative)
  real<lower=0, upper=1> pred_control_uninf[N];   // predicted proportion not infected among control by time t (and are at risk in the next time step)
  real<lower=0, upper=1> pred_control_I_new[N]; // predicted proportion of new infections  among control group between the interval t and t-1 

  
  real <lower=0, upper=1> prob_inf;  // Probability of infection at each time pt
  
  prob_inf= 1-exp(-inc*dt) ; // prob of infection at each time step (constant)
  
  
  
  // everyone is susceptible in the first time point
  
  pred_treated_uninf[1]=1; 
  pred_control_uninf[1]=1; 

  // nobody is infected in the first time point
  
  pred_treated_I[1]=0; 
  pred_treated_I_new[1]=0;


   p_protect[1]=1;

  pred_control_I[1]=0; 
  pred_control_I_new[1]=0;
 


   time[1]=0;
  
  for(i in 2:N) {
    
    time[i]=time[i-1]+dt;
    
    //protection against each strain, evaluated for each time step
    p_protect[i]= exp(-((time[i])/lambda)^w);

    pred_treated_I_new[i]=  (pred_treated_uninf[i-1]*prob_inf*(1-p_protect[i]));  // proportion of new infections with S in treated group between time t and t-1

    
    pred_treated_I[i]= pred_treated_I[i-1]+ pred_treated_I_new[i];  // proportion of total cumul infections with S in treated group by time t
    pred_treated_uninf[i]=pred_treated_uninf[i-1] -  pred_treated_I_new[i] ;        // proportion without an infection in treated group by time t
    
    pred_control_I_new[i]=pred_control_uninf[i-1]*prob_inf;  // proportion of new infections with S in control group between time t and t-1

    
    pred_control_I[i]= pred_control_I[i-1]+ pred_control_I_new[i];  // proportion of total cumul infections with S in control group by time t
    pred_control_uninf[i]=pred_control_uninf[i-1] -  pred_control_I_new[i] ;        // proportion without an infection in control group by time t
    
    }



}


// The model to be estimated. 

model{ 
  
  // intermediate variables for computing probability of infection between this time and the previous in each row of data.
  
matrix[(N_obs-1),2] probabilities_multi;
matrix[(N_obs-1),2] probabilities_multi_control;

// use model block to declare priors &  likelihood 
// priors
inc    ~ gamma(0.001,0.001);
lambda ~ gamma(5,0.2);
w      ~ gamma(4,0.8);


///not specifying freq- assuming uninformative uniform(0,1)

  
// likelihood ---- edit
for(j in 2:N_obs) {


probabilities_multi[j-1,1]=(pred_treated_I[row_model[j]]- pred_treated_I[row_model[j-1]])/pred_treated_uninf[row_model[j-1]];
probabilities_multi[j-1,2]=pred_treated_uninf[row_model[j]]/pred_treated_uninf[row_model[j-1]];

probabilities_multi_control[j-1,1]=(pred_control_I[row_model[j]]- pred_control_I[row_model[j-1]])/pred_control_uninf[row_model[j-1]];
probabilities_multi_control[j-1,2]=pred_control_uninf[row_model[j]]/pred_control_uninf[row_model[j-1]];


}

for (time_pt in 1:(N_obs-1)){

N_outcome[time_pt]~multinomial(to_vector(probabilities_multi[time_pt]));
N_outcome_control[time_pt]~multinomial(to_vector(probabilities_multi_control[time_pt]));

}


}




