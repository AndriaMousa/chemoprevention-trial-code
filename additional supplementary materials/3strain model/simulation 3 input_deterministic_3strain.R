
N_sims=1000          # number of simulations

list_simulations <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)


# simulation


########################################################################################## PARAMETERS

N0_treat0<-500 ##number enrolled and treated
prevalence<-0.378 ##(Nchelenge) slide prevalence
N0_treat<-round(N0_treat0 * 0.9 * (1-prevalence)) ## number that will actually be analysed (negative at day 0 and not lost to follow-up)

stoptime<-64 ## follow up is 63 but it starts at day 1 instead of 0.
inf_inc<- 10/365 ## infection rate (infections per person per year) 
prob_inf<-1-exp(-inf_inc) ##daily probability of infection

prob_determ<-0.85 ##0.788 but we think it can be improved

mean_protect_S<-30  
mean_protect_R1<-18    
mean_protect_R2<-10    

w<-5  ##shape parameter 

lambda_S<- mean_protect_S/ (gamma(1+(1/w)))            ### calculate lambda (scale parameter) based on the mean
lambda_R1<- mean_protect_R1/ (gamma(1+(1/w)))
lambda_R2<- mean_protect_R2/ (gamma(1+(1/w)))


freq_R1<- 0.70 
freq_R2<- 0.06 

breaks <- c(0,2,3,5,7,14,21,28,35,42,49,56,63)


ind<-data.frame(id=1:N0_treat)
ind$treated<-1   ## treated groups

##set up dataframes to store number reinfected with resistant and sensitive

runs_N_treated_R1<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
runs_N_treated_R2<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
runs_N_treated_I_undeterm<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))

##########################################################################################################


for (run in 1:N_sims) {   ## change to 1000
  
  #initialising states
  ind$state<-"S"  ## Start with everyone being susceptible  to new infection
  
  ## set up matrix to store individuals' states over time. Rows=people, columns=days.
  
  out<-matrix(nrow=N0_treat,ncol=stoptime) # make a matrix that gets overwritten at each simulation
  out[,1]<-ind$state # we input the first column of all S (And then we will substitute)
  
  for(t in 2:stoptime) {
    
    ind$prob_unif<- NA
    
    is.S_last_t<-which(out[,(t-1)]=="S") #which ones are susceptible in the last time period
    ind$prob_unif[is.S_last_t]<- runif(length(is.S_last_t))     #### uniform probability
    
    is.new_expo<-which(ind$prob_unif< prob_inf)                ### assign who gets exposed  with a parasite 
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_expo]<- runif(length(is.new_expo))
    is.new_expo_R<-which(ind$prob_unif<(freq_R1+freq_R2))                  # assign who gets gets exposed to R
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_expo_R]<- runif(length(is.new_expo_R))
    is.new_expo_R1<-which(ind$prob_unif<freq_R1)                  # assign who gets gets exposed to R1
    is.new_expo_R2<-is.new_expo_R[!is.new_expo_R%in%is.new_expo_R1]  # # assign who gets gets exposed to R2
    
    is.new_expo_S<-is.new_expo[!is.new_expo%in%is.new_expo_R]  #
    
    p_protected_R1<- exp(-((t-1)/lambda_R1)^w)             #  probability of being protected against R1 at that time point
    p_protected_R2<- exp(-((t-1)/lambda_R2)^w)             #  probability of being protected against R2 at that time point
    p_protected_S<- exp(-((t-1)/lambda_S)^w)
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_expo_R1]<- runif(length(is.new_expo_R1)) 
    is.new_inf_R1<-which(ind$prob_unif<(1-p_protected_R1))     ### assign who gets infected with R1 
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_expo_R2]<- runif(length(is.new_expo_R2)) 
    is.new_inf_R2<-which(ind$prob_unif<(1-p_protected_R2))     ### assign who gets infected with R2  
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_expo_S]<- runif(length(is.new_expo_S))
    is.new_inf_S<-which(ind$prob_unif<(1-p_protected_S))       ### assign who gets infected with S
    
    
    is.new_inf<- c(is.new_inf_R1,is.new_inf_R2, is.new_inf_S)
    
    out[,t]<-out[,(t-1)]  ## first copy over previous day's states.
    
    out[is.new_inf_R1,t]<-"I_R1"   ## new infection (Resistant)
    out[is.new_inf_R2,t]<-"I_R2"   ## new infection (Resistant)
    out[is.new_inf_S,t]<-"I_S"   ## new infection (Sensitive)
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_inf_R1]<- runif(length(is.new_inf_R1))     #### uniform probability
    is.new_inf_R1_determ<- which(ind$prob_unif< prob_determ)
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_inf_R2]<- runif(length(is.new_inf_R2))     #### uniform probability
    is.new_inf_R2_determ<- which(ind$prob_unif< prob_determ)
    
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_inf_S]<- runif(length(is.new_inf_S))     #### uniform probability
    is.new_inf_S_determ<- which(ind$prob_unif< prob_determ)
    
    is.new_inf_determ<-c(is.new_inf_R1_determ,is.new_inf_R2_determ, is.new_inf_S_determ)
    is.new_inf_undeterm<- is.new_inf[!(is.new_inf %in% is.new_inf_determ)]
    
    out[is.new_inf_undeterm,t]<-"I_undeterm"   

  }
   
  ##### the code below prepares the input dataframes for the stan model
  
  runs_N_treated_R1[run,1:stoptime]<-colCounts(out,value='I_R1')
  runs_N_treated_R2[run,1:stoptime]<-colCounts(out,value='I_R2')
  runs_N_treated_S[run,1:stoptime]<-colCounts(out,value='I_S')
  runs_N_treated_I_undeterm[run,1:stoptime]<-colCounts(out,value='I_undeterm')
  
  
  input_df<-data.frame(T=breaks)
  
  
  input_df$N_treated_at_risk<-NA
  input_df$N_treated_I_S<-NA
  input_df$N_treated_I_R1<-NA
  input_df$N_treated_I_R2<-NA
  input_df$N_treated_uninf<-NA
  input_df$N_treated_I_undeterm<-NA
  
  input_df$N_treated_at_risk[1]<-N0_treat
  input_df$N_treated_I_S[1]<-0
  input_df$N_treated_I_R1[1]<-0
  input_df$N_treated_I_R2[1]<-0
  input_df$N_treated_I_undeterm<-0
  input_df$N_treated_uninf[1]<-N0_treat
  
  for (i in 2:length(breaks)) {
    
    j<-1+breaks[i]
    
    
    input_df$N_treated_at_risk[i]<-input_df$N_treated_uninf[i-1]
    input_df$N_treated_I_S[i]<-colCounts(out,value='I_S')[j]
    input_df$N_treated_I_R1[i]<-colCounts(out,value='I_R1')[j]
    input_df$N_treated_I_R2[i]<-colCounts(out,value='I_R2')[j]
    input_df$N_treated_I_undeterm[i]<-colCounts(out,value='I_undeterm')[j]
    input_df$N_treated_uninf[i]<-colCounts(out,value='S')[j]
  }
  
  input_df$N_treated_I_S_new[1]<-0
  input_df$N_treated_I_R1_new[1]<-0
  input_df$N_treated_I_R2_new[1]<-0
  input_df$N_treated_I_undeterm_new[1]<-0
  
  
  for (i in 2:nrow(input_df)) {
    input_df$N_treated_I_S_new[i]<-input_df$N_treated_I_S[i]- input_df$N_treated_I_S[i-1]
    input_df$N_treated_I_R1_new[i]<-input_df$N_treated_I_R1[i]- input_df$N_treated_I_R1[i-1]
    input_df$N_treated_I_R2_new[i]<-input_df$N_treated_I_R2[i]- input_df$N_treated_I_R2[i-1]
    input_df$N_treated_I_undeterm_new[i]<-input_df$N_treated_I_undeterm[i]- input_df$N_treated_I_undeterm[i-1]
    
    
  } 
  
  list_simulations[[run]]<-input_df
}

saveRDS(list_simulations, 
        file="input_df_sim_3str_freq0.7_0.06_R18_10_days_10ippy_determ_0.85_500.RData")

