library(matrixStats)
library(gdata)
library(ggplot2)
library(cowplot)
library(rstan)
library(bayesplot)
library(plyr)


### simulate_trial function will generate an R object in the form of a list where each item in the list is a simulated dataset.
### Change input parameters

simulate_trial_2strain <- function(N_sims= 1000,          ## number of simulated datasets to be generated
                           w_S=5,                 ## shape parameter (Sensitive)
                           w_R=5,                 ## shape parameter (Resistant)
                           mean_protect_S= 30,    ## mean duration of protection against S
                           mean_protect_R=18,     ## mean duration of protection against R
                           N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                           followup=63,           ## specify total followup
                           ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                           prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                           freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                           prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                           ltf=0.10,              ## loss to followup
                           control=0,             ## presence of control group? 0=no,1=yes
                           AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                           N0_control0=200        ## number in control group. Will only be used if control=1
                           
                           ) {
  
  list_simulations <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)
  
  inf_inc<-ippy/365 ## ippy converted to ippd
    
  # if (control==1) {
  #   mean_protect_control_S<-0
  #   mean_protect_control_R<-0
  #   lambda_control_S<- mean_protect_control_S/ (gamma(1+(1/w_S)))            ### calculate lambda (scale parameter) based on the mean
  #   lambda_control_R<- mean_protect_control_R/ (gamma(1+(1/w_R)))
  # }
  # 
  
  for (run in 1:N_sims) {   ## change to 1000
    
    ###### including stochasticity when sampling negatives on day0 and those not lost to follow-up 
    ind0<-data.frame(id=1:N0_treat0)
    ind0$prob_unif<-NA
    ind0$prob_unif<- runif(N0_treat0)
    is.slide.neg<- which(ind0$prob_unif<(1-prevalence))
    ind0$prob_unif<-NA
    ind0$prob_unif[is.slide.neg]<-runif(length(is.slide.neg))
    is.slide.neg.not.LTF<-which(ind0$prob_unif<(1-ltf))
    N0_treat<-length(is.slide.neg.not.LTF)
    
    
    if (AS_or_placebo=="AS") {
      
      ind0$prob_unif<-NA
      ind0<-data.frame(id=1:N0_control0)
      ind0$prob_unif<- runif(N0_control0)
      is.slide.neg<- which(ind0$prob_unif<1)   ### all of them are cleared on day 0
      ind0$prob_unif<-NA
      ind0$prob_unif[is.slide.neg]<-runif(length(is.slide.neg))
      is.slide.neg.not.LTF<-which(ind0$prob_unif<(1-ltf))
      N0_control<-length(is.slide.neg.not.LTF)
      
      ##N0_control<-round(N0_control0 * (1-ltf)) ### assumes artesunate clears all infections (before enrollment), hence would be able to use all 200 on day 0
    }
    
    if (AS_or_placebo=="placebo") {
      ind0<-data.frame(id=1:N0_control0)
      ind0$prob_unif<-NA
      ind0$prob_unif<- runif(N0_control0)
      is.slide.neg<- which(ind0$prob_unif<(1-prevalence))   ### all of them are cleared on day 0
      ind0$prob_unif<-NA
      ind0$prob_unif[is.slide.neg]<-runif(length(is.slide.neg))
      is.slide.neg.not.LTF<-which(ind0$prob_unif<(1-ltf))
      N0_control<-length(is.slide.neg.not.LTF)
      
      ## N0_control<-round(N0_control0 * (1-ltf) * (1-prevalence))       ###untreated control or placebo
    }
    
    
    
    prob_inf<-1-exp(-inf_inc) ##daily probability of infection
    lambda_S<- mean_protect_S/ (gamma(1+(1/w_S)))            ### calculate lambda (scale parameter) based on the mean
    lambda_R<- mean_protect_R/ (gamma(1+(1/w_R)))
    stoptime<-followup+1     ## follow up is 63 but it starts at day 1 instead of 0.
    
    if (followup==63){
      breaks <- c(0,2,3,5,7,14,21,28,35,42,49,56,63)
    }
    
    if (followup==42){
      breaks <- c(0,2,3,5,7,14,21,28,35,42)
    }
    
    if (followup==28){
      breaks <- c(0,2,3,5,7,14,21,28)
    }
    #######################################################################################################
    ##set up dataframes to store number reinfected with resistant and sensitive
    
    ind<-data.frame(id=1:N0_treat)
    runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
    runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
    runs_N_treated_I_undeterm<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
    
    if (control==1) {
      ind_control<-data.frame(id=1:N0_control)
      runs_N_control_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
      runs_N_control_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
      runs_N_control_I_undeterm<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
    }
    
    
    
    
    ##########################################################################################################
    
    
    
    
    #initialising states
    ind$state<-"U"  ## Start with everyone being susceptible  to new infection
    if (control==1){
      ind_control$state<-"U"  ## Start with everyone being susceptible  to new infection
    }
    
    ## set up matrix to store individuals' states over time. Rows=people, columns=days.
    
    out<-matrix(nrow=N0_treat,ncol=stoptime) # make a matrix that gets overwritten at each simulation
    out[,1]<-ind$state # we input the first column of all S (And then we will substitute)
    
    if (control==1){
      out_control<-matrix(nrow=N0_control,ncol=stoptime) # make a matrix that gets overwritten at each simulation
      out_control[,1]<-ind_control$state # we input the first column of all S (And then we will substitute)
    }
    
    
    for(t in 2:stoptime) {
      
      ind$prob_unif<- NA
      
      is.U_last_t<-which(out[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      
      is.new_expo<-which(ind$prob_unif< prob_inf)                ### assign who gets exposed  with a parasite 
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_expo]<- runif(length(is.new_expo))
      is.new_expo_R<-which(ind$prob_unif<freq_R)                  # assign who gets gets exposed to R
      is.new_expo_S<-is.new_expo[!is.new_expo%in%is.new_expo_R]  #
      
      p_protected_R<- exp(-((t-1)/lambda_R)^w_R)             #  probability of being protected against R at that time point
      p_protected_S<- exp(-((t-1)/lambda_S)^w_S)
      
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_expo_R]<- runif(length(is.new_expo_R)) 
      is.new_inf_R<-which(ind$prob_unif<(1-p_protected_R))     ### assign who gets infected with R 
      
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_expo_S]<- runif(length(is.new_expo_S))
      is.new_inf_S<-which(ind$prob_unif<(1-p_protected_S))       ### assign who gets infected with S
      
      
      is.new_inf<- c(is.new_inf_R, is.new_inf_S)
      
      out[,t]<-out[,(t-1)]  ## first copy over previous day's states.
      
      out[is.new_inf_R,t]<-"I_R"   ## new infection (Resistant)
      out[is.new_inf_S,t]<-"I_S"   ## new infection (Sensitive)
      
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_inf_R]<- runif(length(is.new_inf_R))     #### uniform probability
      is.new_inf_R_determ<- which(ind$prob_unif< prob_determ)
      
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_inf_S]<- runif(length(is.new_inf_S))     #### uniform probability
      is.new_inf_S_determ<- which(ind$prob_unif< prob_determ)
      
      is.new_inf_determ<-c(is.new_inf_R_determ,is.new_inf_S_determ)
      is.new_inf_undeterm<- is.new_inf[!(is.new_inf %in% is.new_inf_determ)]
      
      out[is.new_inf_undeterm,t]<-"I_undeterm"   
      
    }
    
    if (control ==1) {for(t in 2:stoptime) {
      
      ind_control$prob_unif<- NA
      
      is.U_last_t<-which(out_control[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind_control$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      
      is.new_expo<-which(ind_control$prob_unif< prob_inf)                ### assign who gets exposed  with a parasite 
      ind_control$prob_unif<-NA 
      ind_control$prob_unif[is.new_expo]<- runif(length(is.new_expo))
      is.new_inf_R<-which(ind_control$prob_unif<freq_R)                  # assign who gets gets exposed to R all exposures lead to infections (ie no protection in the control grp)
      is.new_inf_S<-is.new_expo[!is.new_expo%in%is.new_inf_R]  #
      
      is.new_inf<- c(is.new_inf_R, is.new_inf_S)
      
      out_control[,t]<-out_control[,(t-1)]  ## first copy over previous day's states.
      
      out_control[is.new_inf_R,t]<-"I_R"   ## new infection (Resistant)
      out_control[is.new_inf_S,t]<-"I_S"   ## new infection (Sensitive)
      
      ind_control$prob_unif<-NA 
      ind_control$prob_unif[is.new_inf_R]<- runif(length(is.new_inf_R))     #### uniform probability
      is.new_inf_R_determ<- which(ind_control$prob_unif< prob_determ)
      
      ind_control$prob_unif<-NA 
      ind_control$prob_unif[is.new_inf_S]<- runif(length(is.new_inf_S))     #### uniform probability
      is.new_inf_S_determ<- which(ind_control$prob_unif< prob_determ)
      
      is.new_inf_determ<-c(is.new_inf_R_determ,is.new_inf_S_determ)
      is.new_inf_undeterm<- is.new_inf[!(is.new_inf %in% is.new_inf_determ)]
      
      out_control[is.new_inf_undeterm,t]<-"I_undeterm"   
      
    }
    }
    ##### the code below prepares the input dataframes for the stan model
    
    runs_N_treated_R[run,1:stoptime]<-colCounts(out,value='I_R')
    runs_N_treated_S[run,1:stoptime]<-colCounts(out,value='I_S')
    runs_N_treated_I_undeterm[run,1:stoptime]<-colCounts(out,value='I_undeterm')
    
    if (control==1) {
      runs_N_control_R[run,1:stoptime]<-colCounts(out_control,value='I_R')
      runs_N_control_S[run,1:stoptime]<-colCounts(out_control,value='I_S')
      runs_N_control_I_undeterm[run,1:stoptime]<-colCounts(out_control,value='I_undeterm')
    }
    
    input_df<-data.frame(T=breaks)
    
    
    input_df$N_treated_at_risk<-NA
    input_df$N_treated_I_S<-NA
    input_df$N_treated_I_R<-NA
    input_df$N_treated_uninf<-NA
    input_df$N_treated_I_undeterm<-NA
    
    input_df$N_treated_at_risk[1]<-N0_treat
    input_df$N_treated_I_S[1]<-0
    input_df$N_treated_I_R[1]<-0
    input_df$N_treated_I_undeterm<-0
    input_df$N_treated_uninf[1]<-N0_treat
    
    if (control==1) {
      
      input_df$N_control_at_risk<-NA
      input_df$N_control_I_S<-NA
      input_df$N_control_I_R<-NA
      input_df$N_control_uninf<-NA
      input_df$N_control_I_undeterm<-NA
      input_df$N_control_at_risk[1]<-N0_control
      input_df$N_control_I_S[1]<-0
      input_df$N_control_I_R[1]<-0
      input_df$N_control_I_undeterm<-0
      input_df$N_control_uninf[1]<-N0_control
    }
    
    for (i in 2:length(breaks)) {
      
      j<-1+breaks[i]
      
      
      input_df$N_treated_at_risk[i]<-input_df$N_treated_uninf[i-1]
      input_df$N_treated_I_S[i]<-colCounts(out,value='I_S')[j]
      input_df$N_treated_I_R[i]<-colCounts(out,value='I_R')[j]
      input_df$N_treated_I_undeterm[i]<-colCounts(out,value='I_undeterm')[j]
      input_df$N_treated_uninf[i]<-colCounts(out,value='U')[j]
    }
    
    input_df$N_treated_I_S_new[1]<-0
    input_df$N_treated_I_R_new[1]<-0
    input_df$N_treated_I_undeterm_new[1]<-0
    
    
    for (i in 2:nrow(input_df)) {
      input_df$N_treated_I_S_new[i]<-input_df$N_treated_I_S[i]- input_df$N_treated_I_S[i-1]
      input_df$N_treated_I_R_new[i]<-input_df$N_treated_I_R[i]- input_df$N_treated_I_R[i-1]
      input_df$N_treated_I_undeterm_new[i]<-input_df$N_treated_I_undeterm[i]- input_df$N_treated_I_undeterm[i-1]
      
      
    } 
    
    
    
    
    
    if (control==1) {
      
      for (i in 2:length(breaks)) {
        
        j<-1+breaks[i]
        
        
        input_df$N_control_at_risk[i]<-input_df$N_control_uninf[i-1]
        input_df$N_control_I_S[i]<-colCounts(out_control,value='I_S')[j]
        input_df$N_control_I_R[i]<-colCounts(out_control,value='I_R')[j]
        input_df$N_control_I_undeterm[i]<-colCounts(out_control,value='I_undeterm')[j]
        input_df$N_control_uninf[i]<-colCounts(out_control,value='U')[j]
      }
      
      input_df$N_control_I_S_new[1]<-0
      input_df$N_control_I_R_new[1]<-0
      input_df$N_control_I_undeterm_new[1]<-0
      
      
      for (i in 2:nrow(input_df)) {
        input_df$N_control_I_S_new[i]<-input_df$N_control_I_S[i]- input_df$N_control_I_S[i-1]
        input_df$N_control_I_R_new[i]<-input_df$N_control_I_R[i]- input_df$N_control_I_R[i-1]
        input_df$N_control_I_undeterm_new[i]<-input_df$N_control_I_undeterm[i]- input_df$N_control_I_undeterm[i-1]
        
        
      } 
    }
    
    
    list_simulations[[run]]<-input_df
  }
  
  if (control==0) {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,".RData"))
  }


  if (control==1 & AS_or_placebo=="AS") {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "AS_",followup,"d_ltf",ltf,".RData"))
  }

  if (control==1 & AS_or_placebo=="placebo") {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "plac_",followup,"d_ltf",ltf,".RData"))
  }
  # 
  
  output <- list_simulations
  return(output)
}


