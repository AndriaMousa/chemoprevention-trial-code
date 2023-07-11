library(matrixStats)
library(gdata)
library(ggplot2)
library(cowplot)
library(rstan)
library(bayesplot)
library(plyr)


### simulate_trial function will generate an R object in the form of a list where each item in the list is a simulated dataset.
### Change input parameters

simulate_trial_1strain <- function(N_sims= 1000,          ## number of simulated datasets to be generated
                           w=5,                   ## shape parameter 
                           mean_protect= 20,      ## mean duration of protection against parasites
                           N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                           followup=63,           ## specify total followup in days
                           ippy_av=10,            ## infection rate (infections per person per year). Includes asymptomatic infections
                           prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                           ltf=0.10,              ## loss to followup
                           control=0,             ## presence of control group? 0=no,1=yes
                           AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                           N0_control0=200,       ## number in control group. Will only be used if control=1
                           seasonality="none"     ## options : 
                                                  ##"start" of season (increasing transmission), 
                                                  ## "end" of season (declining transmission), or 
                                                  ## "none" (constant incidence over the time of followup)
                           ) {
  
  list_simulations <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)
  
  df_seasonality <-data.frame(time=seq(from=0,to=followup,by=1))
  
  inf_inc_min<- ippy_av  - (0.5*ippy_av )
  inf_inc_max<- ippy_av  + (0.5*ippy_av ) 
  df_seasonality$inf_inc<-NA
  if (seasonality=="none") {
    
    df_seasonality$inf_inc[]<-ippy_av
  }
  
  if (seasonality=="start") {
    
    df_seasonality$inf_inc [1]<-inf_inc_min
    
    for (day in (1:63) ){
      row<-day+1
      df_seasonality$inf_inc[row]<-df_seasonality$inf_inc [day] + (inf_inc_max-inf_inc_min)/63
      
    }  
    
  }
  
  if (seasonality=="end") {              
    
    df_seasonality$inf_inc [1]<-inf_inc_max
    
    for (day in (1:63) ){
      row<-day+1
      df_seasonality$inf_inc [row]<-df_seasonality$inf_inc [day] - (inf_inc_max-inf_inc_min)/63
      
    }  
    
  }
  df_seasonality$prob_inf<-NA
  df_seasonality$prob_inf[]<-1-exp(- (df_seasonality$inf_inc[]/365)) ##daily probability of infection
  
  
  
  
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
      
    }
    
    

    
    lambda<- mean_protect/ (gamma(1+(1/w)))            ### calculate lambda (scale parameter) based on the mean
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
    ##set up dataframes to store number of new infections
    
    ind<-data.frame(id=1:N0_treat)
    runs_N_treated<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))

    
    if (control==1) {
      ind_control<-data.frame(id=1:N0_control)
      runs_N_control<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))

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
    

    
    df_seasonality$inf_inc [1]<-NA
    
    
    for(t in 2:stoptime) {
      

      
      ind$prob_unif<- NA
      is.U_last_t<-which(out[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      is.new_expo<-which(ind$prob_unif< df_seasonality$prob_inf[t])                ### assign who gets exposed  with a parasite 
      p_protected<- exp(-((t-1)/lambda)^w)             #  probability of being protected at that time point
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_expo]<- runif(length(is.new_expo)) 
      is.new_inf<-which(ind$prob_unif<(1-p_protected))     ### assign who gets infected with R 
      out[,t]<-out[,(t-1)]  ## first copy over previous day's states.
      out[is.new_inf,t]<-"I"   ## new infection 
      
    }
    
    
    if (control ==1) {for(t in 2:stoptime) {
      
      ind_control$prob_unif<- NA
      is.U_last_t<-which(out_control[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind_control$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      is.new_inf<-which(ind_control$prob_unif< df_seasonality$prob_inf[t])                ### assign who gets exposed  with a parasite 
      out_control[,t]<-out_control[,(t-1)]  ## first copy over previous day's states.
      out_control[is.new_inf,t]<-"I"   ## new infection 
      
    }
    }
    ##### the code below prepares the input dataframes for the stan model
    
    runs_N_treated[run,1:stoptime]<-colCounts(out,value='I')

    
    if (control==1) {
      runs_N_control[run,1:stoptime]<-colCounts(out_control,value='I')

    }
    
    input_df<-data.frame(T=breaks)
    
    
    input_df$N_treated_at_risk<-NA
    input_df$N_treated_I<-NA
    input_df$N_treated_uninf<-NA

    input_df$N_treated_at_risk[1]<-N0_treat
    input_df$N_treated_I[1]<-0
    input_df$N_treated_uninf[1]<-N0_treat
    
    if (control==1) {
      
      input_df$N_control_at_risk<-NA
      input_df$N_control_I<-NA
      input_df$N_control_uninf<-NA
      input_df$N_control_at_risk[1]<-N0_control
      input_df$N_control_I[1]<-0
      input_df$N_control_uninf[1]<-N0_control
    }
    
    for (i in 2:length(breaks)) {
      
      j<-1+breaks[i]
      
      
      input_df$N_treated_at_risk[i]<-input_df$N_treated_uninf[i-1]
      input_df$N_treated_I[i]<-colCounts(out,value='I')[j]
      input_df$N_treated_uninf[i]<-colCounts(out,value='U')[j]
    }
    
    input_df$N_treated_I_new[1]<-0

    
    for (i in 2:nrow(input_df)) {
      input_df$N_treated_I_new[i]<-input_df$N_treated_I[i]- input_df$N_treated_I[i-1]

      
    } 
    
    
    
    
    
    if (control==1) {
      
      for (i in 2:length(breaks)) {
        
        j<-1+breaks[i]
        
        
        input_df$N_control_at_risk[i]<-input_df$N_control_uninf[i-1]
        input_df$N_control_I[i]<-colCounts(out_control,value='I')[j]
        input_df$N_control_uninf[i]<-colCounts(out_control,value='U')[j]
      }
      
      input_df$N_control_I_new[1]<-0

      
      for (i in 2:nrow(input_df)) {
        input_df$N_control_I_new[i]<-input_df$N_control_I[i]- input_df$N_control_I[i-1]
        
      } 
    }
    
    
    list_simulations[[run]]<-input_df
  }
  
  if (control==0) {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_",
                        mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                        "_N",N0_treat0,"_",followup,"d_ltf",ltf,".RData"))
  }


  if (control==1 & AS_or_placebo=="AS") {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_",
                        mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                        "_N",N0_treat0,"_",N0_control0, "AS_",followup,"d_ltf",ltf,".RData"))
  }

  if (control==1 & AS_or_placebo=="placebo") {

    saveRDS(list_simulations,
            file=paste0("input_df_sim_", 
                        mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                        "_N",N0_treat0,"_",N0_control0, "plac_",followup,"d_ltf",ltf,".RData"))
  }

  output <- list_simulations
  return(output)
}



