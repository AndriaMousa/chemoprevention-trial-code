


### simulate_trial function will generate an R object in the form of a list where each item in the list is a simulated dataset.

simulate_trial <- function(N_sims= 1000,          ## number of simulated datasets to be generated
                           w=5,                   ## shape parameter
                           mean_protect= 20,      ## mean duration of protection against parasites
                           N0_treat=25,         ## sample size (enrolled and treated with chemoprevention)
                           followup=63,           ## specify total followup
                           ippy=2,               ## infection rate (infections per person per year. Includes asymptomatic infections
                           N0_control=25         ## number in control group. Will only be used if control=1

) {
  
  list_simulations <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)
  

 prob_inf<-1-exp(- (ippy/365)) ##daily probability of infection
 
 for (run in 1:N_sims) {   ## change to 1000
  #run<-1 
    
 lambda<- mean_protect/ (gamma(1+(1/w)))            ### calculate lambda (scale parameter) based on the mean
 stoptime<-followup+1     ## follow up is 63 but it starts at day 1 instead of 0.
    
 breaks <- c(0,2,3,5,7,14,21,28,35,42,49,56,63)
    

    #######################################################################################################
    ##set up dataframes to store number of new infections
    
    ind<-data.frame(id=1:N0_treat)
    runs_N_treated<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))

      ind_control<-data.frame(id=1:N0_control)
      runs_N_control<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=stoptime))
      
 
    
    
    
    
    ##########################################################################################################
    
    
    
    
    #initialising states
    ind$state<-"U"  ## Start with everyone being susceptible  to new infection
      ind_control$state<-"U"  ## Start with everyone being susceptible  to new infection
    
    
    ## set up matrix to store individuals' states over time. Rows=people, columns=days.
    
    out<-matrix(nrow=N0_treat,ncol=stoptime) # make a matrix that gets overwritten at each simulation
    out[,1]<-ind$state # we input the first column of all S (And then we will substitute)
    
    out_control<-matrix(nrow=N0_control,ncol=stoptime) # make a matrix that gets overwritten at each simulation
    out_control[,1]<-ind_control$state # we input the first column of all S (And then we will substitute)
    
    
    for(t in 2:stoptime) {
      
      
      
      ind$prob_unif<- NA
      is.U_last_t<-which(out[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      is.new_expo<-which(ind$prob_unif<prob_inf)                ### assign who gets exposed  with a parasite 
      p_protected<- exp(-((t-1)/lambda)^w)             #  probability of being protected at that time point
      ind$prob_unif<-NA 
      ind$prob_unif[is.new_expo]<- runif(length(is.new_expo)) 
      is.new_inf<-which(ind$prob_unif<(1-p_protected))     ### assign who gets infected with R 
      out[,t]<-out[,(t-1)]  ## first copy over previous day's states.
      out[is.new_inf,t]<-"I"   ## new infection 
      
    }
    
    
 for(t in 2:stoptime) {
      
      ind_control$prob_unif<- NA
      is.U_last_t<-which(out_control[,(t-1)]=="U") #which ones are susceptible in the last time period
      ind_control$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
      is.new_inf<-which(ind_control$prob_unif<prob_inf)                ### assign who gets exposed  with a parasite 
      out_control[,t]<-out_control[,(t-1)]  ## first copy over previous day's states.
      out_control[is.new_inf,t]<-"I"   ## new infection 
      
    }
    
    ##### the code below prepares the input dataframes for the stan model
    
    runs_N_treated[run,1:stoptime]<-colCounts(out,value='I')
    
    runs_N_control[run,1:stoptime]<-colCounts(out_control,value='I')
      

    
    input_df<-data.frame(T=breaks)
    
    
    input_df$N_treated_at_risk<-NA
    input_df$N_treated_I<-NA
    input_df$N_treated_uninf<-NA
    
    input_df$N_treated_at_risk[1]<-N0_treat
    input_df$N_treated_I[1]<-0
    input_df$N_treated_uninf[1]<-N0_treat
    

      input_df$N_control_at_risk<-NA
      input_df$N_control_I<-NA
      input_df$N_control_uninf<-NA
      input_df$N_control_at_risk[1]<-N0_control
      input_df$N_control_I[1]<-0
      input_df$N_control_uninf[1]<-N0_control
    
    
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
    
    
    
    list_simulations[[run]]<-input_df
  }
  
 
  
  

    saveRDS(list_simulations,
            file=paste0("additional supplementary materials/Cox PH method/input_df_sim_",
                        mean_protect,"days_",ippy,"ippy",
                        "_Ntreat",N0_treat,"_Ncontrol",N0_control,".RData"))
  
  
  
  output <- list_simulations
  return(output)
}

transmission<- c(1,2,4)
protection<- c(10,15,20,25,30)
N_per_arm<-c(25,50,75,100,125,150,175,200,225,250)


combos<-expand.grid(transmission=transmission, protection=protection, N_per_arm=N_per_arm)

for (i in 1:nrow(combos)) {

output<- simulate_trial(N_sims= 1000,          ## number of simulated datasets to be generated
                        w=5,                    ## shape parameter
                        mean_protect= combos$protection[i],       ## mean duration of protection 
                        N0_treat=combos$N_per_arm[i],          ## sample size (enrolled and treated with chemoprevention) 
                        followup=63,            ## specify total followup
                        ippy=combos$transmission[i],                ## infection rate (infections per person per year. Includes asymptomatic infections
                        N0_control=combos$N_per_arm[i])        ## number in control group. Will only be used if control=1
 print(i)      
}

