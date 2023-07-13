

N_sims=1000          # number of simulations

list_simulations <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)


# simulation


########################################################################################## PARAMETERS

N0_treat<-2000 ##number enrolled and treated (lets assume no LTF, and that we analyse everyone on day 0 for simplicity)

stoptime<-64 ## follow up is 63 but it starts at day 1 instead of 0.
inf_inc<- 10/365 ## infection rate (infections per person per year) 
prob_inf<-1-exp(-inf_inc) ##daily probability of infection



prob0<-dpois(0,inf_inc) ## probability of having no infections per day
prob1<-dpois(1,inf_inc) ## probability of having 1 infection per day

##the probability of having 2 or more infections on the same day
prob2plus<-1 - dpois(0,inf_inc) - dpois(1,inf_inc) 

##note that prob1+prob2plus==prob_inf

mean_protect_S<-30  
mean_protect_R<-18    

w<-5  ##shape parameter 

lambda_S<- mean_protect_S/ (gamma(1+(1/w)))            ### calculate lambda (scale parameter) based on the mean
lambda_R<- mean_protect_R/ (gamma(1+(1/w)))

freq_R<- 0.50 #### prevalence of resistant genotypes  

#breaks <- c(0,2,3,5,7,14,21,28,35,42,49,56,63) ## frequent followup
breaks <- c(0,7,14,28,42,63) # infrequent followup


ind<-data.frame(id=1:N0_treat)
ind$treated<-1   ## treated groups

prob_R_only<-   freq_R^2
prob_S_only<-  (1-freq_R)^2
prob_both<-     freq_R * (1-freq_R)
prob_none<-    (1-freq_R) * freq_R

##check above are equal to 1
prob_R_only+prob_S_only+prob_both+prob_none

##need to normalise proportions to remove from the denominator the probability of the infection being neither S or R (not possible by the current system)

prob_both_norm<-  prob_both/ (1-prob_none)
prob_S_only_norm<-  prob_S_only/ (1-prob_none)
prob_R_only_norm<-  prob_R_only/ (1-prob_none)

##########################################################################################################

#run<-1
for (run in 1:N_sims) {   ## change to 1000
  print(run)
  #initialising states
  ind$state<-"U"  ## Start with everyone being susceptible  to new infection (uninfected)
  
  ## set up matrix to store individuals' states over time. Rows=people, columns=days.
  
  out<-matrix(nrow=N0_treat,ncol=stoptime) # make a matrix that gets overwritten at each simulation
  out[,1]<-ind$state # we input the first column of all U (And then we will substitute)
  
  for(t in 2:stoptime) {
    
    
    is.U_last_t<-which(out[,(t-1)]=="U") #which ones are susceptible in the last time period
    is.I_S_last_t<-which(out[,(t-1)]=="I_S")
    is.I_R_last_t<-which(out[,(t-1)]=="I_R")
    
    ind$prob_unif<- NA
    ind$prob_unif[is.U_last_t]<- runif(length(is.U_last_t))     #### uniform probability
    is.new_expo<-which(ind$prob_unif< prob_inf)                ### assign who gets exposed  with at least 1 parasite on that day
    
    ind$prob_unif<- NA
    ind$prob_unif[is.new_expo]<- runif(length(is.new_expo))     #### uniform probability
    is.new_1expo<-which(ind$prob_unif< (prob1/prob_inf))  ##assign who gets 1 infection on a single day (based on probability of 1 infection divided by the probability of any infection).  ##note that prob1+prob2+prob3plus==prob_inf
    is.new_2expo<-is.new_expo[!is.new_expo%in%is.new_1expo]
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_1expo]<- runif(length(is.new_1expo))     #### uniform probability
    is.new_1expo_1R<-which(ind$prob_unif<freq_R)    ##single infections are straight forward 
    is.new_1expo_1S<-is.new_1expo[!is.new_1expo%in%is.new_1expo_1R] 
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_2expo]<- runif(length(is.new_2expo))     #### uniform probability
    is.new_2expo_2R<-which(ind$prob_unif<prob_R_only_norm)  ## these get exposed to 2 resistant infections on the same day
    
    
    is.new_2expo_min1S<-is.new_2expo[!is.new_2expo%in%is.new_2expo_2R] ## those who get exposed to 2 infections, where at least 1 of them is S.
    
    ind$prob_unif<-NA 
    
    ind$prob_unif[is.new_2expo_min1S]<- runif(length(is.new_2expo_min1S))
    is.new_2expo_2S<-which(ind$prob_unif<(prob_S_only_norm/(prob_S_only_norm+prob_both_norm))) # assign who gets gets exposed to two S infections on the same day
    
    is.new_2expo_1R1S<- is.new_2expo_min1S[! is.new_2expo_min1S%in%is.new_2expo_2S]  # the remaining are exposed to both on the same day
    
    
    ###### I_R exposed to S
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.I_R_last_t]<- runif(length(is.I_R_last_t))     #### uniform probability
    
    is.new_R_expo<-which(ind$prob_unif< prob_inf)                ### assign who gets exposed  with a parasite 
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_R_expo]<- runif(length(is.new_R_expo))
    is.new_R_expo_S<-which(ind$prob_unif<(1-freq_R))                  # assign which I_R gets gets exposed to S
    
    ###### I_S exposed to R
    ind$prob_unif[is.I_S_last_t]<- runif(length(is.I_S_last_t))     #### uniform probability
    
    is.new_S_expo<-which(ind$prob_unif< prob_inf)                ### assign who gets exposed  with a parasite 
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_S_expo]<- runif(length(is.new_S_expo))
    is.new_S_expo_R<-which(ind$prob_unif<freq_R)                  # assign which I_S gets gets exposed to R
    
    
    #######probability of protection    
    p_protected_R<- exp(-((t-1)/lambda_R)^w)             #  probability of being protected against R at that time point
    p_protected_S<- exp(-((t-1)/lambda_S)^w)
    
    #### U infected with R or S  (single infection in a day)      
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_1expo_1R]<- runif(length(is.new_1expo_1R)) 
    is.new_inf1R_1expo1R<-which(ind$prob_unif<(1-p_protected_R))     ### assign who gets infected with R 
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_1expo_1S]<- runif(length(is.new_1expo_1S)) 
    is.new_inf1S_1expo1S<-which(ind$prob_unif<(1-p_protected_S))     ### assign who gets infected with S
    
    
    ##2 EXPOSURES
    
    #### U infected with multiple infections on the same day    
    
    ### 2 R EXPOSURES
    ind$prob_unif<-NA 
    ind$prob_unif[ is.new_2expo_2R]<- runif(length( is.new_2expo_2R)) 
    is.new_inf2R_2expo2R<-which(ind$prob_unif<((1-p_protected_R)*(1-p_protected_R)))     ### 
    is.2expo2R_notinf2R<-is.new_2expo_2R[!is.new_2expo_2R%in%is.new_inf2R_2expo2R]
    
    ##to do : out of those exposed to 2R who are do not get 2R inf, who gets 1? DONE
    ## maybe sample out of those who is not above, and divide below prob with 1-above prob?
    ind$prob_unif<-NA 
    ind$prob_unif[is.2expo2R_notinf2R]<- runif(length(is.2expo2R_notinf2R)) 
    is.new_inf1R_2expo2R<-which(ind$prob_unif<((2*(1-p_protected_R)*(p_protected_R))/(1-((1-p_protected_R)*(1-p_protected_R)))))  ## times two because there are 2 parasites (can either  get the first or the second)
    
    
    ###2 S EXPOSURES
    ind$prob_unif<-NA 
    ind$prob_unif[ is.new_2expo_2S]<- runif(length( is.new_2expo_2S)) 
    is.new_inf2S_2expo2S<-which(ind$prob_unif<((1-p_protected_S)*(1-p_protected_S)))     ### 
    
    is.2expo2S_notinf2S<-is.new_2expo_2S[!is.new_2expo_2S%in%is.new_inf2S_2expo2S]
    ind$prob_unif<-NA 
    ind$prob_unif[is.2expo2S_notinf2S]<- runif(length(is.2expo2S_notinf2S)) 
    is.new_inf1S_2expo2S<-which(ind$prob_unif<((2*(1-p_protected_S)*(p_protected_S))/(1-((1-p_protected_S)*(1-p_protected_S)))))  ## times two because there are 2 parasites (can either  get the first or the second)
    
    
    ### 1S and 1R EXPOSURES
    ind$prob_unif<-NA 
    ind$prob_unif[ is.new_2expo_1R1S]<- runif(length( is.new_2expo_1R1S)) 
    is.new_inf1R1S_2expo1R1S<-which(ind$prob_unif<((1-p_protected_S)*(1-p_protected_R)))     ### not protected against neither leading to mixed
    is.2expo1R1S_notinf1R1S<-is.new_2expo_1R1S[!is.new_2expo_1R1S%in%is.new_inf1R1S_2expo1R1S] ###protected against at least one
    
    ind$prob_unif<-NA 
    ind$prob_unif[is.2expo1R1S_notinf1R1S]<- runif(length(is.2expo1R1S_notinf1R1S)) 
    is.new_inf1S_2expo1S1R<-which(ind$prob_unif<(((1-p_protected_S)*(p_protected_R))/ (1-((1-p_protected_S)*(1-p_protected_R)))))     ### protected against R only
    is.2expo1S1R_notinf1R1S_notinf1S<-is.2expo1R1S_notinf1R1S[!is.2expo1R1S_notinf1R1S%in% is.new_inf1S_2expo1S1R]
    
    ind$prob_unif<-NA 
    ind$prob_unif[ is.new_inf1S_2expo1S1R]<- runif(length( is.new_inf1S_2expo1S1R)) 
    
    is.new_inf1R_2expo1S1R<-which(ind$prob_unif<(((p_protected_S)*(1-p_protected_R))/(1-(((1-p_protected_S)*(p_protected_R))/ (1-((1-p_protected_S)*(1-p_protected_R)))))))     ### protected against S only
    
    
    #### I_R infected with S  
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_R_expo_S]<- runif(length(is.new_R_expo_S))
    is.new_R_inf_S<-which(ind$prob_unif<(1-p_protected_S))       ### assign who gets infected with S    
    
    #### I_S infected with R    
    ind$prob_unif<-NA 
    ind$prob_unif[is.new_S_expo_R]<- runif(length(is.new_S_expo_R))
    is.new_S_inf_R<-which(ind$prob_unif<(1-p_protected_R))       ### assign who gets infected with R   
    
    
    
    
    out[,t]<-out[,(t-1)]  ## first copy over previous day's states.
    
    out[is.new_inf1R_1expo1R,t]<-"I_R"   ## new infection (Resistant)
    out[is.new_inf2R_2expo2R,t]<-"I_R"   ## new infection (Resistant)
    out[is.new_inf1R_2expo2R,t]<-"I_R"   ## new infection (Resistant) ## exposed to two 
    out[is.new_inf1R_2expo1S1R,t]<-"I_R"   ## new infection (Resistant) ## exposed to two 
    
    out[is.new_inf1S_1expo1S,t]<-"I_S"   ## new infection (Sensitive)
    out[is.new_inf2S_2expo2S,t]<-"I_S"   ## new infection (Sensitive)
    out[is.new_inf1S_2expo2S,t]<-"I_S"   ## new infection (Sensitive)
    out[is.new_inf1S_2expo1S1R,t]<-"I_S"   ## new infection (Sensitive)
    
    out[is.new_inf1R1S_2expo1R1S,t]<-"I_mixed"  ## mixed new infection (from susceptible)
    out[is.new_R_inf_S,t]<-"I_mixed"   ## mixed new infection (from I_R)
    out[is.new_S_inf_R,t]<-"I_mixed"   ## mixed new infection (from I_S)
    
    
  }
  
  ##### the code below prepares the input dataframes for the stan model
  
  out_observed<- out[,(breaks+1)]
  
  for(t in 2:ncol(out_observed)) {
    # print(t)
    out_observed[which(out_observed[,t]=="I_mixed" & out_observed[,t-1]=="I_R"),t]<-"I_R"  
    out_observed[which(out_observed[,t]=="I_mixed" & out_observed[,t-1]=="I_S"),t]<-"I_S"
  }
  
  input_df<-data.frame(T=breaks)
  
  
  input_df$N_treated_I_S<-NA
  input_df$N_treated_I_R<-NA
  input_df$N_treated_I_mixed<-NA
  input_df$N_treated_uninf<-NA
  
  input_df$N_treated_I_S[1]<-0
  input_df$N_treated_I_R[1]<-0
  input_df$N_treated_I_mixed[1]<-0
  input_df$N_treated_uninf[1]<-N0_treat
  
  for (i in 2:ncol(out_observed)) {
    input_df$N_treated_I_S[i]<-colCounts(out_observed,value='I_S')[i]
    input_df$N_treated_I_R[i]<-colCounts(out_observed,value='I_R')[i]
    input_df$N_treated_I_mixed[i]<-colCounts(out_observed,value='I_mixed')[i]
    input_df$N_treated_uninf[i]<-colCounts(out_observed,value='U')[i]
  }
  
  input_df$N_treated_I_S_new[1]<-0
  input_df$N_treated_I_R_new[1]<-0
  input_df$N_treated_I_mixed_new[1]<-0
  
  
  for (i in 2:nrow(input_df)) {
    input_df$N_treated_I_S_new[i]<-input_df$N_treated_I_S[i]- input_df$N_treated_I_S[i-1]
    input_df$N_treated_I_R_new[i]<-input_df$N_treated_I_R[i]- input_df$N_treated_I_R[i-1]
    input_df$N_treated_I_mixed_new[i]<-input_df$N_treated_I_mixed[i]- input_df$N_treated_I_mixed[i-1]
    
  } 
  
  list_simulations[[run]]<-input_df
}
saveRDS(list_simulations,  file= "mixed_inf_all_10ippy_infreq_freq0.5.RData")
