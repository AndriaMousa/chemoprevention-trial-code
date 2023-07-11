
assemble_posteriors_2strain<- function(N_sims= 1000,          ## number of simulated datasets to be generated
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
                               N0_control0=200,
                               dt=0.5## number in control group. Will only be used if control=1
) {

  object <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)
  

  if (control==0) {
    for (run in 1:N_sims) {
      object[[run]]<- readRDS(paste0("stan_output/freq", freq_R,"_R",
                                     mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                     "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
    }
  }
  
  
  if (control==1) {
    for (run in 1:N_sims) {
      object[[run]]<- readRDS(paste0("stan_output/freq", freq_R,"_R",
                                     mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                     "_determ", prob_determ,
                                     "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
    }
  }
  
  
  medians<- data.frame(sim=1:N_sims)# a dataframe to store medians and 95%CrI against R and S for each simulation (for each combination of inputs)
  medians$PE30_R_low<-NA
  medians$PE30_R_med<-NA
  medians$PE30_R_high<-NA
  medians$mean_dur_prot_R_low<-NA
  medians$mean_dur_prot_R_med<-NA
  medians$mean_dur_prot_R_high<-NA
  
  medians$PE30_S_low<-NA
  medians$PE30_S_med<-NA
  medians$PE30_S_high<-NA
  medians$mean_dur_prot_S_low<-NA
  medians$mean_dur_prot_S_med<-NA
  medians$mean_dur_prot_S_high<-NA
  
  iter_temp<-data.frame(iter=1:10000)
  
  iter_combined<-data.frame(expand_grid(sim=1:1000,iter=1:10000))
  iter_combined$d30PE_R<-NA
  iter_combined$d30PE_S<-NA
  
  iter_combined$mean_dur_prot_R<-NA
  iter_combined$mean_dur_prot_S<-NA
  
  
  for (run in 1:N_sims) {  
    
    iter_temp$PE30_R<-NA
    iter_temp$mean_dur_prot_R<-NA
    iter_temp$PE30_S<-NA
    iter_temp$mean_dur_prot_S<-NA
    
    for (iter in 1:10000){
      
      
      lambda_R<-object[[run]]$lambda_R[iter]
      lambda_S<-object[[run]]$lambda_S[iter]
      
      w_R<-object[[run]]$w_R[iter]
      w_S<-object[[run]]$w_S[iter]
      
      mean_dur_prot_R<-gamma(1+(1/ w_R)) * lambda_R
      mean_dur_prot_S<-gamma(1+(1/ w_S)) * lambda_S
      
      prob_inf<-1-exp(- (object[[run]]$inc[iter])*dt) 
      time<-seq(from=0,to=30,by=dt)
      p_protect_R<- exp(-(time/lambda_R)^w_R)
      p_protect_S<- exp(-(time/lambda_S)^w_S)
      
      
      control_group_R<-control_group_S<-control_I_R<-control_I_S<-control_I_R_new<-control_I_S_new<-vector(length=length(time))
      treated_R<-treated_S<-treated_I_R<-treated_I_S<-treated_I_R_new<-treated_I_S_new<-vector(length=length(time))
      
      treated_R[1]<-1
      treated_S[1]<-1
      
      treated_I_R[1]<-0
      treated_I_S[1]<-0
      
      treated_I_R_new[1]<-0
      treated_I_S_new[1]<-0
      
      control_group_R[1]<-1
      control_group_S[1]<-1
      
      control_I_R[1]<-0
      control_I_S[1]<-0
      
      control_I_R_new[1]<-0
      control_I_S_new[1]<-0
      
      for(i in 2:length(time)) {
        treated_I_R_new[i]<-  treated_R[i-1] * (prob_inf* (1-p_protect_R[i])) 
        treated_I_R[i]<-      treated_I_R[i-1]+ treated_I_R_new[i] ## cumul proportion of new infections in treated group
        treated_R[i]<-        treated_R[i-1]-treated_I_R_new[i]  ##at risk in treated group
      }
      
      for(i in 2:length(time)) {
        treated_I_S_new[i]<-  treated_S[i-1] * (prob_inf* (1-p_protect_S[i])) 
        treated_I_S[i]<-      treated_I_S[i-1]+ treated_I_S_new[i] ## cumul proportion of new infections in treated group
        treated_S[i]<-        treated_S[i-1]-treated_I_S_new[i]  ##at risk in treated group
      }
      
      
      for(i in 2:length(time)) {
        
        control_I_R_new[i]<-control_group_R[i-1]* prob_inf ## proportion of new infections in control group
        control_I_R[i]<- control_I_R[i-1]+ control_I_R_new[i] ## cumul proportion of new infections in control group
        control_group_R[i]<-control_group_R[i-1]-control_I_R_new[i] ##at risk in control group
      }
      
      for(i in 2:length(time)) {
        
        control_I_S_new[i]<-control_group_S[i-1]* prob_inf ## proportion of new infections in control group
        control_I_S[i]<- control_I_S[i-1]+ control_I_S_new[i] ## cumul proportion of new infections in control group
        control_group_S[i]<-control_group_S[i-1]-control_I_S_new[i] ##at risk in control group
      }
      
      iter_temp$PE30_R[iter]<-1-((1-treated_R[length(treated_R)])/(1-control_group_R[length(control_group_R)]))
      iter_temp$mean_dur_prot_R[iter]<-mean_dur_prot_R
      
      iter_temp$PE30_S[iter]<-1-((1-treated_S[length(treated_S)])/(1-control_group_S[length(control_group_S)]))
      iter_temp$mean_dur_prot_S[iter]<-mean_dur_prot_S
      
    }
    iter_combined$d30PE_R[which(iter_combined$sim==run)]<-iter_temp$PE30_R
    iter_combined$mean_dur_prot_R[which(iter_combined$sim==run)]<-iter_temp$mean_dur_prot_R
    iter_combined$d30PE_S[which(iter_combined$sim==run)]<-iter_temp$PE30_S
    iter_combined$mean_dur_prot_S[which(iter_combined$sim==run)]<-iter_temp$mean_dur_prot_S
    
    medians[run,2:4]<-quantile (iter_temp$PE30_R,probs=c(0.025,0.5,0.975))
    medians[run,5:7]<-quantile (iter_temp$mean_dur_prot_R,probs=c(0.025,0.5,0.975))
    
    medians[run,8:10]<-quantile (iter_temp$PE30_S,probs=c(0.025,0.5,0.975))
    medians[run,11:13]<-quantile (iter_temp$mean_dur_prot_S,probs=c(0.025,0.5,0.975))
    
    print(run)
    
  }

  
  if (control==0) {
    write.csv(iter_combined,paste0("saved_dfs/2strain_model/iter_combined_freq", freq_R,"_R",
                                   mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                   "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,".csv"))
    
    write.csv(medians,paste0("saved_dfs/2strain_model/medians_freq", freq_R,"_R",
                             mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                             "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,".csv"))
  }
  
  if (control==1) {
    write.csv(iter_combined,paste0("saved_dfs/2strain_model/iter_combined_",mean_protect,
                                   "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                   "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,".csv"))
    write.csv(medians,paste0("saved_dfs/2strain_model/medians_freq", freq_R,"_R",
                             mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                             "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,".csv"))
    
  }

}
