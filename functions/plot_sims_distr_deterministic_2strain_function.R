


sims_distr_deterministic_plot_2strain <- function(N_sims= 1000,          ## number of simulated datasets to be generated
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
                           N0_control0=200,        ## number in control group. Will only be used if control=1
                           dt=0.5                  ##time step
) {
  
  
  
  if (control==0) {

    input_sim_dfs<- readRDS(paste0("simulated_data/input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,".RData"))
  }


  if (control==1 & AS_or_placebo=="AS") {
    
    input_sim_dfs<-readRDS(paste0("simulated_data/input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "AS_",followup,"d_ltf",ltf,".RData"))
  }
  
  if (control==1 & AS_or_placebo=="placebo") {
    
    input_sim_dfs<-readRDS(paste0("simulated_data/input_df_sim_freq",freq_R,"_R",
                        mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                        "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "plac_",followup,"d_ltf",ltf,".RData"))
  }
  

  
  time<-seq(from=0,to=followup,by=dt)
  
  deterministic_treated_I_R_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  deterministic_treated_I_S_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  deterministic_treated_I_undeterm_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  
  if (control==1) {
    
  deterministic_control_I_R_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  deterministic_control_I_S_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  deterministic_control_I_undeterm_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  }
  
  inc<-ippy/365
  prob_inf<-1-exp(-inc*dt)  ## prob of infection at each time step
  
  
  lambda_S<- mean_protect_S/ (gamma(1+(1/w_S)))            ### calculate gamma (scale parameter) based on the mean
  lambda_R<- mean_protect_R/ (gamma(1+(1/w_R)))
  
  p_protect_R<- exp(-((time)/lambda_R)^w_R)
  p_protect_S<- exp(-((time)/lambda_S)^w_S)
  
  
  ################ treated grp
  treated<-treated_I_R<-treated_I_S<-treated_I_undeterm<-vector(length=length(time))
  treated_I_R_new<-treated_I_S_new<-treated_I_undeterm_new<-vector(length=length(time))
  
  ## everyone is susceptible in the first time point
  treated[1]<-1
  
  ### nobody is infected in the first time point
  treated_I_S[1]<-0
  treated_I_R[1]<-0
  treated_I_undeterm[1]<-0
  
  treated_I_S_new[1]<-0
  treated_I_R_new[1]<-0
  treated_I_undeterm_new[1]<-0
  
  ############## control group
  
  if (control==1) {
    control_group<-control_I_R<-control_I_S<-control_I_undeterm<-vector(length=length(time))
    control_I_R_new<-control_I_S_new<-control_I_undeterm_new<-vector(length=length(time))
    
  
  ## everyone is susceptible in the first time point
  control_group[1]<-1
  
  ### nobody is infected in the first time point
  control_I_S[1]<-0
  control_I_R[1]<-0
  control_I_undeterm[1]<-0
  
  control_I_S_new[1]<-0
  control_I_R_new[1]<-0
  control_I_undeterm_new[1]<-0
  }
  ############
  
  
  for(i in 2:length(time)) {
    
    
  ###chemoprevention group
    
    treated_I_S_new[i]<-     treated[i-1]* (prob_inf*(1-freq_R)*(1-p_protect_S[i])) * prob_determ ## proportion of new infections with S in treated group
    treated_I_R_new[i]<-     treated[i-1]* (prob_inf*  (freq_R)*(1-p_protect_R[i])) * prob_determ     ## proportion of new infections with R in treated group
    treated_I_undeterm_new[i]<- (treated[i-1]* (prob_inf*(1-freq_R)*(1-p_protect_S[i])) + 
                                   (treated[i-1]* (prob_inf*  (freq_R)*(1-p_protect_R[i])))) * (1-prob_determ)
    
    
    treated_I_S[i]<- treated_I_S[i-1]+ treated_I_S_new[i] ## proportion of new infections with S in treated group
    treated_I_R[i]<- treated_I_R[i-1]+ treated_I_R_new[i]    ## proportion of new infections with R in treated group
    treated_I_undeterm[i]<- treated_I_undeterm[i-1] + treated_I_undeterm_new[i]
    
    treated[i]<-treated[i-1]-treated_I_S_new[i]- treated_I_R_new[i]-treated_I_undeterm_new[i]
  }
    ####control group
    if (control==1) {
      for(i in 2:length(time)) {
        
      
    control_I_S_new[i]<-         control_group[i-1]* prob_inf*(1-freq_R) * prob_determ ## proportion of new infections with S in control group
    control_I_R_new[i]<-         control_group[i-1]* prob_inf*   freq_R * prob_determ     ## proportion of new infections with R in control group
    control_I_undeterm_new[i]<-  ((control_group[i-1]* prob_inf*(1-freq_R)) + (control_group[i-1]* prob_inf*freq_R)) * (1-prob_determ)
    
    
    control_I_S[i]<- control_I_S[i-1]+ control_I_S_new[i] ## proportion of new infections with S in control group
    control_I_R[i]<- control_I_R[i-1]+ control_I_R_new[i]    ## proportion of new infections with R in control group
    control_I_undeterm[i]<- control_I_undeterm[i-1] + control_I_undeterm_new[i]
    
    control_group[i]<-control_group[i-1]-control_I_S_new[i]- control_I_R_new[i]-control_I_undeterm_new[i]
    }
  }
  
  
  # 
  deterministic_treated_I_R_df$prop<-treated_I_R
  deterministic_treated_I_S_df$prop<-treated_I_S
  deterministic_treated_I_undeterm_df$prop<-treated_I_undeterm
  
  
  
  if (control==1) {
    
    deterministic_control_I_R_df$prop<-control_I_R
    deterministic_control_I_S_df$prop<-control_I_S
    deterministic_control_I_undeterm_df$prop<-control_I_undeterm
  }
  
  
  if (followup==63){
    breaks <- c(0,2,3,5,7,14,21,28,35,42,49,56,63)
  }
  
  if (followup==42){
    breaks <- c(0,2,3,5,7,14,21,28,35,42)
  }
  
  if (followup==28){
    breaks <- c(0,2,3,5,7,14,21,28)
  }
  
  
  percentiles_R<-data.frame(days=1:length(breaks))
  percentiles_S<-data.frame(days=1:length(breaks))
  percentiles_undeterm<-data.frame(days=1:length(breaks))
  
  
  percentiles_R$genotype<-"Resistant"
  percentiles_S$genotype<-"Sensitive"
  percentiles_undeterm$genotype<-"Undetermined"
  
  
  percentiles_R$days<-breaks
  percentiles_S$days<-breaks
  percentiles_undeterm$days<-breaks
  
  
  percentiles_R$group<-"Treated"
  percentiles_S$group<-"Treated"
  percentiles_undeterm$group<-"Treated"
  
  percentiles_R$perc_0.025<-NA
  percentiles_R$perc_0.5<-NA
  percentiles_R$perc_0.975<-NA
  percentiles_S$perc_0.025<-NA
  percentiles_S$perc_0.5<-NA
  percentiles_undeterm$perc_0.975<-NA
  percentiles_undeterm$perc_0.025<-NA
  percentiles_undeterm$perc_0.5<-NA
  percentiles_undeterm$perc_0.975<-NA
  
  
  runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_undeterm<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_treated_R[run,]<- input_sim_dfs[[run]]$N_treated_I_R[]/ (input_sim_dfs[[run]]$N_treated_I_undeterm[]+input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_S[run,]<- input_sim_dfs[[run]]$N_treated_I_S[]/ (input_sim_dfs[[run]]$N_treated_I_undeterm[]+input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_undeterm[run,]<- input_sim_dfs[[run]]$N_treated_I_undeterm[]/ (input_sim_dfs[[run]]$N_treated_I_undeterm[]+input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_R$perc_0.025[time]<-quantile(runs_N_treated_R[,time], probs = c(0.025))
    percentiles_R$perc_0.5[time]<-quantile(runs_N_treated_R[,time], probs = c(0.5))
    percentiles_R$perc_0.975[time]<-quantile(runs_N_treated_R[,time],probs = c(0.975))
    percentiles_S$perc_0.025[time]<-quantile(runs_N_treated_S[,time], probs = c(0.025))
    percentiles_S$perc_0.5[time]<-quantile(runs_N_treated_S[,time], probs = c(0.5))
    percentiles_S$perc_0.975[time]<-quantile(runs_N_treated_S[,time],probs = c(0.975))
    
    percentiles_undeterm$perc_0.025[time]<-quantile(runs_N_treated_undeterm[,time], probs = c(0.025))
    percentiles_undeterm$perc_0.5[time]<-quantile(runs_N_treated_undeterm[,time], probs = c(0.5))
    percentiles_undeterm$perc_0.975[time]<-quantile(runs_N_treated_undeterm[,time],probs = c(0.975))
  }
  
  
  
  
  if (control==1) {
    
  
  percentiles_control_R<-data.frame(days=1:length(breaks))
  percentiles_control_S<-data.frame(days=1:length(breaks))
  percentiles_control_undeterm<-data.frame(days=1:length(breaks))
  
  
  percentiles_control_R$genotype<-"Resistant"
  percentiles_control_S$genotype<-"Sensitive"
  percentiles_control_undeterm$genotype<-"Undetermined"
  
  
  percentiles_control_R$days<-breaks
  percentiles_control_S$days<-breaks
  percentiles_control_undeterm$days<-breaks
  
  
  percentiles_control_R$group<-"Control"
  percentiles_control_S$group<-"Control"
  percentiles_control_undeterm$group<-"Control"
  
  percentiles_control_R$perc_0.025<-NA
  percentiles_control_R$perc_0.5<-NA
  percentiles_control_R$perc_0.975<-NA
  percentiles_control_S$perc_0.025<-NA
  percentiles_control_S$perc_0.5<-NA
  percentiles_control_undeterm$perc_0.975<-NA
  percentiles_control_undeterm$perc_0.025<-NA
  percentiles_control_undeterm$perc_0.5<-NA
  percentiles_control_undeterm$perc_0.975<-NA
  
  
  runs_N_control_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_control_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_control_undeterm<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_control_R[run,]<- input_sim_dfs[[run]]$N_control_I_R[]/ (input_sim_dfs[[run]]$N_control_I_undeterm[]+input_sim_dfs[[run]]$N_control_I_R[] +input_sim_dfs[[run]]$N_control_I_S[]+ input_sim_dfs[[run]]$N_control_uninf[])
    runs_N_control_S[run,]<- input_sim_dfs[[run]]$N_control_I_S[]/ (input_sim_dfs[[run]]$N_control_I_undeterm[]+input_sim_dfs[[run]]$N_control_I_R[] +input_sim_dfs[[run]]$N_control_I_S[]+ input_sim_dfs[[run]]$N_control_uninf[])
    runs_N_control_undeterm[run,]<- input_sim_dfs[[run]]$N_control_I_undeterm[]/ (input_sim_dfs[[run]]$N_control_I_undeterm[]+input_sim_dfs[[run]]$N_control_I_R[] +input_sim_dfs[[run]]$N_control_I_S[]+ input_sim_dfs[[run]]$N_control_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_control_R$perc_0.025[time]<-quantile(runs_N_control_R[,time], probs = c(0.025))
    percentiles_control_R$perc_0.5[time]<-quantile(runs_N_control_R[,time], probs = c(0.5))
    percentiles_control_R$perc_0.975[time]<-quantile(runs_N_control_R[,time],probs = c(0.975))
    percentiles_control_S$perc_0.025[time]<-quantile(runs_N_control_S[,time], probs = c(0.025))
    percentiles_control_S$perc_0.5[time]<-quantile(runs_N_control_S[,time], probs = c(0.5))
    percentiles_control_S$perc_0.975[time]<-quantile(runs_N_control_S[,time],probs = c(0.975))
    
    percentiles_control_undeterm$perc_0.025[time]<-quantile(runs_N_control_undeterm[,time], probs = c(0.025))
    percentiles_control_undeterm$perc_0.5[time]<-quantile(runs_N_control_undeterm[,time], probs = c(0.5))
    percentiles_control_undeterm$perc_0.975[time]<-quantile(runs_N_control_undeterm[,time],probs = c(0.975))
  }
  
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_undeterm, percentiles_control_R,percentiles_control_S, percentiles_control_undeterm)
  
  }
  
  if (control==0) {
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_undeterm)
  }
  
  percentiles$genotype<-as.factor(percentiles$genotype)
  
  if (control==1) {
  percentiles$arm_genotype<-paste0(as.character(percentiles$group)," ", as.character(percentiles$genotype))
  percentiles$arm_genotype<-as.factor(percentiles$arm_genotype)
  }
  
  
  if (control==0) {
    
  plot_output<- ggplot() + theme_bw()+ theme(legend.title = element_blank())+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=genotype),  size = 1.5, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=genotype),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    scale_colour_manual(values = c( "Resistant"="violetred1", "Sensitive"="royalblue", "Undetermined"= "purple"))+
    scale_fill_manual(values = c( "Resistant"="violetred1", "Sensitive"="royalblue","Undetermined"= "purple")) +
    geom_line(data=deterministic_treated_I_R_df, aes(x=time,y=prop), colour="violetred1", linetype="dotted", size = 1.5)+
    geom_line(data=deterministic_treated_I_S_df, aes(x=time,y=prop), colour="royalblue", linetype="dotted",  size = 1.5)+
    geom_line(data=deterministic_treated_I_undeterm_df, aes(x=time,y=prop), colour="purple", linetype="dotted",  size = 1.5)+ xlim(0, 63)
  
  }
  
  if (control==1) {
    
    plot_output<- 
      ggplot() + theme_bw()+ theme(legend.title = element_blank())+
      geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=arm_genotype),  size = 1.5, alpha=0.5) +
      geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=arm_genotype),  alpha=0.2) +
      ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
      scale_colour_manual(values = c( "Treated Resistant"="violetred1", "Treated Sensitive"="royalblue", "Treated Undetermined"= "purple",
                                      "Control Resistant"="orange", "Control Sensitive"="darkgreen", "Control Undetermined"= "chocolate4"))+
      scale_fill_manual(values = c( "Treated Resistant"="violetred1", "Treated Sensitive"="royalblue", "Treated Undetermined"= "purple",
                                    "Control Resistant"="orange", "Control Sensitive"="darkgreen", "Control Undetermined"= "chocolate4")) +
      geom_line(data=deterministic_treated_I_R_df, aes(x=time,y=prop), colour="violetred1", linetype="dotted", size = 1.5)+
      geom_line(data=deterministic_treated_I_S_df, aes(x=time,y=prop), colour="royalblue", linetype="dotted",  size = 1.5)+
      geom_line(data=deterministic_treated_I_undeterm_df, aes(x=time,y=prop), colour="purple", linetype="dotted",  size = 1.5)+ xlim(0, 63) +
      geom_line(data=deterministic_control_I_R_df, aes(x=time,y=prop), colour="orange", linetype="dotted", size = 1.5)+
      geom_line(data=deterministic_control_I_S_df, aes(x=time,y=prop), colour="darkgreen", linetype="dotted",  size = 1.5)+
      geom_line(data=deterministic_control_I_undeterm_df, aes(x=time,y=prop), colour="chocolate4", linetype="dotted",  size = 1.5)
    
  }
  
  return(plot_output)
  
}




