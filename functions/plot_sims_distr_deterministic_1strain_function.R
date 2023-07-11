

sims_distr_deterministic_plot_1strain <- function(N_sims= 1000,          ## number of simulated datasets to be generated
                           w=5,                 ## shape parameter
                           mean_protect= 20,    ## mean duration of protection against S
                           N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                           followup=63,           ## specify total followup
                           ippy_av=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                           prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                           ltf=0.10,              ## loss to followup
                           control=0,             ## presence of control group? 0=no,1=yes
                           AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                           N0_control0=200,        ## number in control group. Will only be used if control=1
                           seasonality="none",
                           dt=0.5                  ##time step
) {
  
  
  
  if (control==0) {

    input_sim_dfs<- readRDS(paste0("simulated_data/input_df_sim_",
                                   mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                   "_N",N0_treat0,"_",followup,"d_ltf",ltf,".RData"))
  }

  

  if (control==1 & AS_or_placebo=="AS") {
    
    input_sim_dfs<-readRDS(paste0("simulated_data/input_df_sim_",
                                  mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                  "_N",N0_treat0,"_",N0_control0, "AS_",followup,"d_ltf",ltf,".RData"))
  }
  
  if (control==1 & AS_or_placebo=="placebo") {
    
    input_sim_dfs<-readRDS(paste0("simulated_data/input_df_sim_", 
                                  mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                  "_N",N0_treat0,"_",N0_control0, "plac_",followup,"d_ltf",ltf,".RData"))
  }
  

  
  time<-seq(from=0,to=followup,by=dt)
  
  deterministic_treated_I_df<-data.frame(time=seq(from=0,to=followup,by=dt))

  if (control==1) {
    
  deterministic_control_I_df<-data.frame(time=seq(from=0,to=followup,by=dt))
  }
  
 
   df_seasonality <-data.frame(time=seq(from=0,to=followup,by=dt))
  
  inf_inc_min<- ippy_av  - (0.5*ippy_av )
  inf_inc_max<- ippy_av  + (0.5*ippy_av ) 
  df_seasonality$inf_inc<-NA
  if (seasonality=="none") {
    
    df_seasonality$inf_inc[]<-ippy_av
  }
  
  if (seasonality=="start") {
    
    df_seasonality$inf_inc [1]<-inf_inc_min
    
    for (half_day in (1:(followup/dt)) ){
      row<-half_day+1
      df_seasonality$inf_inc[row]<-df_seasonality$inf_inc [half_day] + (inf_inc_max-inf_inc_min)/(followup/dt)
      
    }  
    
  }
  
  if (seasonality=="end") {              
    
    df_seasonality$inf_inc [1]<-inf_inc_max
    
    for (half_day in (1:(followup/dt)) ){
      row<-half_day+1
      df_seasonality$inf_inc [row]<-df_seasonality$inf_inc [half_day] - (inf_inc_max-inf_inc_min)/(followup/dt)
      
    }  
    
  }
  
 
  df_seasonality$prob_inf<-NA
  df_seasonality$prob_inf[]<-1-exp(- (df_seasonality$inf_inc[]/365)*dt) ##daily probability of infection
  
  
  
  lambda<- mean_protect/ (gamma(1+(1/w)))            ### calculate gamma (scale parameter) based on the mean

  p_protect<- exp(-((time)/lambda)^w)

  
  ################ treated grp
  treated<-treated_I<-treated_I_new<-vector(length=length(time))

  ## everyone is susceptible in the first time point
  treated[1]<-1
  
  ### nobody is infected in the first time point
  treated_I[1]<-0

  treated_I_new[1]<-0

  ############## control group
  
  if (control==1) {
    control_group<-control_I<-control_I_new<-vector(length=length(time))

  
  ## everyone is susceptible in the first time point
  control_group[1]<-1
  
  ### nobody is infected in the first time point
  control_I[1]<-0

  
  control_I_new[1]<-0
  }
  ############
  

  for(i in 2:length(time)) {
    
    
  ###chemoprevention group
    
    treated_I_new[i]<-     treated[i-1] * (df_seasonality$prob_inf[i]*(1-p_protect[i]))  ## proportion of new infections in treated group

    
    treated_I[i]<- treated_I[i-1]+ treated_I_new[i] ## proportion of new infections in treated group

    treated[i]<-treated[i-1]-treated_I_new[i]
  }
    ####control group
    if (control==1) {
      for(i in 2:length(time)) {
        
      
    control_I_new[i]<-         control_group[i-1]* df_seasonality$prob_inf[i]
    ## proportion of new infections in control group
    control_I[i]<- control_I[i-1]+ control_I_new[i] ## proportion of new infections in control group
    control_group[i]<-control_group[i-1]-control_I_new[i]
    }
  }
  
  
  # 
  deterministic_treated_I_df$prop<-treated_I

  
  
  if (control==1) {
    
    deterministic_control_I_df$prop<-control_I
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
  
  
  percentiles_treated<-data.frame(days=1:length(breaks))

  
  
  percentiles_treated$days<-breaks
  
  
  percentiles_treated$group<-"Treated"

  percentiles_treated$perc_0.025<-NA
  percentiles_treated$perc_0.5<-NA
  percentiles_treated$perc_0.975<-NA

  
  
  runs_N_treated<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))

  for (run in 1:N_sims) { 
    runs_N_treated[run,]<- input_sim_dfs[[run]]$N_treated_I[]/ (input_sim_dfs[[run]]$N_treated_I[]+ input_sim_dfs[[run]]$N_treated_uninf[])

  }
  
  for (time in 1:length(breaks)) { 
    percentiles_treated$perc_0.025[time]<-quantile(runs_N_treated[,time], probs = c(0.025))
    percentiles_treated$perc_0.5[time]<-quantile(runs_N_treated[,time], probs = c(0.5))
    percentiles_treated$perc_0.975[time]<-quantile(runs_N_treated[,time],probs = c(0.975))

  }
  
  
  
  
  if (control==1) {
    
  
  percentiles_control<-data.frame(days=1:length(breaks))
  percentiles_control$days<-breaks
  percentiles_control$group<-"Control"

  percentiles_control$perc_0.025<-NA
  percentiles_control$perc_0.5<-NA
  percentiles_control$perc_0.975<-NA
  
  
  runs_N_control<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))

  for (run in 1:N_sims) { 
    runs_N_control[run,]<- input_sim_dfs[[run]]$N_control_I[]/ (input_sim_dfs[[run]]$N_control_I[]+ input_sim_dfs[[run]]$N_control_uninf[])

  }
  
  for (time in 1:length(breaks)) { 
    percentiles_control$perc_0.025[time]<-quantile(runs_N_control[,time], probs = c(0.025))
    percentiles_control$perc_0.5[time]<-quantile(runs_N_control[,time], probs = c(0.5))
    percentiles_control$perc_0.975[time]<-quantile(runs_N_control[,time],probs = c(0.975))

  }
  
  percentiles<-rbind(percentiles_treated, percentiles_control)
  
  }
  
  if (control==0) {
  percentiles<-percentiles_treated
  }
  
  
  
  if (control==0) {
    
  plot_output<- ggplot() + theme_bw()+ theme(legend.title = element_blank(), legend.position = "none")+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour="orange"),  size = 1.2, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill="#F8766D"),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    geom_line(data=deterministic_treated_I_df, aes(x=time,y=prop), colour="orange", linetype="dotted",  size = 1.2)+ xlim(0, 63) + ylim(0,1)
  
  }
  
  if (control==1) {
    
    plot_output<- 
      ggplot() + theme_bw()+ theme(legend.title = element_blank())+
      geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=group),  size = 1.2, alpha=0.5) +
      geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=group),  alpha=0.2) +
      ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
      scale_colour_manual(values = c( "Treated"="#F8766D", "Control"="#00BA38"))+
      scale_fill_manual(values = c( "Treated"="#F8766D", "Control"="#00BA38")) +
      geom_line(data=deterministic_treated_I_df, aes(x=time,y=prop), colour="orange", linetype="dotted",  size = 1.2)+ xlim(0, 63) + ylim(0,1)+
      geom_line(data=deterministic_control_I_df, aes(x=time,y=prop), colour="green", linetype="dotted",  size = 1.2)
    
  }
  
  return(plot_output)
  
}


