plot_PE_over_time_2strain <- function(N_sims= 1000,          ## number of simulated datasets to be generated
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
  
  
  object <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)
  
  
  if (control==0) {
    for (run in 1:N_sims) {
      object[[run]]<- readRDS(paste0("stan_output/freq",freq_R,"_R",
                                     mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                     "_determ", prob_determ,"_N",N0_treat0,"_",followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
    }
  }
  
  
  if (control==1 & AS_or_placebo=="AS") {
    for (run in 1:N_sims) {
      
      object[[run]]<-readRDS(paste0("stan_output/freq",freq_R,"_R",
                                    mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                    "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "AS_",followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
    }
  }
  
  
  if (control==1 & AS_or_placebo=="placebo") {
    for (run in 1:N_sims) {
      
      object[[run]]<-readRDS(paste0("stan_output/freq",freq_R,"_R",
                                    mean_protect_R,"days_",ippy,"ippy_prev",prevalence,
                                    "_determ", prob_determ,"_N",N0_treat0,"_",N0_control0, "plac_",followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
    }
  }
  
  sims<-seq(1,N_sims,by=1)
  time<-seq(0,63,by=1) ###can change later to dt 
  
  
  
  df_sims_pars<-as.data.frame(expand.grid(sims))
  names( df_sims_pars)<-("sims")
  
  ##to store model parameter outputs per sim
  df_sims_pars$med_lambda_R<-NA
  df_sims_pars$med_w_R<-NA
  df_sims_pars$med_lambda_S<-NA
  df_sims_pars$med_w_S<-NA
  
  ##
  for (run in 1:N_sims) {
    df_sims_pars$med_lambda_R[which(df_sims_pars$sims==run)]<-median(object[[run]]$lambda_R)
    df_sims_pars$med_lambda_S[which(df_sims_pars$sims==run)]<-median(object[[run]]$lambda_S)
    
    df_sims_pars$med_w_R[which(df_sims_pars$sims==run)]<-median(object[[run]]$w_R)
    df_sims_pars$med_w_S[which(df_sims_pars$sims==run)]<-median(object[[run]]$w_S)
  }
  
  ##to store model parameter outputs (medians across all sims)
  med_lambda_R<-median(df_sims_pars$med_lambda_R)
  med_w_R<-median(df_sims_pars$med_w_R)
  med_lambda_S<-median(df_sims_pars$med_lambda_S)
  med_w_S<-median(df_sims_pars$med_w_S)
  
  ###generate large df with PE over time using median parameters
  
  df_sims_PE_over_time<-as.data.frame(expand.grid(time,sims))
  names( df_sims_PE_over_time)<-c("time","sims")
  
  
  df_sims_PE_over_time$PE_R<-NA
  df_sims_PE_over_time$PE_S<-NA
  
  for (time in 0:63) {
    
    for (run in 1:N_sims) {
      df_sims_PE_over_time$PE_R[which(df_sims_PE_over_time$sims==run & df_sims_PE_over_time$time==time)]<-
        exp(-((time/(df_sims_pars$med_lambda_R[which(df_sims_pars$sims==run)])
        ))^(df_sims_pars$med_w_R[which(df_sims_pars$sims==run)]))
      
      df_sims_PE_over_time$PE_S[which(df_sims_PE_over_time$sims==run & df_sims_PE_over_time$time==time)]<-
        exp(-((time/(df_sims_pars$med_lambda_S[which(df_sims_pars$sims==run)])
        ))^(df_sims_pars$med_w_S[which(df_sims_pars$sims==run)]))
    }
    
    print(time)
  }
  
  time<-seq(0,63,by=1) ###can change later to dt 
  
  df_medians_PE_over_time<-as.data.frame(time)
  names( df_medians_PE_over_time)<-("time")
  
  df_medians_PE_over_time$PE_R<-NA
  df_medians_PE_over_time$PE_S<-NA
  
  for (time in 0:63) {
    df_medians_PE_over_time$PE_R[which(df_medians_PE_over_time$time==time)]<- exp(-((time/med_lambda_R))^med_w_R)
    df_medians_PE_over_time$PE_S[which(df_medians_PE_over_time$time==time)]<- exp(-((time/med_lambda_S))^med_w_S)
    
    
  }
  
  plot_output<- ggplot() + theme_bw()+ theme(legend.title = element_blank())+
    geom_line(data=df_sims_PE_over_time, color=alpha("#D82632",0.02), aes(x=time, y=PE_R, group = sims))+
    geom_line(data=df_sims_PE_over_time, color=alpha("#290AD8",0.02), aes(x=time, y=PE_S, group = sims))+
    geom_line(data=df_medians_PE_over_time, aes(x=time, y=PE_R), colour="#D82632",  linewidth = 1.5) +
    geom_line(data=df_medians_PE_over_time, aes(x=time, y=PE_S), colour="#290AD8",  linewidth = 1.5) +
    ylab("Protective efficacy") + xlab("Days since treatment") 
  
  
  return(plot_output)
  
  
}
