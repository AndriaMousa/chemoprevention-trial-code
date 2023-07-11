


plot_diff_low_CI <- function(N_sims= 1000,          ## number of simulated datasets to be generated
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
  
  
  duration_medians<- data.frame(sim=1:N_sims)# a dataframe to store medians and 95%CrI against R and S for each simulation (for each combination of inputs)
  duration_medians$low_R<-NA
  duration_medians$med_R<-NA
  duration_medians$high_R<-NA
  duration_medians$low_S<-NA
  duration_medians$med_S<-NA
  duration_medians$high_S<-NA
  
  parameters<- data.frame(sim=1:N_sims)
  parameters$lambda_R_low<-NA
  parameters$lambda_R_med<-NA
  parameters$lambda_R_high<-NA
  parameters$lambda_S_low<-NA
  parameters$lambda_S_med<-NA
  parameters$lambda_S_high<-NA
  
  parameters$w_R_low<-NA
  parameters$w_R_med<-NA
  parameters$w_R_high<-NA
  parameters$w_S_low<-NA
  parameters$w_S_med<-NA
  parameters$w_S_high<-NA
  
  
  duration_S_minus_R<-data.frame(sim=1:N_sims)
  duration_S_minus_R$low_diff<-NA
  duration_S_minus_R$med_diff<-NA
  duration_S_minus_R$high_diff<-NA
  
  
  for (run in 1:N_sims) {  
    parameters[run,2:4]<- quantile (object[[run]]$lambda_R ,probs=c(0.025,0.5,0.975))
    parameters[run,5:7]<- quantile (object[[run]]$lambda_S ,probs=c(0.025,0.5,0.975))
    parameters[run,8:10]<-quantile (object[[run]]$w_R,probs=c(0.025,0.5,0.975))
    parameters[run,11:13]<-quantile (object[[run]]$w_S,probs=c(0.025,0.5,0.975))
    
    dur_prot_R<-gamma(1+(1/ object[[run]]$w_R)) *  object[[run]]$lambda_R 
    dur_prot_S<-gamma(1+(1/ object[[run]]$w_S)) * object[[run]]$lambda_S 
    duration_medians[run,2:4]<-quantile (dur_prot_R,probs=c(0.025,0.5,0.975))
    duration_medians[run,5:7]<-quantile (dur_prot_S,probs=c(0.025,0.5,0.975))     ### to check % of sims where duration of med_R> low_S (= Power)
    duration_S_minus_R[run,2:4]<-quantile (dur_prot_S  - dur_prot_R,probs=c(0.025,0.5,0.975)) 
  }
  
  power<-length(which(duration_S_minus_R$low_diff>0))/N_sims *100

  
  plot_output<-
    ggplot() + theme_bw()+geom_vline(aes(xintercept = 0), colour="black") + ylim(0,85) + xlim (-25,20)+
    geom_histogram(data=duration_S_minus_R,aes(x=low_diff), bins=100, fill = "black", alpha = 0.2) +
    ylab("Number of simulations") + xlab("Low limit of Credible Interval") + labs(title="") + geom_line() +
    geom_label(aes(x=-10,y=75,label = paste("Power:", format(round(power, 1), nsmall = 1), "%")))
  
  
  
  return(plot_output)
  
  
}
