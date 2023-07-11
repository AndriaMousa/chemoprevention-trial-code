

transmission<- c(1,2,4)
protection<- c(10,15,20,25,30)
N_per_arm<-c(25,50,75,100,125,150,175,200,225,250)

combos<-expand.grid(transmission=transmission, protection=protection, N_per_arm=N_per_arm)

combos$power<-NA


  

N_sims= 1000          ## number of simulated datasets to be generated
w=5                   ## shape parameter
followup=63            ## specify total followup


for (i in 1:nrow(combos)) {
  
  
  mean_protect= combos$protection[i]       ## mean duration of protection 
  N0_treat=combos$N_per_arm[i]          ## sample size (enrolled and treated with chemoprevention) 
  ippy=combos$transmission[i]               ## infection rate (infections per person per year. Includes asymptomatic infections
  N0_control=combos$N_per_arm[i]

input_sim_dfs<-readRDS( file=paste0("additional supplementary materials/Cox PH method/sim_data/input_df_sim_",
                                      mean_protect,"days_",ippy,"ippy",
                                      "_Ntreat",N0_treat,"_Ncontrol",N0_control,".RData"))

scenario<-data.frame(sim=seq(1,N_sims,by=1))
scenario$coxph_p_value<-NA
  
###loop over sims with run

for (run in 1:N_sims) {   ## change to 1000
  
#first read in 1 simulated dataset
temp_df<-input_sim_dfs[[run]]

#### generate a variable that shows how many invdividuals are censored at each time point (either infected or stopped following)
temp_df$censored_treated<- temp_df$N_treated_I_new
temp_df$censored_treated[13]<- temp_df$N_treated_I_new[13]+temp_df$N_treated_uninf[13]
temp_df$censored_control<- temp_df$N_control_I_new
temp_df$censored_control[13]<- temp_df$N_control_I_new[13]+temp_df$N_control_uninf[13]


keeps_treated <- c("T", "censored_treated")
temp_treated_df = temp_df[keeps_treated]
keeps_control <- c("T", "censored_control")
temp_control_df = temp_df[keeps_control]

treated_df = temp_treated_df %>% 
  slice(rep(1:n(), censored_treated)) %>% 
  select(-censored_treated)

treated_df$arm<-"treated" 
treated_df$infection_event<-1
treated_df$infection_event[(1+temp_df$N_treated_uninf[1]-temp_df$N_treated_uninf[13]): temp_df$N_treated_uninf[1]]<-0

control_df = temp_control_df %>% 
  slice(rep(1:n(), censored_control)) %>% 
  select(-censored_control)

control_df$arm<-"control" 
control_df$infection_event<-1
control_df$infection_event[(1+temp_df$N_control_uninf[1]-temp_df$N_control_uninf[13]): temp_df$N_control_uninf[1]]<-0

combined_df<-bind_rows(list(treated_df,control_df))
combined_df$arm <- factor(combined_df$arm, levels = c("treated","control"))


scenario$coxph_p_value[run]<-summary(coxph(Surv(T, infection_event) ~ arm, data = combined_df))$logtest[3]  ## where cox prop haz test p value is stored
}

combos$power[i]<-table(scenario$coxph_p_value<0.05)[2]/N_sims

print(paste0("combo:", i))
}


write.csv(combos, "additional supplementary materials/Cox PH method/combos_power.csv")
