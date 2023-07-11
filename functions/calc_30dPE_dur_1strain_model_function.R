

assemble_posteriors_1strain<- function(N_sims= 1000,          ## number of simulated datasets to be generated
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
  


object <- vector(mode = "list", length = N_sims) ## creates a list to save all input dataframes with simulated data (one for each simulation)


lambda<- mean_protect/ (gamma(1+(1/w)))  

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

prob_inf<-df_seasonality$prob_inf[1:61]
time<-seq(from=0,to=30,by=dt)

p_protect<- exp(-(time/lambda)^w)

################ treated grp
control_group<-control_I<-control_I_new<-treated<-treated_I<-treated_I_new<-vector(length=length(time))

## everyone is susceptible in the first time point
treated[1]<-1

### nobody is infected in the first time point
treated_I[1]<-0
treated_I_new[1]<-0
## everyone is susceptible in the first time point
control_group[1]<-1

### nobody is infected in the first time point
control_I[1]<-0
control_I_new[1]<-0


for(i in 2:length(time)) {
  
  
  ###chemoprevention group
  
  treated_I_new[i]<-  treated[i-1] * (prob_inf[i]*(1-p_protect[i]))  ## proportion of new infections in treated group
  treated_I[i]<-      treated_I[i-1]+ treated_I_new[i] ## proportion of new infections in treated group
  treated[i]<-        treated[i-1]-treated_I_new[i]
}
  #### theoretical control group
for(i in 2:length(time)) {
  
  
  control_I_new[i]<-         control_group[i-1]* prob_inf[i]
  control_I[i]<- control_I[i-1]+ control_I_new[i] ## proportion of new infections in control group
  control_group[i]<-control_group[i-1]-control_I_new[i]
}

determ_true<-1-((1-treated[length(treated)])/(1-control_group[length(control_group)]))




if (control==0) {
  for (run in 1:N_sims) {
    object[[run]]<- readRDS(paste0("stan_output/input_df_sim_", 
                                  mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                  "_N",N0_treat0,"_",followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
  }
}


if (control==1) {
  for (run in 1:N_sims) {
    object[[run]]<- readRDS(paste0("stan_output/input_df_sim_", 
                                   mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                   "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,"/stan_output_run",run,".rds"))
  }
}





medians<- data.frame(sim=1:N_sims)# a dataframe to store medians and 95%CrI against R and S for each simulation (for each combination of inputs)
medians$PE30_low<-NA
medians$PE30_med<-NA
medians$PE30_high<-NA
medians$mean_dur_prot_low<-NA
medians$mean_dur_prot_med<-NA
medians$mean_dur_prot_high<-NA

iter_temp<-data.frame(iter=1:10000)

iter_combined<-data.frame(expand_grid(sim=1:1000,iter=1:10000))
iter_combined$d30PE<-NA
iter_combined$mean_dur_prot<-NA


for (run in 1:N_sims) {  

iter_temp$PE30<-NA
iter_temp$mean_dur_prot<-NA

for (iter in 1:10000){

  
lambda<-object[[run]]$lambda[iter]
w<-object[[run]]$w[iter]

mean_dur_prot<-gamma(1+(1/ w)) * lambda

prob_inf<-1-exp(- (object[[run]]$inc[iter])*dt) 
time<-seq(from=0,to=30,by=dt)
p_protect<- exp(-(time/lambda)^w)
control_group<-control_I<-control_I_new<-treated<-treated_I<-treated_I_new<-vector(length=length(time))
treated[1]<-1
treated_I[1]<-0
treated_I_new[1]<-0
control_group[1]<-1
control_I[1]<-0
control_I_new[1]<-0
for(i in 2:length(time)) {
  treated_I_new[i]<-  treated[i-1] * (prob_inf*(1-p_protect[i]))  ## proportion of new infections in treated group
  treated_I[i]<-      treated_I[i-1]+ treated_I_new[i] ## cumul proportion of new infections in treated group
  treated[i]<-        treated[i-1]-treated_I_new[i]  ##at risk in treated group
}
for(i in 2:length(time)) {

  control_I_new[i]<-         control_group[i-1]* prob_inf ## proportion of new infections in control group
  control_I[i]<- control_I[i-1]+ control_I_new[i] ## cumul proportion of new infections in control group
  control_group[i]<-control_group[i-1]-control_I_new[i] ##at risk in control group
}

iter_temp$PE30[iter]<-1-((1-treated[length(treated)])/(1-control_group[length(control_group)]))
iter_temp$mean_dur_prot[iter]<-mean_dur_prot

}
iter_combined$d30PE[which(iter_combined$sim==run)]<-iter_temp$PE30
iter_combined$mean_dur_prot[which(iter_combined$sim==run)]<-iter_temp$mean_dur_prot

medians[run,2:4]<-quantile (iter_temp$PE30,probs=c(0.025,0.5,0.975))
medians[run,5:7]<-quantile (iter_temp$mean_dur_prot,probs=c(0.025,0.5,0.975))

print(run)

}


ggplot() + theme_bw()+
  geom_histogram(data=iter_combined, aes(d30PE, y=..density..), binwidth=0.01, colour="white", fill=alpha("black", 0.2)) + 
  geom_histogram(data=medians, aes(PE30_med, y=..density..), binwidth=0.01, colour="white", fill=alpha("black", 0.5)) + 
  geom_vline(data=medians, aes(xintercept=mean(medians$PE30_med, na.rm=T)), color="black", linetype="dashed", size=1) + 
  ylab("Probability density (%)") + xlab("30-day Protective Efficacy (%)")


prop.table(table(medians$PE30_low>determ_true))

###power to detect a protective efficacy that is significantly higher than a given threshold.

power_threshold<-data.frame(threshold=seq(from=0, to=1,by=0.01))
power_threshold$power<-NA
for (row in 1:nrow(power_threshold)){
  power_threshold$power[row]<-(length(which(medians$PE30_low> power_threshold$threshold[row])))/N_sims
}

ggplot(data=power_threshold) +geom_line(aes(x=threshold,y=power))



if (control==0) {
  write.csv(iter_combined,paste0("saved_dfs/1strain_model/iter_combined_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                  "_N",N0_treat0,"_",followup,"d_ltf",ltf,".csv"))
  
  write.csv(power_threshold,paste0("saved_dfs/1strain_model/power_threshold_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                 "_N",N0_treat0,"_",followup,"d_ltf",ltf,".csv"))
  
  write.csv(medians,paste0("saved_dfs/1strain_model/medians_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                 "_N",N0_treat0,"_",followup,"d_ltf",ltf,".csv"))
}

if (control==1) {
  write.csv(iter_combined,paste0("saved_dfs/1strain_model/iter_combined_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                 "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,".csv"))
  write.csv(power_threshold,paste0("saved_dfs/1strain_model/power_threshold_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                 "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,".csv"))
  write.csv(medians,paste0("saved_dfs/1strain_model/medians_",mean_protect,
                                 "days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                 "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf,".csv"))
  
}
  

if (control==0) {
 return (paste0("Finished Scenario ", mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                      "_N",N0_treat0,"_",followup,"d_ltf",ltf))
  }


if (control==1) {
  return (paste0("Finished Scenario ", mean_protect,"days_",ippy_av,"ippy_prev",prevalence,"_seas_", seasonality,
                                      "_N",N0_treat0,"_",N0_control0, AS_or_placebo,"_", followup,"d_ltf",ltf))
  }

}

