

baseline_anyinf<-readRDS("simulated_data/input_df_sim_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.RData")
baseline_clinical<-baseline_anyinf

prob_symptomatic<-0.5 ## probability of becoming symptomatic

for (sim in 1:1000) { 
  
baseline_clinical[[sim]]$N_treated_I_S_new<-round(baseline_clinical[[sim]]$N_treated_I_S_new*prob_symptomatic, digits = 0)
baseline_clinical[[sim]]$N_treated_I_R_new<-round(baseline_clinical[[sim]]$N_treated_I_R_new*prob_symptomatic, digits = 0)
baseline_clinical[[sim]]$N_treated_I_undeterm_new<-round(baseline_clinical[[sim]]$N_treated_I_undeterm_new*prob_symptomatic, digits = 0)


baseline_clinical[[sim]]$N_treated_I_S<-cumsum(baseline_clinical[[sim]]$N_treated_I_S_new)
baseline_clinical[[sim]]$N_treated_I_R<-cumsum(baseline_clinical[[sim]]$N_treated_I_R_new)
baseline_clinical[[sim]]$N_treated_I_undeterm<-cumsum(baseline_clinical[[sim]]$N_treated_I_undeterm_new)

baseline_clinical[[sim]]$N_treated_uninf<-baseline_clinical[[sim]]$N_treated_at_risk[1] -
                                          baseline_clinical[[sim]]$N_treated_I_S -
                                          baseline_clinical[[sim]]$N_treated_I_R -
                                          baseline_clinical[[sim]]$N_treated_I_undeterm

for (r in 2:13) { 
baseline_clinical[[sim]]$N_treated_at_risk[r]<- baseline_clinical[[sim]]$N_treated_uninf[r-1]
}
print(sim)
}

### dataframe with clinical episodes rather than infections

saveRDS(baseline_clinical,"input_df_sim_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1_clinical.RData")
