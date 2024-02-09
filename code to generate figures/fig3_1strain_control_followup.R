

iter_combined_baseline<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_add_AS<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")
medians_add_AS<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")

iter_combined_red_followup28<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")
medians_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")

iter_combined_add_AS_red_followup28<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_28d_ltf0.1.csv")
medians_add_AS_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_28d_ltf0.1.csv")


iter_combined_baseline$scenario<-"Chemoprevention arm (N=600) only, \n 63 days follow-up)" 
iter_combined_add_AS$scenario<-"Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 63 days follow-up"
iter_combined_red_followup28$scenario<- "Chemoprevention arm (N=600) only, \n 28 days follow-up)" 
iter_combined_add_AS_red_followup28$scenario<-"Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 28 days follow-up"



medians_baseline$scenario<-"Chemoprevention arm (N=600) only, \n 63 days follow-up)" 
medians_add_AS$scenario<-"Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 63 days follow-up"
medians_red_followup28$scenario<-  "Chemoprevention arm (N=600) only, \n 28 days follow-up)" 
medians_add_AS_red_followup28$scenario<-"Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 28 days follow-up"


iter_combined_design<-bind_rows(list(iter_combined_baseline,
                                     iter_combined_add_AS,
                                     iter_combined_red_followup28,
                                     iter_combined_add_AS_red_followup28))

iter_combined_design$scenario<-factor(iter_combined_design$scenario,levels = c("Chemoprevention arm (N=600) only, \n 63 days follow-up)" ,
                                                                               "Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 63 days follow-up",
                                                                               "Chemoprevention arm (N=600) only, \n 28 days follow-up)" ,
                                                                               "Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 28 days follow-up"))

medians_design<-bind_rows(list(medians_baseline,
                               medians_add_AS,
                               medians_red_followup28,
                               medians_add_AS_red_followup28))

medians_design$scenario<-factor(medians_design$scenario,levels = c("Chemoprevention arm (N=600) only, \n 63 days follow-up)",
                                                                   "Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 63 days follow-up",
                                                                   "Chemoprevention arm (N=600) only, \n 28 days follow-up)",
                                                                   "Chemoprevention arm (N=600) + short-acting control arm (N=200) \n 28 days follow-up"))

medians_design$PE30_med <- medians_design$PE30_med*100  ### as a %
iter_combined_design$d30PE<-iter_combined_design$d30PE*100

medians_design$mean_PE30_med <- ave(medians_design$PE30_med, medians_design$scenario)   
medians_design$mean_mean_dur_prot_med <- ave(medians_design$mean_dur_prot_med, medians_design$scenario)   


panelA<-ggplot() +
  ylab("Probability density")+ xlab("30-day Protective Efficacy (%)")+
  stat_density_ridges(data=medians_design, 
                      aes(x = PE30_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                      quantile_lines = TRUE, quantiles = 2, alpha=0.8,bandwidth = 1) +
  geom_density_ridges(data=iter_combined_design, 
                      aes(x = d30PE, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                      alpha=0.2,bandwidth = 1) +
  geom_vline(xintercept=56.4,
             color="black", size=1.5,linetype="longdash")+ ###  deterministic (20 days)
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust =0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))



panelB<-ggplot() +
  stat_density_ridges(data=medians_design, 
                      aes(x = mean_dur_prot_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                      quantile_lines = TRUE, quantiles = 2, alpha=0.8,bandwidth = 0.3) +xlim(0,40)+
  
  geom_density_ridges(data=iter_combined_design, 
                      aes(x = mean_dur_prot , y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.2,bandwidth = 0.3) + xlim(0,40)+
  geom_vline(xintercept=20,
             color="black", size=1.5,linetype="longdash")+ ###  deterministic (20 days)
  theme_ridges() + 
  ylab("Probability density")+ xlab("Mean duration of protection (days)")+
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust = 0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))



Fig3<-plot_grid(panelA,panelB, labels = c('A', 'B'),label_size = 20, nrow=2)
pdf(file = "Fig3.pdf",   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 15) # The height of the plot in inches
Fig3
dev.off()
