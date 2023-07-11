

iter_combined_baseline<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_add_AS<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")
medians_add_AS<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")

iter_combined_red_followup28<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")
medians_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")

iter_combined_add_AS_red_followup28<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_28d_ltf0.1.csv")
medians_add_AS_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_28d_ltf0.1.csv")


iter_combined_baseline$scenario<-"Baseline \n (single arm, N=600, 63 days follow-up)"
iter_combined_add_AS$scenario<-"Addition of short-acting drug arm, N=200"
iter_combined_red_followup28$scenario<- "Reduction of follow-up to 28 days"
iter_combined_add_AS_red_followup28$scenario<-"Reduction of follow-up to 28 days \n Addition of short-acting drug arm"



medians_baseline$scenario<-"Baseline \n (single arm, N=600, 63 days follow-up)"
medians_add_AS$scenario<-"Addition of short-acting drug arm, N=200"
medians_red_followup28$scenario<- "Reduction of follow-up to 28 days"
medians_add_AS_red_followup28$scenario<-"Reduction of follow-up to 28 days \n Addition of short-acting drug arm"


iter_combined_design<-bind_rows(list(iter_combined_baseline,
                                     iter_combined_add_AS,
                                     iter_combined_red_followup28,
                                     iter_combined_add_AS_red_followup28))

iter_combined_design$scenario<-factor(iter_combined_design$scenario,levels = c("Baseline \n (single arm, N=600, 63 days follow-up)",
                                                                               "Addition of short-acting drug arm, N=200",
                                                                               "Reduction of follow-up to 28 days",
                                                                               "Reduction of follow-up to 28 days \n Addition of short-acting drug arm"))

medians_design<-bind_rows(list(medians_baseline,
                               medians_add_AS,
                               medians_red_followup28,
                               medians_add_AS_red_followup28))

medians_design$scenario<-factor(medians_design$scenario,levels = c("Baseline \n (single arm, N=600, 63 days follow-up)",
                                                                   "Addition of short-acting drug arm, N=200",
                                                                   "Reduction of follow-up to 28 days",
                                                                   "Reduction of follow-up to 28 days \n Addition of short-acting drug arm"))

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
  theme_ridges() + 
  ylab("Probability density")+ xlab("Mean duration of protection (days)")+
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust = 0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))


###VIOLIN PLOTS


medians_baseline$PE30_width_CrI<-medians_baseline$PE30_high-medians_baseline$PE30_low
medians_add_AS$PE30_width_CrI<-medians_add_AS$PE30_high-medians_add_AS$PE30_low
medians_red_followup28$PE30_width_CrI<-medians_red_followup28$PE30_high-medians_red_followup28$PE30_low
medians_add_AS_red_followup28$PE30_width_CrI<-medians_add_AS_red_followup28$PE30_high-medians_add_AS_red_followup28$PE30_low

medians_baseline$mean_dur_prot_width_CrI<-medians_baseline$mean_dur_prot_high-medians_baseline$mean_dur_prot_low
medians_add_AS$mean_dur_prot_width_CrI<-medians_add_AS$mean_dur_prot_high-medians_add_AS$mean_dur_prot_low
medians_red_followup28$mean_dur_prot_width_CrI<-medians_red_followup28$mean_dur_prot_high-medians_red_followup28$mean_dur_prot_low
medians_add_AS_red_followup28$mean_dur_prot_width_CrI<-medians_add_AS_red_followup28$mean_dur_prot_high-medians_add_AS_red_followup28$mean_dur_prot_low

medians_design<-bind_rows(list(medians_baseline,
                              medians_add_AS,
                              medians_red_followup28,
                              medians_add_AS_red_followup28))


medians_design$scenario <- factor(medians_design$scenario, levels =c("Baseline \n (single arm, N=600, 63 days follow-up)",
                                                                     "Addition of short-acting drug arm, N=200",
                                                                     "Reduction of follow-up to 28 days",
                                                                     "Reduction of follow-up to 28 days \n Addition of short-acting drug arm"))

medians_design$code_col<-1
medians_design$code_col[which(medians_design$scenario== "Baseline")]<-2
medians_design$code_col<-factor(medians_design$code_col)


panelC<-ggplot(medians_design, aes(x=scenario, y=PE30_width_CrI *100)) + 
  theme_bw()+ 
  theme(legend.position = "none",
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"))+
  ylab("Width of 95% Credible interval in 30-day efficacy per simulation (%)") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_design, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=100*median(medians_design$PE30_width_CrI[which(medians_design$scenario== "Baseline \n (single arm, N=600, 63 days follow-up)")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_design[which(medians_design$scenario== "Baseline \n (single arm, N=600, 63 days follow-up)"),],fun=median, geom="point", size=2, color="slateblue")

panelD<-ggplot(medians_design, aes(x=scenario, y=mean_dur_prot_width_CrI)) + 
  theme_bw()+ 
  theme(legend.position = "none",
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"))+
  ylab("Width of 95% Credible interval in mean duration of protection") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_design, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=median(medians_design$mean_dur_prot_width_CrI[which(medians_design$scenario== "Baseline \n (single arm, N=600, 63 days follow-up)")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_design[which(medians_design$scenario== "Baseline \n (single arm, N=600, 63 days follow-up)"),],fun=median, geom="point", size=2, color="slateblue")

Fig3<-plot_grid(panelA,panelB,  panelC, panelD, labels = c('A', 'B', 'C', 'D'),label_size = 25, nrow=2)
pdf(file = "Fig3.pdf",   # The directory you want to save the file in
    width = 22, # The width of the plot in inches
    height = 11) # The height of the plot in inches
Fig3
dev.off()
