

iter_combined_baseline<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/2strain_model/medians_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_freq0.9<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.9_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_freq0.9<-read.csv("saved_dfs/2strain_model/medians_freq0.9_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_freq0.2<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.2_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_freq0.2<-read.csv("saved_dfs/2strain_model/medians_freq0.2_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_20daysR<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.5_R20days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_20daysR<-read.csv("saved_dfs/2strain_model/medians_freq0.5_R20days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")



iter_combined_baseline$scenario<-"Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)"
iter_combined_freq0.9$scenario<-"\n 90% frequency of resistant strain \n"
iter_combined_freq0.2$scenario<-"\n 20% frequency of resistant strain \n"
iter_combined_20daysR$scenario<-"\n effect size = 10 days difference \n"
  

medians_baseline$scenario<-"Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)"
medians_freq0.9$scenario<-"\n 90% frequency of resistant strain \n"
medians_freq0.2$scenario<-"\n 20% frequency of resistant strain \n"
medians_20daysR$scenario<-"\n effect size = 10 days difference \n"

iter_combined<-bind_rows(list(iter_combined_baseline, iter_combined_freq0.9, iter_combined_freq0.2, iter_combined_20daysR))

iter_combined$scenario<-factor(iter_combined$scenario,levels = c("Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)",
                                                                 "\n 90% frequency of resistant strain \n",
                                                                 "\n 20% frequency of resistant strain \n",
                                                                 "\n effect size = 10 days difference \n"))

medians<-bind_rows(list(medians_baseline,medians_freq0.9,medians_freq0.2,medians_20daysR))

medians$scenario<-factor(medians$scenario,levels = c("Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)",
                                                     "\n 90% frequency of resistant strain \n",
                                                     "\n 20% frequency of resistant strain \n",
                                                     "\n effect size = 10 days difference \n"))

medians$PE30_R_med<-medians$PE30_R_med*100
medians$PE30_S_med<-medians$PE30_S_med*100
iter_combined$d30PE_R<-iter_combined$d30PE_R*100
iter_combined$d30PE_S<-iter_combined$d30PE_S*100

medians$mean_PE30_med_R <- ave(medians$PE30_R_med, medians$scenario)   
medians$mean_mean_dur_prot_med_R <- ave(medians$mean_dur_prot_R_med, medians$scenario)   

medians$mean_PE30_med_S <- ave(medians$PE30_S_med, medians$scenario)   
medians$mean_mean_dur_prot_med_S <- ave(medians$mean_dur_prot_S_med, medians$scenario)   

plotA<-ggplot() +
  ylab("Probability density")+ xlab("30-day Protective Efficacy(%)")+
  geom_density_ridges(data=iter_combined, 
                      aes(x = d30PE_R, y = fct_rev(as_factor(scenario))), 
                      alpha=0.2,bandwidth = 2, fill="#D82632")+ xlim(0,100)+
  geom_density_ridges(data=medians,
                      aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))), 
                      alpha=0.8,bandwidth = 2, fill="#D82632")+ xlim(0,100)+
  geom_density_ridges(data=iter_combined, 
                      aes(x = d30PE_S, y = fct_rev(as_factor(scenario))), 
                      alpha=0.2,bandwidth = 2, fill="#290AD8") + xlim(0,100)+
  geom_density_ridges(data=medians,
                      aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))), 
                      alpha=0.8,bandwidth = 2, fill="#290AD8")+ xlim(0,100)+
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust =0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

plotB<-ggplot() +
  ylab("Probability density")+ xlab("Mean duration of protection (days)")+
  geom_density_ridges(data=iter_combined, 
                      aes(x = mean_dur_prot_R, y = fct_rev(as_factor(scenario))), 
                      alpha=0.2,bandwidth = 0.6, fill="#D82632")+ xlim(0,60)+
  geom_density_ridges(data=medians,
                      aes(x = mean_dur_prot_R_med, y = fct_rev(as_factor(scenario))), 
                      alpha=0.8,bandwidth = 0.6, fill="#D82632")+xlim(0,60)+
  geom_density_ridges(data=iter_combined, 
                      aes(x = mean_dur_prot_S, y = fct_rev(as_factor(scenario))), 
                      alpha=0.2,bandwidth = 0.6, fill="#290AD8") +xlim(0,60)+
  geom_density_ridges(data=medians,
                      aes(x = mean_dur_prot_S_med, y = fct_rev(as_factor(scenario))), 
                      alpha=0.8,bandwidth = 0.6, fill="#290AD8")+xlim(0,60)+
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust =0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))



Fig4<-plot_grid(plot_grid(plotA, plotB,labels = c('A', 'B'), label_size=20,nrow = 2 ))

pdf(file = "Fig4.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
Fig4
dev.off()

