


####read in for each scenario

### a) all posterior draws for all 1000 simulations (1000sims x 4chains x 10,000iters = 40,000,000 total values) --> iter_combined_SCENARIO
### b) medians of posterior for each of 1000 simulations --> medians_SCENARIO

### to include details on which files contain the processing to get these csv files.


iter_combined_baseline<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/iter_combined_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/medians_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_seas_start<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")
medians_seas_start<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")

iter_combined_seas_end<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")
medians_seas_end<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")

### add a label for each scenario to the dataframe

iter_combined_baseline$scenario<-"Incidence = 10ippy, constant risk"
iter_combined_red_inc_5ippy$scenario<-"Incidence = 5ippy, constant risk"
iter_combined_seas_start$scenario<- "Seasonality - start of season"
iter_combined_seas_end$scenario<-"Seasonality - end of season"

medians_baseline$scenario<-"Incidence = 10ippy, constant risk"
medians_red_inc_5ippy$scenario<-"Incidence = 5ippy, constant risk"
medians_seas_start$scenario<- "Seasonality - start of season"
medians_seas_end$scenario<-"Seasonality - end of season"


###combine scenarios in a single dataframe for all posterior values and for medians
 
iter_combined_chars<-bind_rows(list(iter_combined_baseline,
                                    iter_combined_red_inc_5ippy,
                                    iter_combined_seas_start,
                                    iter_combined_seas_end))

iter_combined_chars$scenario<-factor(iter_combined_chars$scenario,levels = c("Incidence = 10ippy, constant risk",
                                                                             "Incidence = 5ippy, constant risk",
                                                                             "Seasonality - start of season",
                                                                             "Seasonality - end of season"))

medians_chars<-bind_rows(list(medians_baseline,
                              medians_red_inc_5ippy,
                              medians_seas_start,
                              medians_seas_end))

medians_chars$scenario<-factor(medians_chars$scenario,levels = c("Incidence = 10ippy, constant risk",
                                                                 "Incidence = 5ippy, constant risk",
                                                                 "Seasonality - start of season",
                                                                 "Seasonality - end of season"))

medians_chars$PE30_med <- medians_chars$PE30_med*100  ### convert proportions to a %
iter_combined_chars$d30PE<-iter_combined_chars$d30PE*100

### these  produce mean values by scenario (faint dashed lines shown on the probability density plots)
medians_chars$mean_PE30_med <- ave(medians_chars$PE30_med, medians_chars$scenario)   
medians_chars$mean_mean_dur_prot_med <- ave(medians_chars$mean_dur_prot_med, medians_chars$scenario)   


panelA<-ggplot() +
  ylab("Probability density")+ xlab("30-day Protective Efficacy (%)")+
  stat_density_ridges(data=medians_chars, 
                      aes(x = PE30_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                      quantile_lines = TRUE, quantiles = 2, alpha=0.8,bandwidth = 1) +
 geom_density_ridges(data=iter_combined_chars, 
                      aes(x = d30PE, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                          alpha=0.2,bandwidth = 1) +
  # dotted line that shows the deterministic values for 30-day PE in seasonal settings:
  ## highlights true efficacy is overestimated/underestimated when fitting a constant risk of infection to data where transmission increases or decreases during followu
  geom_vline(xintercept=52,
             color="#7CAE00", size=1.5,linetype="longdash")+ ### START OF SEASON deterministic
  geom_vline(xintercept=58.3,
             color="#F8766D", size=1.5,linetype="longdash")+ ### END OF SEASON deterministic
  geom_vline(xintercept=61.1,
             color= "#00BFC4", size=1.5,linetype="longdash")+ ### 5ippy deterministic
  geom_vline(xintercept=56.4,
             color="#C77CFF", size=1.5,linetype="longdash")+ ### baseline deterministic
  
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.y=element_text(size=15,vjust =0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))



panelB<-ggplot() +
  stat_density_ridges(data=medians_chars, 
                      aes(x = mean_dur_prot_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))),
                      quantile_lines = TRUE, quantiles = 2, alpha=0.8,bandwidth = 0.3) +xlim(0,40)+
  
  geom_density_ridges(data=iter_combined_chars, 
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


Fig2<-plot_grid(panelA,panelB, labels = c('A', 'B', 'C', 'D'),label_size = 25, nrow=2)

pdf(file = "Fig2.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 12) # The height of the plot in inches
Fig2
dev.off()
