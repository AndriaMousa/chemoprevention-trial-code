

iter_combined_baseline<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/iter_combined_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/medians_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_red_inc_5ippy_red_prev0.3<-read.csv("saved_dfs/1strain_model/iter_combined_20days_5ippy_prev0.3_seas_none_N600_63d_ltf0.1.csv")
medians_red_inc_5ippy_red_prev0.3<-read.csv("saved_dfs/1strain_model/medians_20days_5ippy_prev0.3_seas_none_N600_63d_ltf0.1.csv")

iter_combined_inc_ltf0.2<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.2.csv")
medians_inc_ltf0.2<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.2.csv")

iter_combined_inc_dur25<-read.csv("saved_dfs/1strain_model/iter_combined_25days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_inc_dur25<-read.csv("saved_dfs/1strain_model/medians_25days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_red_dur15<-read.csv("saved_dfs/1strain_model/iter_combined_15days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_dur15<-read.csv("saved_dfs/1strain_model/medians_15days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")

iter_combined_seas_start<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")
medians_seas_start<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")

iter_combined_seas_end<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")
medians_seas_end<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")

iter_combined_baseline$scenario<-"Baseline"
iter_combined_red_inc_5ippy$scenario<-"Reduction in incidence to 5ippy"
iter_combined_red_inc_5ippy_red_prev0.3$scenario<-"Reduction in incidence to 5ippy and prevalence to 30%"
iter_combined_inc_ltf0.2$scenario<-"Increase in LTF to 20%"
iter_combined_red_dur15$scenario<-"Reduction in mean duration of protection to 15 days"
iter_combined_inc_dur25$scenario<- "Increase in mean duration of protection to 25 days"
iter_combined_seas_start$scenario<- "Seasonality - start of season"
iter_combined_seas_end$scenario<-"Seasonality - end of season"



medians_baseline$scenario<-"Baseline"
medians_red_inc_5ippy$scenario<-"Reduction in incidence to 5ippy"
medians_red_inc_5ippy_red_prev0.3$scenario<-"Reduction in incidence to 5ippy and prevalence to 30%"
medians_inc_ltf0.2$scenario<-"Increase in LTF to 20%"
medians_red_dur15$scenario<-"Reduction in mean duration of protection to 15 days"
medians_inc_dur25$scenario<- "Increase in mean duration of protection to 25 days"
medians_seas_start$scenario<- "Seasonality - start of season"
medians_seas_end$scenario<-"Seasonality - end of season"

iter_combined_chars<-bind_rows(list(iter_combined_baseline,
                                          iter_combined_red_inc_5ippy,
                                          iter_combined_red_inc_5ippy_red_prev0.3,
                                          iter_combined_inc_ltf0.2,
                                          iter_combined_inc_dur25,
                                          iter_combined_red_dur15,
                                          iter_combined_seas_start,
                                          iter_combined_seas_end))

iter_combined_chars$scenario<-factor(iter_combined_chars$scenario,levels = c("Baseline",
                                                                                         "Reduction in incidence to 5ippy",
                                                                                         "Reduction in incidence to 5ippy and prevalence to 30%",
                                                                                         "Increase in LTF to 20%",
                                                                                         "Reduction in mean duration of protection to 15 days",
                                                                                         "Increase in mean duration of protection to 25 days",
                                                                                         "Seasonality - start of season",
                                                                                         "Seasonality - end of season"))

medians_chars<-bind_rows(list(medians_baseline,
                                    medians_red_inc_5ippy,
                                    medians_red_inc_5ippy_red_prev0.3,
                                    medians_inc_ltf0.2,
                                    medians_inc_dur25,
                                    medians_red_dur15,
                                    medians_seas_start,
                                    medians_seas_end))

medians_chars$scenario<-factor(medians_chars$scenario,levels = c("Baseline",
                                                                             "Reduction in incidence to 5ippy",
                                                                             "Reduction in incidence to 5ippy and prevalence to 30%",
                                                                             "Increase in LTF to 20%",
                                                                             "Reduction in mean duration of protection to 15 days",
                                                                             "Increase in mean duration of protection to 25 days",
                                                                             "Seasonality - start of season",
                                                                             "Seasonality - end of season"))


medians_chars$mean_PE30_med <- ave(medians_chars$PE30_med, medians_chars$scenario)   
medians_chars$mean_mean_dur_prot_med <- ave(medians_chars$mean_dur_prot_med, medians_chars$scenario)   


ggplot() +
  geom_density_ridges(data=iter_combined_chars, 
                      aes(x = d30PE, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.2,bandwidth = 0.01) +
  geom_density_ridges(data=medians_chars, 
                      aes(x = PE30_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.8,bandwidth = 0.01) +
  geom_vline(data=medians_chars, 
             aes(xintercept=mean_PE30_med , 
                 color=fct_rev(as_factor(scenario))), 
                 linetype="dashed") + 
  theme_ridges() + ggtitle("30-day Protective Efficacy")+
  ylab("Probability density")+ xlab("30-day Protective Efficacy")+
  theme(legend.position = "none") 


ggplot() +
  geom_density_ridges(data=iter_combined_chars, 
                      aes(x = mean_dur_prot , y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.2,bandwidth = 0.3) + xlim(0,40)+
  geom_density_ridges(data=medians_chars,
                      aes(x = mean_dur_prot_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.8,bandwidth = 0.3) +xlim(0,40)+
  geom_vline(data=medians_chars, 
             aes(xintercept=mean_mean_dur_prot_med , 
                 color=fct_rev(as_factor(scenario))), 
             linetype="dashed") + 
  theme_ridges() + ggtitle("Mean duration of protection")+
  ylab("Probability density")+ xlab("Mean duration of protection (days)")+
  theme(legend.position = "none") 

####study design


iter_combined_red_sample400<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N400_63d_ltf0.1.csv")
medians_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_63d_ltf0.1.csv")

iter_combined_red_followup42<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_42d_ltf0.1.csv")
medians_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_42d_ltf0.1.csv")

iter_combined_red_followup28<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")
medians_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")

iter_combined_add_AS<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")
medians_add_AS<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")

iter_combined_add_placebo<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200placebo_63d_ltf0.1.csv")
medians_add_placebo<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200placebo_63d_ltf0.1.csv")

iter_combined_add_AS_red_sample400<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N400_200AS_63d_ltf0.1.csv")
medians_add_AS_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200AS_63d_ltf0.1.csv")

iter_combined_add_placebo_red_sample400<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N400_200placebo_63d_ltf0.1.csv")
medians_add_placebo_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200placebo_63d_ltf0.1.csv")

iter_combined_add_AS_red_followup42<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N600_200AS_42d_ltf0.1.csv")
medians_add_AS_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_42d_ltf0.1.csv")

iter_combined_add_AS_red_sample400_red_followup42<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_none_N400_200AS_42d_ltf0.1.csv")
medians_add_AS_red_sample400_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200AS_42d_ltf0.1.csv")

iter_combined_add_AS_seas_start<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_start_N600_200AS_63d_ltf0.1.csv")
medians_add_AS_seas_start<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_start_N600_200AS_63d_ltf0.1.csv")

iter_combined_add_AS_seas_end<-read.csv("saved_dfs/1strain_model/iter_combined_20days_10ippy_prev0.4_seas_end_N600_200AS_63d_ltf0.1.csv")
medians_add_AS_seas_end<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_end_N600_200AS_63d_ltf0.1.csv")

iter_combined_red_sample400$scenario<-"Reduction in sample size to 400"
iter_combined_red_followup42$scenario<-"Reduction of follow-up to 42 days"
iter_combined_red_followup28$scenario<-"Reduction of follow-up to 28 days"
iter_combined_add_AS$scenario<-"Addition of short-acting control group"
iter_combined_add_placebo$scenario<-"Addition of untreated control group"
iter_combined_add_AS_red_sample400$scenario<-"Addition of short-acting control group and reduction in sample size to 400"
iter_combined_add_placebo_red_sample400$scenario<-"Addition of untreated control group and reduction in sample size to 400"
iter_combined_add_AS_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days"
iter_combined_add_AS_red_sample400_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400"
iter_combined_add_AS_seas_start$scenario<-"Addition of short-acting control group, Seasonality - start of season"
iter_combined_add_AS_seas_end$scenario<-"Addition of short-acting control group, Seasonality - end of season"

medians_red_sample400$scenario<-"Reduction in sample size to 400"
medians_red_followup42$scenario<-"Reduction of follow-up to 42 days"
medians_red_followup28$scenario<-"Reduction of follow-up to 28 days"
medians_add_AS$scenario<-"Addition of short-acting control group"
medians_add_placebo$scenario<-"Addition of untreated control group"
medians_add_AS_red_sample400$scenario<-"Addition of short-acting control group and reduction in sample size to 400"
medians_add_placebo_red_sample400$scenario<-"Addition of untreated control group and reduction in sample size to 400"
medians_add_AS_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days"
medians_add_AS_red_sample400_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400"
medians_add_AS_seas_start$scenario<-"Addition of short-acting control group, Seasonality - start of season"
medians_add_AS_seas_end$scenario<-"Addition of short-acting control group, Seasonality - end of season"


iter_combined_design<-bind_rows(list(iter_combined_baseline,
                                       iter_combined_red_sample400,
                                       iter_combined_red_followup42,
                                       iter_combined_red_followup28,
                                       iter_combined_add_AS,
                                       iter_combined_add_placebo,
                                       iter_combined_add_AS_red_sample400,
                                       iter_combined_add_placebo_red_sample400,
                                       iter_combined_add_AS_red_followup42,
                                       iter_combined_add_AS_red_sample400_red_followup42,
                                       iter_combined_add_AS_seas_start,
                                       iter_combined_add_AS_seas_end))

iter_combined_design$scenario <- factor(iter_combined_design$scenario, levels = c("Baseline", 
                                                                                      "Reduction in sample size to 400",
                                                                                      "Reduction of follow-up to 42 days",
                                                                                      "Reduction of follow-up to 28 days",
                                                                                      "Addition of short-acting control group",
                                                                                      "Addition of untreated control group",
                                                                                      "Addition of short-acting control group and reduction in sample size to 400",
                                                                                      "Addition of untreated control group and reduction in sample size to 400",
                                                                                      "Addition of short-acting control group, reduction of follow-up to 42 days",
                                                                                      "Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400",
                                                                                      "Addition of short-acting control group, Seasonality - start of season",
                                                                                      "Addition of short-acting control group, Seasonality - end of season"))

medians_design<-bind_rows(list(medians_baseline,
                                     medians_red_sample400,
                                     medians_red_followup42,
                                     medians_red_followup28,
                                     medians_add_AS,
                                     medians_add_placebo,
                                     medians_add_AS_red_sample400,
                                     medians_add_placebo_red_sample400,
                                     medians_add_AS_red_followup42,
                                     medians_add_AS_red_sample400_red_followup42,
                                     medians_add_AS_seas_start,
                                     medians_add_AS_seas_end))

medians_design$scenario <- factor(medians_design$scenario, levels = c("Baseline", 
                                                                                  "Reduction in sample size to 400",
                                                                                  "Reduction of follow-up to 42 days",
                                                                                  "Reduction of follow-up to 28 days",
                                                                                  "Addition of short-acting control group",
                                                                                  "Addition of untreated control group",
                                                                                  "Addition of short-acting control group and reduction in sample size to 400",
                                                                                  "Addition of untreated control group and reduction in sample size to 400",
                                                                                  "Addition of short-acting control group, reduction of follow-up to 42 days",
                                                                                  "Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400",
                                                                                  "Addition of short-acting control group, Seasonality - start of season",
                                                                                  "Addition of short-acting control group, Seasonality - end of season"))




medians_design$mean_PE30_med <- ave(medians_design$PE30_med, medians_design$scenario)   
medians_design$mean_mean_dur_prot_med <- ave(medians_design$mean_dur_prot_med, medians_design$scenario)   


ggplot() +
  geom_density_ridges(data=iter_combined_design, 
                      aes(x = d30PE, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.2,bandwidth = 0.01) +
  geom_density_ridges(data=medians_design, 
                      aes(x = PE30_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.8,bandwidth = 0.01) +
  geom_vline(data=medians_design, 
             aes(xintercept=mean_PE30_med , 
                 color=fct_rev(as_factor(scenario))), 
             linetype="dashed") + 
  theme_ridges() + ggtitle("30-day Protective Efficacy")+
  ylab("Probability density")+ xlab("30-day Protective Efficacy")+
  theme(legend.position = "none") 


ggplot() +
  geom_density_ridges(data=iter_combined_design, 
                      aes(x = mean_dur_prot , y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.2,bandwidth = 0.3) + xlim(0,40)+
  geom_density_ridges(data=medians_design,
                      aes(x = mean_dur_prot_med, y = fct_rev(as_factor(scenario)),
                          fill = fct_rev(as_factor(scenario))), alpha=0.8,bandwidth = 0.3) +xlim(0,40)+
  geom_vline(data=medians_design, 
             aes(xintercept=mean_mean_dur_prot_med , 
                 color=fct_rev(as_factor(scenario))), 
             linetype="dashed") + 
  theme_ridges() + ggtitle("Mean duration of protection")+
  ylab("Probability density")+ xlab("Mean duration of protection (days)")+
  theme(legend.position = "none") 


