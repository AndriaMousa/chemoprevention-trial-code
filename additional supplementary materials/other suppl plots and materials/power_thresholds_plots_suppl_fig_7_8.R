


N_sims<-1000
###setting characteristics

###OUTCOME: 30-DAY PROTECTIVE EFFICACY

power_threshold_baseline<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
power_threshold_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/power_threshold_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
power_threshold_red_inc_5ippy_red_prev0.3<-read.csv("saved_dfs/1strain_model/power_threshold_20days_5ippy_prev0.3_seas_none_N600_63d_ltf0.1.csv")
power_threshold_inc_ltf0.2<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.2.csv")
power_threshold_inc_dur25<-read.csv("saved_dfs/1strain_model/power_threshold_25days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
power_threshold_red_dur15<-read.csv("saved_dfs/1strain_model/power_threshold_15days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
power_threshold_seas_start<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")
power_threshold_seas_end<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")


power_threshold_baseline$scenario<-"Baseline"
power_threshold_red_inc_5ippy$scenario<-"Reduction in incidence to 5ippy"
power_threshold_red_inc_5ippy_red_prev0.3$scenario<-"Reduction in incidence to 5ippy and prevalence to 30%"
power_threshold_inc_ltf0.2$scenario<-"Increase in LTF to 20%"
power_threshold_red_dur15$scenario<-"Reduction in mean duration of protection to 15 days"
power_threshold_inc_dur25$scenario<- "Increase in mean duration of protection to 25 days"
power_threshold_seas_start$scenario<- "Seasonality - start of season "
power_threshold_seas_end$scenario<-"Seasonality - end of season"



power_threshold_chars<-bind_rows(list(power_threshold_baseline,
                                      power_threshold_red_inc_5ippy,
                                      power_threshold_red_inc_5ippy_red_prev0.3,
                                      power_threshold_inc_ltf0.2,
                                      power_threshold_inc_dur25,
                                      power_threshold_red_dur15,
                                      power_threshold_seas_start,
                                      power_threshold_seas_end))

power_threshold_chars$scenario<-factor(power_threshold_chars$scenario,levels = c("Baseline",
                                                                                 "Reduction in incidence to 5ippy",
                                                                                 "Reduction in incidence to 5ippy and prevalence to 30%",
                                                                                 "Increase in LTF to 20%",
                                                                                 "Reduction in mean duration of protection to 15 days",
                                                                                 "Increase in mean duration of protection to 25 days",
                                                                                 "Seasonality - start of season ",
                                                                                 "Seasonality - end of season"))

ggplot() + theme_bw() + 
  geom_line(data=power_threshold_chars, aes(x=threshold,y=power, colour=scenario), size=1.2)+
  geom_line(data=power_threshold_chars[which(power_threshold_chars$scenario=="Baseline"),], 
            aes(x=threshold,y=power), size=1.2, colour="#F8766D") +
  ylab("Power to detect a 30 day PE > threshold") + xlab("30-day protective efficacy threshold") +xlim(0.2,0.8)+
  scale_color_discrete(name = "Scenario")


######## OUTCOME: MEAN DURATION OF PROTECTION

medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_inc_5ippy<-read.csv("saved_dfs/1strain_model/medians_20days_5ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_inc_5ippy_red_prev0.3<-read.csv("saved_dfs/1strain_model/medians_20days_5ippy_prev0.3_seas_none_N600_63d_ltf0.1.csv")
medians_inc_ltf0.2<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.2.csv")
medians_inc_dur25<-read.csv("saved_dfs/1strain_model/medians_25days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_dur15<-read.csv("saved_dfs/1strain_model/medians_15days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_seas_start<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_start_N600_63d_ltf0.1.csv")
medians_seas_end<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_end_N600_63d_ltf0.1.csv")


medians_baseline$scenario<-"Baseline"
medians_red_inc_5ippy$scenario<-"Reduction in incidence to 5ippy"
medians_red_inc_5ippy_red_prev0.3$scenario<-"Reduction in incidence to 5ippy and prevalence to 30%"
medians_inc_ltf0.2$scenario<-"Increase in LTF to 20%"
medians_red_dur15$scenario<-"Reduction in mean duration of protection to 15 days"
medians_inc_dur25$scenario<- "Increase in mean duration of protection to 25 days"
medians_seas_start$scenario<- "Seasonality - start of season"
medians_seas_end$scenario<-"Seasonality - end of season"


power_threshold_dur_baseline<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_baseline$power<-NA

power_threshold_dur_red_inc_5ippy<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_inc_5ippy$power<-NA

power_threshold_dur_red_inc_5ippy_red_prev0.3<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_inc_5ippy_red_prev0.3$power<-NA

power_threshold_dur_inc_ltf0.2<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_inc_ltf0.2$power<-NA

power_threshold_dur_red_dur15<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_dur15$power<-NA

power_threshold_dur_inc_dur25<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_inc_dur25$power<-NA

power_threshold_dur_seas_start<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_seas_start$power<-NA

power_threshold_dur_seas_end<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_seas_end$power<-NA

for (row in 1:nrow(power_threshold_dur_baseline)){
  power_threshold_dur_baseline$power[row]<-(length(which(medians_baseline$mean_dur_prot_low> power_threshold_dur_baseline$threshold[row])))/N_sims
  power_threshold_dur_red_inc_5ippy$power[row]<-(length(which(medians_red_inc_5ippy$mean_dur_prot_low> power_threshold_dur_red_inc_5ippy$threshold[row])))/N_sims
  power_threshold_dur_red_inc_5ippy_red_prev0.3$power[row]<-(length(which(medians_red_inc_5ippy_red_prev0.3$mean_dur_prot_low> power_threshold_dur_red_inc_5ippy_red_prev0.3$threshold[row])))/N_sims
  power_threshold_dur_inc_ltf0.2$power[row]<-(length(which(medians_inc_ltf0.2$mean_dur_prot_low> power_threshold_dur_inc_ltf0.2$threshold[row])))/N_sims
  power_threshold_dur_red_dur15$power[row]<-(length(which(medians_red_dur15$mean_dur_prot_low> power_threshold_dur_red_dur15$threshold[row])))/N_sims
  power_threshold_dur_inc_dur25$power[row]<-(length(which(medians_inc_dur25$mean_dur_prot_low> power_threshold_dur_inc_dur25$threshold[row])))/N_sims
  power_threshold_dur_seas_start$power[row]<-(length(which(medians_seas_start$mean_dur_prot_low> power_threshold_dur_seas_start$threshold[row])))/N_sims
  power_threshold_dur_seas_end$power[row]<-(length(which(medians_seas_end$mean_dur_prot_low> power_threshold_dur_seas_end$threshold[row])))/N_sims
  
}

power_threshold_dur_baseline$scenario<-"Baseline"
power_threshold_dur_red_inc_5ippy$scenario<-"Reduction in incidence to 5ippy"
power_threshold_dur_red_inc_5ippy_red_prev0.3$scenario<-"Reduction in incidence to 5ippy and prevalence to 30%"
power_threshold_dur_inc_ltf0.2$scenario<-"Increase in LTF to 20%"
power_threshold_dur_red_dur15$scenario<-"Reduction in mean duration of protection to 15 days"
power_threshold_dur_inc_dur25$scenario<- "Increase in mean duration of protection to 25 days"
power_threshold_dur_seas_start$scenario<- "Seasonality - start of season"
power_threshold_dur_seas_end$scenario<-"Seasonality - end of season"


power_threshold_dur_chars<-bind_rows(list(power_threshold_dur_baseline,
                                         power_threshold_dur_red_inc_5ippy,
                                         power_threshold_dur_red_inc_5ippy_red_prev0.3,
                                         power_threshold_dur_inc_ltf0.2,
                                         power_threshold_dur_inc_dur25,
                                         power_threshold_dur_red_dur15,
                                         power_threshold_dur_seas_start,
                                         power_threshold_dur_seas_end))

power_threshold_dur_chars$scenario<-factor(power_threshold_dur_chars$scenario,levels = c("Baseline",
                                                                                 "Reduction in incidence to 5ippy",
                                                                                 "Reduction in incidence to 5ippy and prevalence to 30%",
                                                                                 "Increase in LTF to 20%",
                                                                                 "Reduction in mean duration of protection to 15 days",
                                                                                 "Increase in mean duration of protection to 25 days",
                                                                                 "Seasonality - start of season",
                                                                                 "Seasonality - end of season"))

ggplot() + theme_bw() + 
  geom_line(data=power_threshold_dur_chars, aes(x=threshold,y=power, colour=scenario), size=1.2) +
  ylab("Power to detect a mean duration of protection > threshold") + xlab("Mean duration of protection (days) threshold") +
  scale_color_discrete(name = "Scenario")
  

 ##study design

###OUTCOME: 30-DAY PROTECTIVE EFFICACY

power_threshold_red_sample400<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N400_63d_ltf0.1.csv")
power_threshold_red_followup42<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_42d_ltf0.1.csv")
power_threshold_red_followup28<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")
power_threshold_add_AS<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")
power_threshold_add_placebo<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_200placebo_63d_ltf0.1.csv")
power_threshold_add_AS_red_sample400<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N400_200AS_63d_ltf0.1.csv")
power_threshold_add_placebo_red_sample400<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N400_200placebo_63d_ltf0.1.csv")
power_threshold_add_AS_red_followup42<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N600_200AS_42d_ltf0.1.csv")
power_threshold_add_AS_red_sample400_red_followup42<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_none_N400_200AS_42d_ltf0.1.csv")
power_threshold_add_AS_seas_start<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_start_N600_200AS_63d_ltf0.1.csv")
power_threshold_add_AS_seas_end<-read.csv("saved_dfs/1strain_model/power_threshold_20days_10ippy_prev0.4_seas_end_N600_200AS_63d_ltf0.1.csv")

power_threshold_red_sample400$scenario<-"Reduction in sample size to 400"
power_threshold_red_followup42$scenario<-"Reduction of follow-up to 42 days"
power_threshold_red_followup28$scenario<-"Reduction of follow-up to 28 days"
power_threshold_add_AS$scenario<-"Addition of short-acting control group"
power_threshold_add_placebo$scenario<-"Addition of untreated control group"
power_threshold_add_AS_red_sample400$scenario<-"Addition of short-acting control group and reduction in sample size to 400"
power_threshold_add_placebo_red_sample400$scenario<-"Addition of untreated control group and reduction in sample size to 400"
power_threshold_add_AS_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days"
power_threshold_add_AS_red_sample400_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400"
power_threshold_add_AS_seas_start$scenario<-"Addition of short-acting control group, Seasonality - start of season"
power_threshold_add_AS_seas_end$scenario<-"Addition of short-acting control group, Seasonality - end of season"


power_threshold_design<-bind_rows(list(power_threshold_baseline,
                                       power_threshold_red_sample400,
                                       power_threshold_red_followup42,
                                       power_threshold_red_followup28,
                                       power_threshold_add_AS,
                                       power_threshold_add_placebo,
                                       power_threshold_add_AS_red_sample400,
                                       power_threshold_add_placebo_red_sample400,
                                       power_threshold_add_AS_red_followup42,
                                       power_threshold_add_AS_red_sample400_red_followup42,
                                       power_threshold_add_AS_seas_start,
                                       power_threshold_add_AS_seas_end))

power_threshold_design$scenario <- factor(power_threshold_design$scenario, levels = c("Baseline", 
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


ggplot() + theme_bw() +
  geom_line(data=power_threshold_design, aes(x=threshold,y=power, colour=scenario), size=1.2)+
  ylab("Power to detect a PE > threshold") + xlab("Protective efficacy threshold") +xlim(0.35,0.65)+
  geom_line(data=power_threshold_design[which(power_threshold_design$scenario=="Baseline"),], 
            aes(x=threshold,y=power), size=1.2, colour="#F8766D") +
  scale_color_discrete(name = "Scenario")

######## OUTCOME: MEAN DURATION OF PROTECTION

medians_baseline<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.csv")
medians_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_63d_ltf0.1.csv")
medians_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_42d_ltf0.1.csv")
medians_red_followup28<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_28d_ltf0.1.csv")
medians_add_AS<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_63d_ltf0.1.csv")
medians_add_placebo<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200placebo_63d_ltf0.1.csv")
medians_add_AS_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200AS_63d_ltf0.1.csv")
medians_add_placebo_red_sample400<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200placebo_63d_ltf0.1.csv")
medians_add_AS_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N600_200AS_42d_ltf0.1.csv")
medians_add_AS_red_sample400_red_followup42<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_none_N400_200AS_42d_ltf0.1.csv")
medians_add_AS_seas_start<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_start_N600_200AS_63d_ltf0.1.csv")
medians_add_AS_seas_end<-read.csv("saved_dfs/1strain_model/medians_20days_10ippy_prev0.4_seas_end_N600_200AS_63d_ltf0.1.csv")


medians_baseline$scenario<-"Baseline"
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

power_threshold_dur_red_sample400<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_sample400$power<-NA

power_threshold_dur_red_followup42<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_followup42$power<-NA

power_threshold_dur_red_followup28<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_red_followup28$power<-NA

power_threshold_dur_add_AS<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS$power<-NA

power_threshold_dur_add_placebo<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_placebo$power<-NA

power_threshold_dur_add_AS_red_sample400<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS_red_sample400$power<-NA

power_threshold_dur_add_placebo_red_sample400<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_placebo_red_sample400$power<-NA

power_threshold_dur_add_AS_red_followup42<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS_red_followup42$power<-NA

power_threshold_dur_add_AS_red_sample400_red_followup42<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS_red_sample400_red_followup42$power<-NA

power_threshold_dur_add_AS_seas_start<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS_seas_start$power<-NA

power_threshold_dur_add_AS_seas_end<-data.frame(threshold=seq(from=5, to=30,by=0.05))
power_threshold_dur_add_AS_seas_end$power<-NA


for (row in 1:nrow(power_threshold_dur_baseline)){
  power_threshold_dur_red_sample400$power[row]<-(length(which(medians_red_sample400$mean_dur_prot_low> power_threshold_dur_red_sample400$threshold[row])))/N_sims
  power_threshold_dur_red_followup42$power[row]<-(length(which(medians_red_followup42$mean_dur_prot_low> power_threshold_dur_red_followup42$threshold[row])))/N_sims
  power_threshold_dur_red_followup28$power[row]<-(length(which(medians_red_followup28$mean_dur_prot_low> power_threshold_dur_red_followup28$threshold[row])))/N_sims
  power_threshold_dur_add_AS$power[row]<-(length(which(medians_add_AS$mean_dur_prot_low> power_threshold_dur_add_AS$threshold[row])))/N_sims
  power_threshold_dur_add_placebo$power[row]<-(length(which(medians_add_placebo$mean_dur_prot_low> power_threshold_dur_add_placebo$threshold[row])))/N_sims
  power_threshold_dur_add_AS_red_sample400$power[row]<-(length(which(medians_add_AS_red_sample400$mean_dur_prot_low> power_threshold_dur_add_AS_red_sample400$threshold[row])))/N_sims
  power_threshold_dur_add_placebo_red_sample400$power[row]<-(length(which(medians_add_placebo_red_sample400$mean_dur_prot_low> power_threshold_dur_add_placebo_red_sample400$threshold[row])))/N_sims
  power_threshold_dur_add_AS_red_followup42$power[row]<-(length(which(medians_add_AS_red_followup42$mean_dur_prot_low> power_threshold_dur_add_AS_red_followup42$threshold[row])))/N_sims
  power_threshold_dur_add_AS_red_sample400_red_followup42$power[row]<-(length(which(medians_add_AS_red_sample400_red_followup42$mean_dur_prot_low> power_threshold_dur_add_AS_red_sample400_red_followup42$threshold[row])))/N_sims
  power_threshold_dur_add_AS_seas_start$power[row]<-(length(which(medians_add_AS_seas_start$mean_dur_prot_low> power_threshold_dur_add_AS_seas_start$threshold[row])))/N_sims
  power_threshold_dur_add_AS_seas_end$power[row]<-(length(which(medians_add_AS_seas_end$mean_dur_prot_low> power_threshold_dur_add_AS_seas_end$threshold[row])))/N_sims

}


power_threshold_dur_baseline$scenario<-"Baseline"
power_threshold_dur_red_sample400$scenario<-"Reduction in sample size to 400"
power_threshold_dur_red_followup42$scenario<-"Reduction of follow-up to 42 days"
power_threshold_dur_red_followup28$scenario<-"Reduction of follow-up to 28 days"
power_threshold_dur_add_AS$scenario<-"Addition of short-acting control group"
power_threshold_dur_add_placebo$scenario<-"Addition of untreated control group"
power_threshold_dur_add_AS_red_sample400$scenario<-"Addition of short-acting control group and reduction in sample size to 400"
power_threshold_dur_add_placebo_red_sample400$scenario<-"Addition of untreated control group and reduction in sample size to 400"
power_threshold_dur_add_AS_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days"
power_threshold_dur_add_AS_red_sample400_red_followup42$scenario<-"Addition of short-acting control group, reduction of follow-up to 42 days and reduction in sample size to 400"
power_threshold_dur_add_AS_seas_start$scenario<-"Addition of short-acting control group, Seasonality - start of season"
power_threshold_dur_add_AS_seas_end$scenario<-"Addition of short-acting control group, Seasonality - end of season"

power_threshold_dur_design<-bind_rows(list(power_threshold_dur_baseline,
                                       power_threshold_dur_red_sample400,
                                       power_threshold_dur_red_followup42,
                                       power_threshold_dur_red_followup28,
                                       power_threshold_dur_add_AS,
                                       power_threshold_dur_add_placebo,
                                       power_threshold_dur_add_AS_red_sample400,
                                       power_threshold_dur_add_placebo_red_sample400,
                                       power_threshold_dur_add_AS_red_followup42,
                                       power_threshold_dur_add_AS_red_sample400_red_followup42,
                                       power_threshold_dur_add_AS_seas_start,
                                       power_threshold_dur_add_AS_seas_end))




power_threshold_dur_design$scenario <- factor(power_threshold_dur_design$scenario, levels = c("Baseline", 
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


ggplot() + theme_bw() + 
  geom_line(data=power_threshold_dur_design, aes(x=threshold,y=power, colour=scenario), size=1.2) +
  geom_line(data=power_threshold_dur_design[which(power_threshold_dur_design$scenario=="Baseline"),], 
            aes(x=threshold,y=power), size=1.2, colour="#F8766D") + xlim(12,23)+
  ylab("Power to detect a mean duration of protection > threshold") + 
    xlab("Mean duration of protection (days) threshold") +
  scale_color_discrete(name = "Scenario")

