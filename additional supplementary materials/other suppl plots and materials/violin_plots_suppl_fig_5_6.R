##violin plots


###setting characteristics

medians_baseline$PE30_width_CrI<-medians_baseline$PE30_high-medians_baseline$PE30_low
medians_red_inc_5ippy$PE30_width_CrI<-medians_red_inc_5ippy$PE30_high-medians_red_inc_5ippy$PE30_low
medians_red_inc_5ippy_red_prev0.3$PE30_width_CrI<-medians_red_inc_5ippy_red_prev0.3$PE30_high-medians_red_inc_5ippy_red_prev0.3$PE30_low
medians_inc_ltf0.2$PE30_width_CrI<-medians_inc_ltf0.2$PE30_high-medians_inc_ltf0.2$PE30_low
medians_inc_dur25$PE30_width_CrI<-medians_inc_dur25$PE30_high-medians_inc_dur25$PE30_low
medians_red_dur15$PE30_width_CrI<-medians_red_dur15$PE30_high-medians_red_dur15$PE30_low
medians_seas_start$PE30_width_CrI<-medians_seas_start$PE30_high-medians_seas_start$PE30_low
medians_seas_end$PE30_width_CrI<-medians_seas_end$PE30_high-medians_seas_end$PE30_low

medians_baseline$mean_dur_prot_width_CrI<-medians_baseline$mean_dur_prot_high-medians_baseline$mean_dur_prot_low
medians_red_inc_5ippy$mean_dur_prot_width_CrI<-medians_red_inc_5ippy$mean_dur_prot_high-medians_red_inc_5ippy$mean_dur_prot_low
medians_red_inc_5ippy_red_prev0.3$mean_dur_prot_width_CrI<-medians_red_inc_5ippy_red_prev0.3$mean_dur_prot_high-medians_red_inc_5ippy_red_prev0.3$mean_dur_prot_low
medians_inc_ltf0.2$mean_dur_prot_width_CrI<-medians_inc_ltf0.2$mean_dur_prot_high-medians_inc_ltf0.2$mean_dur_prot_low
medians_inc_dur25$mean_dur_prot_width_CrI<-medians_inc_dur25$mean_dur_prot_high-medians_inc_dur25$mean_dur_prot_low
medians_red_dur15$mean_dur_prot_width_CrI<-medians_red_dur15$mean_dur_prot_high-medians_red_dur15$mean_dur_prot_low
medians_seas_start$mean_dur_prot_width_CrI<-medians_seas_start$mean_dur_prot_high-medians_seas_start$mean_dur_prot_low
medians_seas_end$mean_dur_prot_width_CrI<-medians_seas_end$mean_dur_prot_high-medians_seas_end$mean_dur_prot_low

medians_chars<-bind_rows(list(medians_baseline,
                              medians_red_inc_5ippy,
                              medians_red_inc_5ippy_red_prev0.3,
                              medians_inc_ltf0.2,
                              medians_inc_dur25,
                              medians_red_dur15,
                              medians_seas_start,
                              medians_seas_end))

medians_chars$scenario <- factor(medians_chars$scenario, levels = c("Baseline",
                                                                    "Reduction in incidence to 5ippy",
                                                                    "Reduction in incidence to 5ippy and prevalence to 30%",
                                                                    "Increase in LTF to 20%",
                                                                    "Increase in mean duration of protection to 25 days",
                                                                    "Reduction in mean duration of protection to 15 days",
                                                                    "Seasonality - start of season",
                                                                    "Seasonality - end of season"))

medians_chars$code_col<-1
medians_chars$code_col[which(medians_chars$scenario== "Baseline")]<-2
medians_chars$code_col<-factor(medians_chars$code_col)


ggplot(medians_chars, aes(x=scenario, y=PE30_width_CrI *100)) + theme_bw()+ theme(legend.position = "none")+
  ylab("Width of 95% Credible interval in 30-day efficacy per simulation (%)") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_chars, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=100*median(medians_chars$PE30_width_CrI[which(medians_chars$scenario== "Baseline")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_chars[which(medians_chars$scenario== "Baseline"),],fun=median, geom="point", size=2, color="slateblue")

ggplot(medians_chars, aes(x=scenario, y=mean_dur_prot_width_CrI)) + theme_bw()+ theme(legend.position = "none")+
  ylab("Width of 95% Credible interval in mean duration of protection") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_chars, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=median(medians_chars$mean_dur_prot_width_CrI[which(medians_chars$scenario== "Baseline")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_chars[which(medians_chars$scenario== "Baseline"),],fun=median, geom="point", size=2, color="slateblue")


##study design



medians_red_sample400$PE30_width_CrI<-medians_red_sample400$PE30_high-medians_red_sample400$PE30_low
medians_red_sample400$mean_dur_prot_width_CrI<-medians_red_sample400$mean_dur_prot_high-medians_red_sample400$mean_dur_prot_low

medians_red_followup42$PE30_width_CrI<-medians_red_followup42$PE30_high-medians_red_followup42$PE30_low
medians_red_followup42$mean_dur_prot_width_CrI<-medians_red_followup42$mean_dur_prot_high-medians_red_followup42$mean_dur_prot_low

medians_red_followup28$PE30_width_CrI<-medians_red_followup28$PE30_high-medians_red_followup28$PE30_low
medians_red_followup28$mean_dur_prot_width_CrI<-medians_red_followup28$mean_dur_prot_high-medians_red_followup28$mean_dur_prot_low

medians_add_AS$PE30_width_CrI<-medians_add_AS$PE30_high-medians_add_AS$PE30_low
medians_add_AS$mean_dur_prot_width_CrI<-medians_add_AS$mean_dur_prot_high-medians_add_AS$mean_dur_prot_low

medians_add_placebo$PE30_width_CrI<-medians_add_placebo$PE30_high-medians_add_placebo$PE30_low
medians_add_placebo$mean_dur_prot_width_CrI<-medians_add_placebo$mean_dur_prot_high-medians_add_placebo$mean_dur_prot_low

medians_add_AS_red_sample400$PE30_width_CrI<-medians_add_AS_red_sample400$PE30_high-medians_add_AS_red_sample400$PE30_low
medians_add_AS_red_sample400$mean_dur_prot_width_CrI<-medians_add_AS_red_sample400$mean_dur_prot_high-medians_add_AS_red_sample400$mean_dur_prot_low

medians_add_placebo_red_sample400$PE30_width_CrI<-medians_add_placebo_red_sample400$PE30_high-medians_add_placebo_red_sample400$PE30_low
medians_add_placebo_red_sample400$mean_dur_prot_width_CrI<-medians_add_placebo_red_sample400$mean_dur_prot_high-medians_add_placebo_red_sample400$mean_dur_prot_low

medians_add_AS_red_followup42$PE30_width_CrI<-medians_add_AS_red_followup42$PE30_high-medians_add_AS_red_followup42$PE30_low
medians_add_AS_red_followup42$mean_dur_prot_width_CrI<-medians_add_AS_red_followup42$mean_dur_prot_high-medians_add_AS_red_followup42$mean_dur_prot_low

medians_add_AS_red_sample400_red_followup42$PE30_width_CrI<-medians_add_AS_red_sample400_red_followup42$PE30_high-medians_add_AS_red_sample400_red_followup42$PE30_low
medians_add_AS_red_sample400_red_followup42$mean_dur_prot_width_CrI<-medians_add_AS_red_sample400_red_followup42$mean_dur_prot_high-medians_add_AS_red_sample400_red_followup42$mean_dur_prot_low

medians_add_AS_seas_start$PE30_width_CrI<-medians_add_AS_seas_start$PE30_high-medians_add_AS_seas_start$PE30_low
medians_add_AS_seas_start$mean_dur_prot_width_CrI<-medians_add_AS_seas_start$mean_dur_prot_high-medians_add_AS_seas_start$mean_dur_prot_low

medians_add_AS_seas_end$PE30_width_CrI<-medians_add_AS_seas_end$PE30_high-medians_add_AS_seas_end$PE30_low
medians_add_AS_seas_end$mean_dur_prot_width_CrI<-medians_add_AS_seas_end$mean_dur_prot_high-medians_add_AS_seas_end$mean_dur_prot_low







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

medians_design$code_col<-1
medians_design$code_col[which(medians_chars$scenario== "Baseline")]<-2
medians_design$code_col<-factor(medians_design$code_col)

ggplot(medians_design, aes(x=scenario, y= PE30_width_CrI *100)) + theme_bw()+ theme(legend.position = "none")+
  ylab("Width of 95% Credible interval in 30-day efficacy per simulation (%)") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_design, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=100*median(medians_design$PE30_width_CrI[which(medians_chars$scenario== "Baseline")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_chars[which(medians_chars$scenario== "Baseline"),],fun=median, geom="point", size=2, color="slateblue")


ggplot(medians_design, aes(x=scenario, y= mean_dur_prot_width_CrI)) + theme_bw()+ theme(legend.position = "none")+
  ylab("Width of 95% Credible interval in mean duration of protection") +
  xlab("Scenario \n \n ")+ 
  scale_x_discrete(limits=rev)+
  geom_violin(data=medians_design, aes(fill=code_col))+ 
  scale_fill_manual(values = c("mediumturquoise","lightslateblue"))+
  coord_flip() + 
  geom_hline(yintercept=median(medians_design$mean_dur_prot_width_CrI[which(medians_chars$scenario== "Baseline")]), color="slateblue")+
  stat_summary(fun=median, geom="point", size=2, color="turquoise4")+
  stat_summary(data=medians_chars[which(medians_chars$scenario== "Baseline"),],fun=median, geom="point", size=2, color="slateblue")


