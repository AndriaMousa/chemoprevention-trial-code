
source("functions/plot_diff_low_CI_power_function.R")
source("functions/plot_PE_over_time_2strain_function.R")

iter_combined_baseline<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_baseline<-read.csv("saved_dfs/2strain_model/medians_freq0.5_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_freq0.9<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.9_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_freq0.9<-read.csv("saved_dfs/2strain_model/medians_freq0.9_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_freq0.2<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.2_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_freq0.2<-read.csv("saved_dfs/2strain_model/medians_freq0.2_R18days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")

iter_combined_20daysR<-read.csv("saved_dfs/2strain_model/iter_combined_freq0.5_R20days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")
medians_20daysR<-read.csv("saved_dfs/2strain_model/medians_freq0.5_R20days_10ippy_prev0.4_determ0.9_N600_63d_ltf0.1.csv")



iter_combined_baseline$scenario<-"50% frequency of resistant strain \n effect size = 12 days difference"
iter_combined_freq0.9$scenario<-"90% frequency of resistant strain \n effect size = 12 days difference"
iter_combined_freq0.2$scenario<-"20% frequency of resistant strain \n effect size = 12 days difference"
iter_combined_20daysR$scenario<-"50% frequency of resistant strain \n effect size = 10 days difference"


medians_baseline$scenario<-"50% frequency of resistant strain \n effect size = 12 days difference"
medians_freq0.9$scenario<-"90% frequency of resistant strain \n effect size = 12 days difference"
medians_freq0.2$scenario<-"20% frequency of resistant strain \n effect size = 12 days difference"
medians_20daysR$scenario<-"50% frequency of resistant strain \n effect size = 10 days difference"

iter_combined<-bind_rows(list(iter_combined_baseline, iter_combined_freq0.9, iter_combined_freq0.2, iter_combined_20daysR))

iter_combined$scenario<-factor(iter_combined$scenario,levels = c("50% frequency of resistant strain \n effect size = 12 days difference",
                                                                 "90% frequency of resistant strain \n effect size = 12 days difference",
                                                                 "20% frequency of resistant strain \n effect size = 12 days difference",
                                                                 "50% frequency of resistant strain \n effect size = 10 days difference"))

medians<-bind_rows(list(medians_baseline,medians_freq0.9,medians_freq0.2,medians_20daysR))

medians$scenario<-factor(medians$scenario,levels = c("50% frequency of resistant strain \n effect size = 12 days difference",
                                                     "90% frequency of resistant strain \n effect size = 12 days difference",
                                                     "20% frequency of resistant strain \n effect size = 12 days difference",
                                                     "50% frequency of resistant strain \n effect size = 10 days difference"))

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
                      alpha=0.2,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
  geom_density_ridges(data=medians,
                      aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))),
                      alpha=0.8,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
  geom_density_ridges(data=iter_combined,
                      aes(x = d30PE_S, y = fct_rev(as_factor(scenario))),
                      alpha=0.2,bandwidth = 1.5, fill="#290AD8") + xlim(20,100)+
  geom_density_ridges(data=medians,
                      aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))),
                      alpha=0.8,bandwidth = 1.5, fill="#290AD8")+ xlim(20,100)+
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.y=element_text(size=10,vjust =0.5),
        plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))

# #plotA1<- 
# 
# iter_combined %>%
#   ggplot(aes(x = d30PE_R, y = factor(scenario))) +
#   geom_density_ridges() 
#   
# 
# ggplot() +
#   geom_density_ridges(data= iter_combined, aes(x = d30PE_R, y = factor(scenario)), alpha=0.2,bandwidth = 1, fill="#D82632") +
#   geom_density_ridges(data= iter_combined, aes(x = d30PE_S, y = factor(scenario)), alpha=0.2,bandwidth = 1, fill="#290AD8") +
#   geom_density_ridges(data= medians, aes(x = PE30_R_med, y = factor(scenario)), alpha=0.8,bandwidth = 1, fill="#D82632") +
#   geom_density_ridges(data= medians, aes(x = PE30_S_med, y = factor(scenario)), alpha=0.8,bandwidth = 1, fill="#290AD8") 
#   
#   
# 
#   
#   ggplot() +
#   ylab("Probability density")+ 
#     # xlab("30-day Protective Efficacy(%)")+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "50% frequency of resistant strain \n effect size = 12 days difference"),], 
#                       aes(x = d30PE_R,y = fct_rev(as_factor(scenario))), 
#                       alpha=0.2,bandwidth = 1.5, fill="#D82632")+ xlim(20,100) +
#    geom_density_ridges(data=medians[which(
#      medians$scenario == "50% frequency of resistant strain \n effect size = 12 days difference"),], 
#                        aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))), 
#                        alpha=0.8,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=iter_combined[which(
#   iter_combined$scenario == "50% frequency of resistant strain \n effect size = 12 days difference"),], 
#                       aes(x = d30PE_S, y = fct_rev(as_factor(scenario))), 
#                        alpha=0.2,bandwidth = 1.5, fill="#290AD8") + xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "50% frequency of resistant strain \n effect size = 12 days difference"),], 
#                       aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))),
#                       alpha=0.8,bandwidth = 1.5, fill="#290AD8")+ xlim(20,100) +
#     ggtitle("50% frequency of resistant strain \n effect size = 12 days difference")+
#   theme_ridges()  +
#   theme(legend.position = "none",
#         axis.title.y = element_text(size=10),
#         axis.text.y=element_blank() )
#   
#         #axis.text.y=element_text(size=15,vjust =0.5),
#         #plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
#         #axis.title.x = element_blank(),
#        # axis.line=element_blank(),
#        #axis.text.x=element_blank()#,
#        #,#axis.ticks=element_blank()#,
#         #axis.title.x=element_blank(),
#        # axis.title.y=element_blank()#,
#        # panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         #panel.grid.minor=element_blank(),plot.background=element_blank()
# 
# 
# theme(axis.line=element_blank(),axis.text.x=element_blank(),
#       axis.text.y=element_blank(),axis.ticks=element_blank(),
#       axis.title.x=element_blank(),
#       axis.title.y=element_blank(),legend.position="none",
#       panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#       panel.grid.minor=element_blank(),plot.background=element_blank()) +
# 
# plotA2<- ggplot() +
#   # ylab("Probability density")+ 
#   # xlab("30-day Protective Efficacy(%)")+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "90% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = d30PE_R,y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "90% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))), 
#     alpha=0.8,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "90% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = d30PE_S, y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#290AD8") + xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "90% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))),
#     alpha=0.8,bandwidth = 1.5, fill="#290AD8")+ xlim(20,100)+
#   theme_ridges() +
#   theme(legend.position = "none",
#         axis.text.y=element_text(size=15,vjust =0.5),
#         plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
#         axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15))
# 
# 
# plotA3<- ggplot() +
#   # ylab("Probability density")+ 
#   # xlab("30-day Protective Efficacy(%)")+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "20% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = d30PE_R,y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "20% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))), 
#     alpha=0.8,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "20% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = d30PE_S, y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#290AD8") + xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "20% frequency of resistant strain \n effect size = 12 days difference"),], 
#     aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))),
#     alpha=0.8,bandwidth = 1.5, fill="#290AD8")+ xlim(20,100)+
#   theme_ridges() +
#   theme(legend.position = "none",
#         axis.text.y=element_text(size=15,vjust =0.5),
#         plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
#         axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15))
# 
# 
# plotA4<- ggplot() +
#   # ylab("Probability density")+ 
#   xlab("30-day Protective Efficacy(%)")+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "50% frequency of resistant strain \n effect size = 10 days difference"),], 
#     aes(x = d30PE_R,y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "50% frequency of resistant strain \n effect size = 10 days difference"),], 
#     aes(x = PE30_R_med, y = fct_rev(as_factor(scenario))), 
#     alpha=0.8,bandwidth = 1.5, fill="#D82632")+ xlim(20,100)+
#   geom_density_ridges(data=iter_combined[which(
#     iter_combined$scenario == "50% frequency of resistant strain \n effect size = 10 days difference"),], 
#     aes(x = d30PE_S, y = fct_rev(as_factor(scenario))), 
#     alpha=0.2,bandwidth = 1.5, fill="#290AD8") + xlim(20,100)+
#   geom_density_ridges(data=medians[which(
#     medians$scenario == "50% frequency of resistant strain \n effect size = 10 days difference"),], 
#     aes(x = PE30_S_med, y = fct_rev(as_factor(scenario))),
#     alpha=0.8,bandwidth = 1.5, fill="#290AD8")+ xlim(20,100)+
#   theme_ridges() +
#   theme(legend.position = "none",
#         axis.text.y=element_text(size=15,vjust =0.5),
#         plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
#         axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15))
# # plotB<-ggplot() +
# #   ylab("Probability density")+ xlab("Mean duration of protection (days)")+
# #   geom_density_ridges(data=iter_combined, 
# #                       aes(x = mean_dur_prot_R, y = fct_rev(as_factor(scenario))), 
# #                       alpha=0.2,bandwidth = 0.6, fill="#D82632")+ xlim(0,60)+
# #   geom_density_ridges(data=medians,
# #                       aes(x = mean_dur_prot_R_med, y = fct_rev(as_factor(scenario))), 
# #                       alpha=0.8,bandwidth = 0.6, fill="#D82632")+xlim(0,60)+
# #   geom_density_ridges(data=iter_combined, 
# #                       aes(x = mean_dur_prot_S, y = fct_rev(as_factor(scenario))), 
# #                       alpha=0.2,bandwidth = 0.6, fill="#290AD8") +xlim(0,60)+
# #   geom_density_ridges(data=medians,
# #                       aes(x = mean_dur_prot_S_med, y = fct_rev(as_factor(scenario))), 
# #                       alpha=0.8,bandwidth = 0.6, fill="#290AD8")+xlim(0,60)+
# #   theme_ridges() +
# #   theme(legend.position = "none",
# #         axis.text.y=element_text(size=15,vjust =0.5),
# #         plot.margin=margin(l=1,r=1,t=1,b=1, unit="cm"),
# #         axis.title.x = element_text(size=15),
# #         axis.title.y = element_text(size=15))

############################################################################
##############protection curves

output_baseline<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                  w_S=5,                 ## shape parameter (Sensitive)
                                  w_R=5,                 ## shape parameter (Resistant)
                                  mean_protect_S= 30,    ## mean duration of protection against S
                                  mean_protect_R=18,     ## mean duration of protection against R
                                  N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                  followup=63,           ## specify total followup
                                  ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                  prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                  freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                                  prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                                  ltf=0.10,              ## loss to followup
                                  control=0,             ## presence of control group? 0=no,1=yes
                                  AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                  N0_control0=200 )      ## number in control group. Will only be used if control=1

baseline_PE<-output_baseline+ggtitle("50% frequency of resistant strain, \n effect size = 12 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
baseline_PE<-output_baseline +theme(axis.title.y = element_text(size=9),
                                    axis.title.x = element_text(size=9),
                                    axis.text.x= element_text(size=7),
                                    axis.text.y= element_text(size=7),
                                    legend.position="none")

output_freq0.9<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                  w_S=5,                 ## shape parameter (Sensitive)
                                  w_R=5,                 ## shape parameter (Resistant)
                                  mean_protect_S= 30,    ## mean duration of protection against S
                                  mean_protect_R=18,     ## mean duration of protection against R
                                  N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                  followup=63,           ## specify total followup
                                  ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                  prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                  freq_R=0.90,           ## prevalence of resistant genotype in the parasite population (frequency)
                                  prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                                  ltf=0.10,              ## loss to followup
                                  control=0,             ## presence of control group? 0=no,1=yes
                                  AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                  N0_control0=200 )      ## number in control group. Will only be used if control=1
freq0.9_PE<-output_freq0.9+ggtitle("90% frequency of resistant strain, \n effect size = 12 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
freq0.9_PE<-output_freq0.9+theme(axis.title.y = element_text(size=9),
                                 axis.title.x = element_text(size=9),
                                 axis.text.x= element_text(size=7),
                                 axis.text.y= element_text(size=7),
                                 legend.position="none")

output_freq0.2<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                  w_S=5,                 ## shape parameter (Sensitive)
                                  w_R=5,                 ## shape parameter (Resistant)
                                  mean_protect_S= 30,    ## mean duration of protection against S
                                  mean_protect_R=18,     ## mean duration of protection against R
                                  N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                  followup=63,           ## specify total followup
                                  ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                  prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                  freq_R=0.20,           ## prevalence of resistant genotype in the parasite population (frequency)
                                  prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                                  ltf=0.10,              ## loss to followup
                                  control=0,             ## presence of control group? 0=no,1=yes
                                  AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                  N0_control0=200 )      ## number in control group. Will only be used if control=1
freq0.2_PE<-output_freq0.2+ggtitle("20% frequency of resistant strain,  \n effect size = 12 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
freq0.2_PE<-output_freq0.2+theme(axis.title.y = element_text(size=9),
                                 axis.title.x = element_text(size=9),
                                 axis.text.x= element_text(size=7),
                                 axis.text.y= element_text(size=7),
                                 legend.position="none")

output_effect_size_10daysdiff<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                  w_S=5,                 ## shape parameter (Sensitive)
                                  w_R=5,                 ## shape parameter (Resistant)
                                  mean_protect_S= 30,    ## mean duration of protection against S
                                  mean_protect_R=20,     ## mean duration of protection against R
                                  N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                  followup=63,           ## specify total followup
                                  ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                  prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                  freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                                  prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                                  ltf=0.10,              ## loss to followup
                                  control=0,             ## presence of control group? 0=no,1=yes
                                  AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                  N0_control0=200 )      ## number in control group. Will only be used if control=1

effect_size_10daysdiff_PE<-output_effect_size_10daysdiff+ggtitle("50% frequency of resistant strain, \n effect size = 10 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
effect_size_10daysdiff_PE<-output_effect_size_10daysdiff +theme(axis.title.y = element_text(size=9),
                                                                axis.title.x = element_text(size=9),
                                                                axis.text.x= element_text(size=7),
                                                                axis.text.y= element_text(size=7),
                                                                legend.position="none")


##################################


PE_panels<-plot_grid(baseline_PE,
                     freq0.9_PE,
                     freq0.2_PE,
                     effect_size_10daysdiff_PE, label_size=20,nrow = 4) +   
  theme(plot.margin = unit(c(3.6,0,3.3,0), "cm")) ## top, right, bottom, left


#plot_grid(plotA1, plotA2, plotA3, plotA4, nrow=4, label_size = 10)

Fig4<-plot_grid(plotA, PE_panels,label_size=20,ncol = 2 ,rel_widths = c(2.5,1))
Fig4

pdf(file = "Fig4.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 8) # The height of the plot in inches
Fig4
dev.off()

