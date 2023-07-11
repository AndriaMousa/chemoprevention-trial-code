

source("functions/plot_diff_low_CI_power_function.R")
source("functions/plot_PE_over_time_2strain_function.R")

output<- plot_diff_low_CI(N_sims= 1000,         ## number of simulated datasets to be generated
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

baseline_power<-output+ggtitle("Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_diff_low_CI(N_sims= 1000,         ## number of simulated datasets to be generated
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

inc_freq0.9_power<-output+ggtitle("\n \n 90% frequency of resistant strain") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- plot_diff_low_CI(N_sims= 1000,         ## number of simulated datasets to be generated
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

inc_freq0.2_power<-output+ggtitle("\n \n 20% frequency of resistant strain") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<- plot_diff_low_CI(N_sims= 1000,         ## number of simulated datasets to be generated
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

effect_size_10daysdiff_power<-output+ggtitle(" \n \n effect size = 10 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

##############protection curves

output<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
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

baseline_PE<-output+ggtitle("Baseline \n (50% frequency of resistant strain, \n effect size = 12 days difference)") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
inc_freq0.9_PE<-output+ggtitle("\n \n 90% frequency of resistant strain") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
inc_freq0.2_PE<-output+ggtitle("\n \n 20% frequency of resistant strain") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<-plot_PE_over_time_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
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

effect_size_10daysdiff_PE<-output+ggtitle(" \n \n effect size = 10 days difference") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


Fig5<-plot_grid(baseline_PE,baseline_power,
                inc_freq0.9_PE,inc_freq0.9_power,
                inc_freq0.2_PE,inc_freq0.2_power, 
                effect_size_10daysdiff_PE, effect_size_10daysdiff_power, 
                labels = c('A', 'B', 'C', 'D', 'E','F','G','H'),label_size = 20, nrow=4)
pdf(file = "Fig5.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10) # The height of the plot in inches
Fig5
dev.off()
