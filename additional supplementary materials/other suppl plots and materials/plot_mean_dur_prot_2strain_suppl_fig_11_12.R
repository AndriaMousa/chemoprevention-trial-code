



source("functions/function_plot_mean_dur_prot.R")

output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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

baseline<-output+ggtitle("Baseline \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=5,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_inc_5ippy<-output+ggtitle("Reduction in incidence to 5ippy \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=5,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.30,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_inc_5ippy_red_prev_0.3<-output+ggtitle("Reduction in incidence to 5ippy \n and prevalence to 30%") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
red_inc_5ippy_red_prev_0.3




output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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

red_freq_0.2<-output+ggtitle("Reduction in frequency to 20% \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
red_freq_0.2


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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

inc_freq_0.9<-output+ggtitle("Increase in frequency to 90% \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")
inc_freq_0.9



output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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
                             ltf=0.20,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

inc_ltf_0.2<-output+ggtitle("Increase in LTF to 20% \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.70,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_determ_0.7<-output+ggtitle("Reduction in proportion determined \n to 70%") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=15,     ## mean duration of protection against R
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

red_durR_15<-output+ggtitle("Reduction in duration of protection \n against resistant strain to 15 days") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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

inc_durR_20<-output+ggtitle("Increase in duration of protection \n against resistant strain to 20 days") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=500,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_sample_500<-output+ggtitle("Reduction in sample size to 500 \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")




output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=42,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_followup_42<-output+ggtitle("Reduction of follow-up to \n 42 days ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=28,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=0,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

red_followup_28<-output+ggtitle("Reduction of follow-up to \n 28 days ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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
                             control=1,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

add_AS<-output+ggtitle("Addition of artesunate control arm \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
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
                             control=1,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="placebo",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

add_placebo<-output+ggtitle("Addition of placebo control arm \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=1,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

add_AS_red_sample_400<-output+ggtitle("Addition of artesunate control arm & \n reduction in sample size to 400") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- plot_hist_mean_prot(N_sims= 1000,         ## number of simulated datasets to be generated
                             w_S=5,                 ## shape parameter (Sensitive)
                             w_R=5,                 ## shape parameter (Resistant)
                             mean_protect_S= 30,    ## mean duration of protection against S
                             mean_protect_R=18,     ## mean duration of protection against R
                             N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                             followup=63,           ## specify total followup
                             ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                             prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                             freq_R=0.50,           ## prevalence of resistant genotype in the parasite population (frequency)
                             prob_determ=0.90,      ## % of infections for which genotype of interest can be determined
                             ltf=0.10,              ## loss to followup
                             control=1,             ## presence of control group? 0=no,1=yes
                             AS_or_placebo="placebo",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                             N0_control0=200 )      ## number in control group. Will only be used if control=1

add_placebo_red_sample_400<-output+ggtitle("Addition of placebo control arm & \n reduction in sample size to 400") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


plot_grid(baseline, red_inc_5ippy, red_inc_5ippy_red_prev_0.3, red_freq_0.2, inc_freq_0.9, inc_ltf_0.2, red_determ_0.7, red_durR_15, inc_durR_20, nrow = 3)
plot_grid(baseline, red_sample_500, red_followup_42,  red_followup_28, add_AS, add_placebo, 
          add_AS_red_sample_400, add_placebo_red_sample_400,nrow = 4)

