
source("functions/plot_sims_distr_deterministic_1strain_function.R")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

baseline<-output+ggtitle("Baseline \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=5,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_inc_5ippy<-output+ggtitle("Reduction in incidence to 5ippy \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=5,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.30,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_inc_5ippy_red_prev_0.3<-output+ggtitle("Reduction in incidence to 5ippy \n and prevalence to 30%") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.20,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

inc_ltf_0.2<-output+ggtitle("Increase in LTF to 20% \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 15,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_dur_15<-output+ggtitle("Reduction in mean duration \n of protection to 15 days") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 25,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

inc_dur_25<-output+ggtitle("Increase in mean duration \n of protection to 25 days") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="start",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

seas_start<-output+ggtitle("Seasonality - start of season \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")

output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="end",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

seas_end<-output+ggtitle("Seasonality - end of season \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


plot_grid(baseline, red_inc_5ippy, red_inc_5ippy_red_prev_0.3, inc_ltf_0.2,red_dur_15, inc_dur_25, seas_start, seas_end, ncol = 2)



output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

baseline<-output+ggtitle("Baseline \n \n") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")




output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_sample_400<-output+ggtitle("Reduction in sample size to 400 \n  \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")




output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=42,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_followup42<-output+ggtitle("Reduction of follow-up \n  to 42 days  \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")





output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=28,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=0,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="none",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

red_followup28<-output+ggtitle("Reduction of follow-up \n  to 28 days  \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")





output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS<-output+ggtitle("Addition of short-acting control group \n \n") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="placebo",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_placebo<-output+ggtitle("Addition of untreated control group \n  \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS_red_sample400<-output+ggtitle("Addition of short-acting control group \n and reduction in sample size to 400 \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")





output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="placebo",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_placebo_red_sample400<-output+ggtitle("Addition of untreated control group \n and reduction in sample size to 400 \n") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=42,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS_red_followup42<-output+ggtitle("Addition of short-acting control group \n Reduction of follow-up to 42 days \n") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")





output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=400,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=42,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="none",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS_red_sample400_red_followup42<-output+ggtitle("Addition of short-acting control group, \n reduction of follow-up to 42 days \n and reduction in sample size to 400 ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="start",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS_seas_start<-output+ggtitle("Addition of short-acting control group, \n Seasonality - start of season \n ") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")


output<- sims_distr_deterministic_plot_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                                      w=5,                 ## shape parameter (Sensitive)
                                      mean_protect= 20,    ## mean duration of protection against S
                                      N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                                      followup=63,           ## specify total followup
                                      ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                                      prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                                      ltf=0.10,              ## loss to followup
                                      control=1,             ## presence of control group? 0=no,1=yes
                                      AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                      N0_control0=200, 
                                      seasonality="end",
                                      dt=0.5 )      ## number in control group. Will only be used if control=1

add_AS_seas_end<-output+ggtitle("Addition of short-acting control group, \n Seasonality - end of season \n") +theme(plot.title = element_text(size = 10, face = "bold"),legend.position="none")



plot_grid(baseline, red_sample_400, red_followup42,  
          red_followup28, add_AS, add_placebo, 
          add_AS_red_sample400, add_placebo_red_sample400, add_AS_red_followup42, 
          add_AS_red_sample400_red_followup42, add_AS_seas_start, add_AS_seas_end, nrow=4)


