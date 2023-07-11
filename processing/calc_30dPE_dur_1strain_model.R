
source("functions/calc_30dPE_dur_1strain_model_function.R")

###   this script calculated 30-day Protective efficacy and mean duration of protection for each parameter value in the posterior distribution
###   it then saves these 3 dataframes for each scenario in the folder "saved_dfs":

##    1) imputed posterior distributions of 30dPE and mean dur across all simulations (1000sims x 10000 iterations per scenario) 
###   2) medians (along with 2.5th and 97.5th percentiles) of 30dPE and mean duration of protection (1 per simulation)
###   3) the power to detect a 30-day Protective efficacy that is higher than a given threshold

###Baseline

assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )      ## number in control group. Will only be used if control=1)


#Reduction in incidence to 5ippy
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Reduction in incidence to 5ippy \n and prevalence to 30%
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Increase in LTF to 20%
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Reduction in mean duration \n of protection to 15 days
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Increase in mean duration \n of protection to 25 days
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Seasonality - start of season
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Seasonality - end of season
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )




#Reduction in sample size to 400 
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Reduction of follow-up \n  to 42 days
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Reduction of follow-up \n  to 28 days
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of short-acting control group
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of untreated control group 
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of short-acting control group \n and reduction in sample size to 400
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of untreated control group \n and reduction in sample size to 400
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of short-acting control group \n Reduction of follow-up to 42 days 
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )
#Addition of short-acting control group, \n reduction of follow-up to 42 days \n and reduction in sample size to 400 
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )      ## number in control group. Will only be used if control=1)
#Addition of short-acting control group, \n Seasonality - start of season 
assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )      ## number in control group. Will only be used if control=1)

#Addition of short-acting control group, \n Seasonality - end of season 

assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                            dt=0.5 )      ## number in control group. Will only be used if control=1)




###addition of short-acting control group, Red followup to 28 

assemble_posteriors_1strain(N_sims= 1000,         ## number of simulated datasets to be generated
                            w=5,                 ## shape parameter (Sensitive)
                            mean_protect= 20,    ## mean duration of protection against S
                            N0_treat0=600,         ## sample size (enrolled and treated with chemoprevention) 
                            followup=28,           ## specify total followup
                            ippy=10,               ## infection rate (infections per person per year. Includes asymptomatic infections
                            prevalence=0.40,       ## slide prevalence (only day0 negative will be analysed for protection)
                            ltf=0.10,              ## loss to followup
                            control=1,             ## presence of control group? 0=no,1=yes
                            AS_or_placebo="AS",  ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                            N0_control0=200, 
                            seasonality="none",
                            dt=0.5 )      ## number in control group. Will only be used if control=1)




