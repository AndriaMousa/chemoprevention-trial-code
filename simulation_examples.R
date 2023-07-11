

######this code uses the simulation functions stored in the functions folder.

##setwd("....") ### set working directory where functions folder is located 


####an example of simulating data in a 1-strain model

source("functions/simulation_function_1strain.R")

output<- simulate_trial_1strain(N_sims= 1000,          ## number of simulated datasets to be generated
                                w=5,                    ## shape parameter
                                mean_protect= 20,       ## mean duration of protection 
                                N0_treat0=600,          ## sample size (enrolled and treated with chemoprevention) 
                                followup=63,            ## specify total followup
                                ippy=10,                ## infection rate (infections per person per year. Includes asymptomatic infections
                                prevalence=0.40,        ## slide prevalence (only day0 negative will be analysed for protection)
                                ltf=0.10,               ## loss to followup
                                control=1,              ## presence of control group? 0=no,1=yes
                                AS_or_placebo="AS",     ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                N0_control0=200,         ## number in control group. Will only be used if control=1
                                seasonality = "end" )     


####an example of simulating data in a 2-strain model

source("functions/simulation_function_2strain.R")

output<- simulate_trial_2strain(N_sims= 1000,         ## number of simulated datasets to be generated
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
                                control=1,             ## presence of control group? 0=no,1=yes
                                AS_or_placebo="AS",    ## Type of control group : options: "none" "AS" "placebo". Will only be used if control=1
                                N0_control0=200 )      ## number in control group. Will only be used if control=1

