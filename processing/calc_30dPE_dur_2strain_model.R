
source("functions/calc_30dPE_dur_2strain_model_function.R")

###   this script calculated 30-day Protective efficacy and mean duration of protection for each parameter value in the posterior distribution
###   it then saves these 3 dataframes for each scenario in the folder "saved_dfs":

##    1) imputed posterior distributions of 30dPE and mean dur across all simulations (1000sims x 10000 iterations per scenario) 
###   2) medians (along with 2.5th and 97.5th percentiles) of 30dPE and mean duration of protection (1 per simulation)
###   3) the power to detect a 30-day Protective efficacy that is higher than a given threshold

#################code below shows 3 scenario examples.

###freq to 90%

assemble_posteriors_2strain(N_sims= 1000,          ## number of simulated datasets to be generated
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
                            N0_control0=200,
                            dt=0.5)

###freq to 20%

assemble_posteriors_2strain(N_sims= 1000,          ## number of simulated datasets to be generated
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
                            N0_control0=200,
                            dt=0.5)

###effect size to 10 days diff (20 days against R)

assemble_posteriors_2strain(N_sims= 1000,          ## number of simulated datasets to be generated
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
                            N0_control0=200,
                            dt=0.5)
