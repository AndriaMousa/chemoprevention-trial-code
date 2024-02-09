# Measuring protective efficacy and quantifying the impact of drug resistance: a novel chemoprevention trial design and methodology
:mosquito:🦟:test_tube:🧪:dna: 🧬
 
## Overview
This repository contains the code used to simulate and analyse chemoprevention trial scenarios, related to study design, and participant and setting characteristics. There are two types of models: a single-strain model that can be used to estimate the overall protective efficacy of chemoprevention, and  two-strain model that can be used to estimate genotype-differences in protective efficacy. 
The corresponding scientific article is available at:
Andria Mousa, Gina Cuomo-Dannenburg, Hayley A. Thompson, R. Matthew Chico, Khalid Beshir, Colin J. Sutherland, David Schellenberg, Roly Gosling, Michael Alifrangis, Emma Filtenborg Hocke, Helle S. Hansson, Ana Chopo-Pizarro, Wilfred F. Mbacham, Innocent M. Ali, Mike Chaponda, Cally Roper, and Lucy C. Okell . Available at: ADD REFERENCE LINK HERE
 
## Repo Contents
- [additional supplementary materials](./additional%20supplementary%20materials): this folder includes the code to produce all supplementary plots and materials, including a three-strain model extension, and an alternative approach of calculating power using Cox proportional hazards.
- [code to generate figures](./code%20to%20generate%20figures): this folder contains code related to figures presented in the main manuscript
- [deterministic model](./deterministic%20model): this folder contains the deterministic model files for the one- and two-strain models, with or without a control group (stan files) 
- [functions](./functions): contains functions required for the analyses presented in the paper. These include the simulation functions, post-processing calculation functions, and functions that plot 1) the simulations and deterministic values, 2)the protective  efficacy over time in the genotype model, 3) the mean duration of protection, and 4) the distribution of the lower 95% credible interval and power. 
- [processing](./processing): This folder contains scripts to process the stan outputs and calculate the 30-day protective efficacy and mean duration of protection for one- and two-strain models. Additionally it includes the post-processing to calculate the width of CrIs distribution for each scenario (precision).
- **model_fitting_Xstrain_X_control files**: these are the files used to fit the stan model to the simulated data. There are separate files for one- and two-strain models and for scenarios with or without a control group
- **simulation_examples**: this file contains examples of simulating data using the simulation functions

### **Important note**: 
Two folders are provided with the publication which are absent from the repo due to file size restrictions. These are needed to run some of the analyses in the repo, and both folders should be stored in the same directory as the files above. 
- **stan_output**: Containing .rds outputs from model fitting (undertaken using a Bayesian framework implemented using rStan). Within this folder there are subfolders reflecting different trial scenarios. Within each scenario folder there are 1000 .rds objects, each containing the posterior distribution of parameters for that simulation.
- **saved_dfs**: Contains the full posterior distribution from each simulation 
 
## Software Requirements
Running the code contained in this repository requires the following:
- The R Programming Language (Version 4.2.2 used here) 
https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.
- The packages **matrixStats** , **gdata**, **ggplot2**, **cowplot**, **plyr**, **tidyr**, **dplyr**, **bayesplot**, **ggridges**, **forcats**, **survival**, **lubridate**, **ggsurvfit**, **gtsummary**, **tidycmprsk**, **condsurv** 
- The package **rstan** :  This work utilises the probabilistic programming language Stan for model fitting (implemented in R via the package rstan). Stan is a program for analysis of Bayesian models using Markov Chain Monte Carlo (MCMC) simulation. More information and details about the software and its use via R are available here: https://github.com/stan-dev/rstan/wiki . Guidelines on how to install rstan can be found here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started .
 ## Installation Guide and Instructions for Use
The following instructions require that all the relevant `R` packages have been installed by the user and that rstan has been installed. To replicate and reproduce the analyses presented in this paper, do the following: 
1. Clone this Github repository and make a local copy on your desktop.
2. Run the `R` code scripts for the particular part of the analysis you are trying to reproduce.
    - **Note that high performance computing may be needed for model fitting to a large number of simulations.**
3. The output from running this code will be a number of MCMC objects, as well as a series of plots representing the output from MCMC based fitting of the relevant model to the simulated data. The code to reproduce all figures presented in main text, and supplementary information of the associated publication is available within this repository.
