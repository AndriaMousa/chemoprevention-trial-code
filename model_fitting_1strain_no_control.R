
##setwd("")

### this is using a single scenario as an example. 
### Read in scenario of interest, and save scenario of interest by editing code below to reflect the conditions in that scenario. 

input_1000_dfs<-readRDS("simulated_data/input_df_sim_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1.RData")


##to read simulations

N_sims<-1000

options(mc.cores = parallel::detectCores())
dt=0.5
endTime=63
N<-endTime/dt+1 # time steps for running the model. +1 to allow for time 0
rstan_options(auto_write = TRUE)




for (run in 1:N_sims) {  ###R may crash after~500 simulations. I would recommend doing this in batches of no more than 500 sims or run on a HPC
  input_df<-as.data.frame(input_1000_dfs[[run]])

  set.seed(999)
  
  start_time = Sys.time()
  
  N_outcome<-as.matrix(cbind(input_df$N_treated_I_new[2:nrow(input_df)],
                             input_df$N_treated_uninf[2:nrow(input_df)]))
  
  #stan data list                                                  
  
  stan_data <- list( N_obs = nrow(input_df),                        ## number of observed time points
                     T = input_df$T,                                ## time point (eg. day 0, day 2 etc)
                     row_model = (input_df$T/dt)+1,
                     N_outcome=N_outcome,
                     N=N,
                     dt=dt)
  
 model <- stan("stan_model_1strain_no_control.stan",
               data = stan_data,
               chains = 4,
               iter = 5000)

                                    
 #warmup=2000,
 #control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15) 
 
 params<-rstan::extract( model,pars=c("inc", "lambda", "w"))
 
  saveRDS(params,paste0("stan_output/input_df_sim_20days_10ippy_prev0.4_seas_none_N600_63d_ltf0.1/stan_output_run",run,".rds"))
  
  end_time=Sys.time()
  diff=end_time-start_time
  print(diff)
  
}

mcmc_trace(model,pars=c("inc", "lambda","w"))
mcmc_hist(model,pars=c("inc", "lambda","w"))

print(summary(model, pars = c("inc", "lambda","w"),digits=3))

