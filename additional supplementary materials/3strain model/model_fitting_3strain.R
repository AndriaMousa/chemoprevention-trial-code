
input_1000_dfs<-readRDS("simulated_data/input_df_sim_3str_freq0.7_0.06_R18_10_days_10ippy_determ_0.85_500.RData")


##to read simulations

N_sims<-1000

options(mc.cores = parallel::detectCores())
dt=0.5
endTime=63
N<-endTime/dt+1 # time steps for running the model. +1 to allow for time 0
rstan_options(auto_write = TRUE)


for (run in 200:200) {   ##just taking 1 example here (200th simulated dataset)
  input_df<-as.data.frame(input_1000_dfs[[run]])
  
  set.seed(999)
  
  start_time = Sys.time()
  
  N_outcome<-as.matrix(cbind(input_df$N_treated_I_R1_new[2:13],input_df$N_treated_I_R2_new[2:13],input_df$N_treated_I_S_new[2:13],input_df$N_treated_uninf[2:13], input_df$N_treated_I_undeterm_new[2:13]))
  
  #stan data list                                                  
  
  stan_data <- list( N_obs = nrow(input_df),                        ## number of observed time points
                     T = input_df$T,                                ## time point (eg. day 0, day 2 etc)
                     row_model = (input_df$T/dt)+1,
                     N_outcome=N_outcome,
                     N=N,
                     dt=dt)
  
 model <- stan("additional supplementary materials/3strain model/3strain_model.stan",
                                                        data = stan_data,
                                                        chains = 4,
                                                        iter = 5000)
 params<-rstan::extract( model,pars=c("inc", "lambda_R1", "lambda_R2","lambda_S", "w_R1", "w_R2", "w_S", "prob_determ"))

  end_time=Sys.time()
  diff=end_time-start_time
  print(diff)
  
}


ggplot() + theme_bw()+
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_S/N_treated_at_risk)), colour="limegreen", size = 1.5) +
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_R1/N_treated_at_risk)), colour="orange", size = 1.5) +
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_R2/N_treated_at_risk)), colour="red", size = 1.5) +
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_undeterm/N_treated_at_risk)), colour="darkblue", size = 1.5) +
  ylab("Proportion infected") + labs(title="") + xlab("Follow-up (days)")




dur_prot_R1<-gamma(1+(1/params$w_R1)) * params$lambda_R1 
mean(dur_prot_R1)

dur_prot_R2<-gamma(1+(1/params$w_R2)) * params$lambda_R2 
mean(dur_prot_R2)

dur_prot_S<-gamma(1+(1/params$w_S)) * params$lambda_S 
mean(dur_prot_S)

new_y<-rstan::extract(model,pars="pr_I_S")
new_pr_I_S<-apply(new_y[[1]],2,quantile,probs=c(0.025,0.5,0.975)) #the median line with 95% credible intervals
new_pr_I_S<-as.data.frame(t(new_pr_I_S))
new_pr_I_S<-plyr::rename(new_pr_I_S, c("2.5%"="low_I_S", "50%"="med_I_S", "97.5%"="high_I_S"))

new_pr_I_S$T<-NA
new_pr_I_S$T<-seq(0,63,by=dt)

new_y<-rstan::extract(model,pars="pr_I_undeterm")
new_pr_I_undeterm<-apply(new_y[[1]],2,quantile,probs=c(0.025,0.5,0.975)) #the median line with 95% credible intervals
new_pr_I_undeterm<-as.data.frame(t(new_pr_I_undeterm))
new_pr_I_undeterm<-plyr::rename(new_pr_I_undeterm, c("2.5%"="low_I_undeterm", "50%"="med_I_undeterm", "97.5%"="high_I_undeterm"))

new_pr_I_undeterm$T<-NA
new_pr_I_undeterm$T<-seq(0,63,by=dt)



new_y<-rstan::extract(model,pars="pr_I_R1")
new_pr_I_R1<-apply(new_y[[1]],2,quantile,probs=c(0.025,0.5,0.975)) #the median line with 95% credible intervals
new_pr_I_R1<-as.data.frame(t(new_pr_I_R1))
new_pr_I_R1<-plyr::rename(new_pr_I_R1, c("2.5%"="low_I_R", "50%"="med_I_R", "97.5%"="high_I_R"))

new_pr_I_R1$T<-NA
new_pr_I_R1$T<-seq(0,63,by=dt)

new_y<-rstan::extract(model,pars="pr_I_R2")
new_pr_I_R2<-apply(new_y[[1]],2,quantile,probs=c(0.025,0.5,0.975)) #the median line with 95% credible intervals
new_pr_I_R2<-as.data.frame(t(new_pr_I_R2))
new_pr_I_R2<-plyr::rename(new_pr_I_R2, c("2.5%"="low_I_R", "50%"="med_I_R", "97.5%"="high_I_R"))

new_pr_I_R2$T<-NA
new_pr_I_R2$T<-seq(0,63,by=dt)

ggplot() +  theme_bw() + theme(legend.title = element_blank())+
  geom_ribbon(data=new_pr_I_S,aes(x = T,ymin =low_I_S,ymax =high_I_S), 
              fill=c(alpha("royalblue",0.2)), group=1) +
  geom_line(data=new_pr_I_S,aes(x =T,y=med_I_S, colour="S"), size=1) +
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_S/N_treated_at_risk[1]), colour="S"))+
  geom_ribbon(data=new_pr_I_R1,aes(x = T,
                                  ymin =low_I_R,
                                  ymax =high_I_R), 
              fill=c(alpha("orange",0.2)), group=1) +
  geom_line(data=new_pr_I_R1,aes(x =T,y=med_I_R,colour="R1"),
            size=1) +
  geom_point(data= input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_R1/N_treated_at_risk[1]), colour="R1")) +
  geom_ribbon(data=new_pr_I_R2,aes(x = T,ymin =low_I_R,ymax =high_I_R), 
              fill=c(alpha("violetred1",0.2)), group=1) +
  geom_line(data=new_pr_I_R2,aes(x =T,y=med_I_R,colour="R2"), size=1) +
  geom_point(data= input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_R2/N_treated_at_risk[1]), colour="R2"))+
geom_ribbon(data=new_pr_I_undeterm,aes(x = T,ymin =low_I_undeterm,ymax =high_I_undeterm), 
            fill=c(alpha("black",0.2)), group=1) +
  geom_line(data=new_pr_I_undeterm,aes(x =T,y=med_I_undeterm,colour="Undetermined"), size=1) +
  geom_point(data=input_1000_dfs[[200]], aes(x=T, y=(N_treated_I_undeterm/N_treated_at_risk[1]), colour="Undetermined"))+
  ylab("Proportion infected among all participants") + labs(title="") + xlab("Follow-up (days)")+
scale_colour_manual(values = c( "S"="royalblue", "R1"= "orange","R2"="violetred1", "Undetermined"= "black"))


trace_plot_parms<-mcmc_trace(model,
                             pars = c("inc", "freq[1]", "freq[2]", "freq[3]","lambda_R1","lambda_R2","lambda_S", "w_R1","w_R2", "w_S", "prob_determ"),
                             facet_args = list(nrow = 6))+ 
  theme(legend.position = 'none') 

posterior_dist_parms<-mcmc_hist(model,
                                pars = c("inc", "freq[1]", "freq[2]", "freq[3]","lambda_R1","lambda_R2","lambda_S", "w_R1","w_R2", "w_S", "prob_determ"),
                                facet_args = list(nrow = 6))

trace_plot_parms
posterior_dist_parms

