N_sims<-1000

##panel A : 10ippy, infreq followup, 50% frequency of R

input_sim_dfs<- readRDS("simulated_data/mixed_inf_all_10ippy_infreq_freq0.5.RData") 

 breaks<- c(0,7,14,28,42,63)
  
  percentiles_R<-data.frame(days=1:length(breaks))
  percentiles_S<-data.frame(days=1:length(breaks))
  percentiles_mixed<-data.frame(days=1:length(breaks))
  
  
  percentiles_R$genotype<-"Resistant"
  percentiles_S$genotype<-"Sensitive"
  percentiles_mixed$genotype<-"Mixed"
  
  
  percentiles_R$days<-breaks
  percentiles_S$days<-breaks
  percentiles_mixed$days<-breaks
  
  
  percentiles_R$group<-"Treated"
  percentiles_S$group<-"Treated"
  percentiles_mixed$group<-"Treated"
  
  percentiles_R$perc_0.025<-NA
  percentiles_R$perc_0.5<-NA
  percentiles_R$perc_0.975<-NA
  percentiles_S$perc_0.025<-NA
  percentiles_S$perc_0.5<-NA
  percentiles_S$perc_0.975<-NA
  percentiles_mixed$perc_0.025<-NA
  percentiles_mixed$perc_0.5<-NA
  percentiles_mixed$perc_0.975<-NA
  
  
  runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_mixed<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_treated_R[run,]<- input_sim_dfs[[run]]$N_treated_I_R[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_S[run,]<- input_sim_dfs[[run]]$N_treated_I_S[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_mixed[run,]<- input_sim_dfs[[run]]$N_treated_I_mixed[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_R$perc_0.025[time]<-quantile(runs_N_treated_R[,time], probs = c(0.025))
    percentiles_R$perc_0.5[time]<-quantile(runs_N_treated_R[,time], probs = c(0.5))
    percentiles_R$perc_0.975[time]<-quantile(runs_N_treated_R[,time],probs = c(0.975))
    percentiles_S$perc_0.025[time]<-quantile(runs_N_treated_S[,time], probs = c(0.025))
    percentiles_S$perc_0.5[time]<-quantile(runs_N_treated_S[,time], probs = c(0.5))
    percentiles_S$perc_0.975[time]<-quantile(runs_N_treated_S[,time],probs = c(0.975))
    percentiles_mixed$perc_0.025[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.025))
    percentiles_mixed$perc_0.5[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.5))
    percentiles_mixed$perc_0.975[time]<-quantile(runs_N_treated_mixed[,time],probs = c(0.975))
  }
  
  
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_mixed)
  
  
  percentiles$genotype<-as.factor(percentiles$genotype)
  
  

  infreq_10ippy_freq0.5<-ggplot() + theme_bw()+ theme(plot.title=element_text(size=12),legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank())+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=genotype),  size = 1.5, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=genotype),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    scale_colour_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey"))+
    scale_fill_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey")) +
    xlim(0, 63) +geom_vline(xintercept = breaks, alpha=0.2, color="red") +ggtitle(~bold("10ippy, infrequent followup, 50% frequency of R"))


  #####################################
  
###panel B : 1ippy, infreq followup, 50% freq
  
  
  N_sims<-1000
  
  input_sim_dfs<- readRDS("simulated_data/mixed_inf_all_1ippy_infreq_freq0.5.RData") 
 
  breaks<- c(0,7,14,28,42,63)
  
  percentiles_R<-data.frame(days=1:length(breaks))
  percentiles_S<-data.frame(days=1:length(breaks))
  percentiles_mixed<-data.frame(days=1:length(breaks))
  
  
  percentiles_R$genotype<-"Resistant"
  percentiles_S$genotype<-"Sensitive"
  percentiles_mixed$genotype<-"Mixed"
  
  
  percentiles_R$days<-breaks
  percentiles_S$days<-breaks
  percentiles_mixed$days<-breaks
  
  
  percentiles_R$group<-"Treated"
  percentiles_S$group<-"Treated"
  percentiles_mixed$group<-"Treated"
  
  percentiles_R$perc_0.025<-NA
  percentiles_R$perc_0.5<-NA
  percentiles_R$perc_0.975<-NA
  percentiles_S$perc_0.025<-NA
  percentiles_S$perc_0.5<-NA
  percentiles_S$perc_0.975<-NA
  percentiles_mixed$perc_0.025<-NA
  percentiles_mixed$perc_0.5<-NA
  percentiles_mixed$perc_0.975<-NA
  
  
  runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_mixed<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_treated_R[run,]<- input_sim_dfs[[run]]$N_treated_I_R[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_S[run,]<- input_sim_dfs[[run]]$N_treated_I_S[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_mixed[run,]<- input_sim_dfs[[run]]$N_treated_I_mixed[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_R$perc_0.025[time]<-quantile(runs_N_treated_R[,time], probs = c(0.025))
    percentiles_R$perc_0.5[time]<-quantile(runs_N_treated_R[,time], probs = c(0.5))
    percentiles_R$perc_0.975[time]<-quantile(runs_N_treated_R[,time],probs = c(0.975))
    percentiles_S$perc_0.025[time]<-quantile(runs_N_treated_S[,time], probs = c(0.025))
    percentiles_S$perc_0.5[time]<-quantile(runs_N_treated_S[,time], probs = c(0.5))
    percentiles_S$perc_0.975[time]<-quantile(runs_N_treated_S[,time],probs = c(0.975))
    percentiles_mixed$perc_0.025[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.025))
    percentiles_mixed$perc_0.5[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.5))
    percentiles_mixed$perc_0.975[time]<-quantile(runs_N_treated_mixed[,time],probs = c(0.975))
  }
  
  
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_mixed)
  
  
  percentiles$genotype<-as.factor(percentiles$genotype)
  
 
  infreq_1ippy_freq0.5<-ggplot() + theme_bw()+ theme(plot.title=element_text(size=12),legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                      panel.background = element_blank())+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=genotype),  size = 1.5, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=genotype),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    scale_colour_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey"))+
    scale_fill_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey")) +
    xlim(0, 63) +geom_vline(xintercept = breaks, alpha=0.2, color="red") +ggtitle("1ippy,"~bold("infrequent followup, 50% frequency of R"))

###############################################################
  
##panel C : 10ippy, frequent followup, 50% freq.
  
  N_sims<-1000
  
  input_sim_dfs<- readRDS("simulated_data/mixed_inf_all_10ippy_freq_freq0.5.RData") 

  
  breaks<- c(0,2,3,5,7,14,21,28,35,42,49,56,63)

  percentiles_R<-data.frame(days=1:length(breaks))
  percentiles_S<-data.frame(days=1:length(breaks))
  percentiles_mixed<-data.frame(days=1:length(breaks))
  
  
  percentiles_R$genotype<-"Resistant"
  percentiles_S$genotype<-"Sensitive"
  percentiles_mixed$genotype<-"Mixed"
  
  
  percentiles_R$days<-breaks
  percentiles_S$days<-breaks
  percentiles_mixed$days<-breaks
  
  
  percentiles_R$group<-"Treated"
  percentiles_S$group<-"Treated"
  percentiles_mixed$group<-"Treated"
  
  percentiles_R$perc_0.025<-NA
  percentiles_R$perc_0.5<-NA
  percentiles_R$perc_0.975<-NA
  percentiles_S$perc_0.025<-NA
  percentiles_S$perc_0.5<-NA
  percentiles_S$perc_0.975<-NA
  percentiles_mixed$perc_0.025<-NA
  percentiles_mixed$perc_0.5<-NA
  percentiles_mixed$perc_0.975<-NA
  
  
  runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_mixed<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_treated_R[run,]<- input_sim_dfs[[run]]$N_treated_I_R[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_S[run,]<- input_sim_dfs[[run]]$N_treated_I_S[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_mixed[run,]<- input_sim_dfs[[run]]$N_treated_I_mixed[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_R$perc_0.025[time]<-quantile(runs_N_treated_R[,time], probs = c(0.025))
    percentiles_R$perc_0.5[time]<-quantile(runs_N_treated_R[,time], probs = c(0.5))
    percentiles_R$perc_0.975[time]<-quantile(runs_N_treated_R[,time],probs = c(0.975))
    percentiles_S$perc_0.025[time]<-quantile(runs_N_treated_S[,time], probs = c(0.025))
    percentiles_S$perc_0.5[time]<-quantile(runs_N_treated_S[,time], probs = c(0.5))
    percentiles_S$perc_0.975[time]<-quantile(runs_N_treated_S[,time],probs = c(0.975))
    percentiles_mixed$perc_0.025[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.025))
    percentiles_mixed$perc_0.5[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.5))
    percentiles_mixed$perc_0.975[time]<-quantile(runs_N_treated_mixed[,time],probs = c(0.975))
  }
  
  
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_mixed)
  
  
  percentiles$genotype<-as.factor(percentiles$genotype)
  
  
 
  
  freq_10ippy_freq0.5<-ggplot() + theme_bw()+ theme(plot.title=element_text(size=12),legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    panel.background = element_blank())+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=genotype),  size = 1.5, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=genotype),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    scale_colour_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey"))+
    scale_fill_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey")) +
    xlim(0, 63) +geom_vline(xintercept = breaks, alpha=0.2, color="red") +ggtitle(bold("10ippy,")~"frequent followup," ~bold("50% frequency of R"))
  
  ############## panel D: 10ippy, infrequent followup, 10% frequency of R
  
  N_sims<-1000
  
 
  input_sim_dfs<- readRDS("simulated_data/mixed_inf_all_10ippy_infreq_freq0.1.RData") 
  
  
  breaks<- c(0,7,14,28,42,63)
  
  percentiles_R<-data.frame(days=1:length(breaks))
  percentiles_S<-data.frame(days=1:length(breaks))
  percentiles_mixed<-data.frame(days=1:length(breaks))
  
  
  percentiles_R$genotype<-"Resistant"
  percentiles_S$genotype<-"Sensitive"
  percentiles_mixed$genotype<-"Mixed"
  
  
  percentiles_R$days<-breaks
  percentiles_S$days<-breaks
  percentiles_mixed$days<-breaks
  
  
  percentiles_R$group<-"Treated"
  percentiles_S$group<-"Treated"
  percentiles_mixed$group<-"Treated"
  
  percentiles_R$perc_0.025<-NA
  percentiles_R$perc_0.5<-NA
  percentiles_R$perc_0.975<-NA
  percentiles_S$perc_0.025<-NA
  percentiles_S$perc_0.5<-NA
  percentiles_S$perc_0.975<-NA
  percentiles_mixed$perc_0.025<-NA
  percentiles_mixed$perc_0.5<-NA
  percentiles_mixed$perc_0.975<-NA
  
  
  runs_N_treated_R<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_S<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  runs_N_treated_mixed<-as.data.frame(matrix(data=NA, nrow=N_sims, ncol=length(breaks)))
  
  for (run in 1:N_sims) { 
    runs_N_treated_R[run,]<- input_sim_dfs[[run]]$N_treated_I_R[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_S[run,]<- input_sim_dfs[[run]]$N_treated_I_S[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    runs_N_treated_mixed[run,]<- input_sim_dfs[[run]]$N_treated_I_mixed[]/ (input_sim_dfs[[run]]$N_treated_I_R[] +input_sim_dfs[[run]]$N_treated_I_S[]+input_sim_dfs[[run]]$N_treated_I_mixed[]+ input_sim_dfs[[run]]$N_treated_uninf[])
    
  }
  
  for (time in 1:length(breaks)) { 
    percentiles_R$perc_0.025[time]<-quantile(runs_N_treated_R[,time], probs = c(0.025))
    percentiles_R$perc_0.5[time]<-quantile(runs_N_treated_R[,time], probs = c(0.5))
    percentiles_R$perc_0.975[time]<-quantile(runs_N_treated_R[,time],probs = c(0.975))
    percentiles_S$perc_0.025[time]<-quantile(runs_N_treated_S[,time], probs = c(0.025))
    percentiles_S$perc_0.5[time]<-quantile(runs_N_treated_S[,time], probs = c(0.5))
    percentiles_S$perc_0.975[time]<-quantile(runs_N_treated_S[,time],probs = c(0.975))
    percentiles_mixed$perc_0.025[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.025))
    percentiles_mixed$perc_0.5[time]<-quantile(runs_N_treated_mixed[,time], probs = c(0.5))
    percentiles_mixed$perc_0.975[time]<-quantile(runs_N_treated_mixed[,time],probs = c(0.975))
  }
  
  
  percentiles<-rbind(percentiles_R,percentiles_S, percentiles_mixed)
  
  
  percentiles$genotype<-as.factor(percentiles$genotype)
  
 
  infreq_10ippy_freq0.1<-ggplot() + theme_bw()+ theme(plot.title=element_text(size=12),legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank())+
    geom_line(data=percentiles, aes(x=days, y=perc_0.5, colour=genotype),  size = 1.5, alpha=0.5) +
    geom_ribbon(data=percentiles, aes(x=days, ymin=perc_0.025, ymax=perc_0.975, fill=genotype),  alpha=0.2) +
    ylab("Proportion infected") + labs(title=" ") + xlab("Follow-up (days)") +
    scale_colour_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey"))+
    scale_fill_manual(values = c( "Resistant"="#D82632", "Sensitive"="#290AD8","Mixed"="grey")) +
    xlim(0, 63) +geom_vline(xintercept = breaks, alpha=0.2, color="red") +ggtitle(bold("10ippy, infrequent followup,")~" 10% frequency of R")

  
  
  plot_grid(infreq_10ippy_freq0.5,NULL,infreq_1ippy_freq0.5,
            freq_10ippy_freq0.5,NULL, infreq_10ippy_freq0.1, 
            labels = c('A',"", 'B', 'C',"", 'D'),
            label_size = 20, ncol=3, rel_widths = c(1, 0.05, 1, 1, 0.05, 1))
  
  
