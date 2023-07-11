

############################################################################################################################
#############################################    PANELs A and B  ###########################################################
############################################################################################################################

# set incidence: infections per person per day (convert from number of infections per year)

inc_1ippy<-1/365
inc_3ippy<-3/365
inc_5ippy<-5/365


dur<-20  ## mean duration of protection (days)
w<-5     ## shape protection parameter
lambda<- dur/ (gamma(1+(1/w)))  ##scale protection parameter


## time step for deterministic model (half a day but could use smaller)
dt=0.5  ## 

## create drug protection weibull curve
time<-seq(from=0,to=60,by=dt)      #### Ran for 63 days, 
#Protective Efficacy will depend on which time interval is being reported (e.g. could output 30-day protective efficacy or 28-day protective efficacy etc).

##probability of drug protection at each time step
p_protect<- exp(-(time/lambda)^w)


##plot(time,p_protect,type="l", col="red",ylab="prob protection")


## simulate reinfection in a control vs two chemoprevention groups in a simple difference equation set up.

##first, generate empty vectors to store proportions over time in each transmission setting:
control_1ippy<-treated_1ippy<-control_5ippy<-treated_5ippy<-control_3ippy<-treated_3ippy<-vector(length=length(time))   

##start with everyone being uninfected (at risk) at the first time point

treated_1ippy[1]<-1
control_1ippy[1]<-1  
treated_3ippy[1]<-1
control_3ippy[1]<-1 
treated_5ippy[1]<-1
control_5ippy[1]<-1 

prob_inf_1ippy<-1-exp(-inc_1ippy*dt)  ## daily prob of infection at each time step (1ippy)
prob_inf_3ippy<-1-exp(-inc_3ippy*dt)  ## daily prob of infection at each time step (3ippy)
prob_inf_5ippy<-1-exp(-inc_5ippy*dt)  ## daily prob of infection at each time step (5ippy)

for(i in 2:length(time)) {
  control_1ippy[i]<-control_1ippy[i-1] - prob_inf_1ippy*control_1ippy[i-1]  
  treated_1ippy[i]<-treated_1ippy[i-1] - prob_inf_1ippy*  treated_1ippy[i-1]*(1-p_protect[i])  
  control_3ippy[i]<-control_3ippy[i-1] - prob_inf_3ippy*control_3ippy[i-1]  
  treated_3ippy[i]<-treated_3ippy[i-1] - prob_inf_3ippy*  treated_3ippy[i-1]*(1-p_protect[i])  
  control_5ippy[i]<-control_5ippy[i-1] - prob_inf_5ippy*control_5ippy[i-1]  
  treated_5ippy[i]<-treated_5ippy[i-1] - prob_inf_5ippy*  treated_5ippy[i-1]*(1-p_protect[i])  
}

### dataframes per arm/transmission combination  
#e.g. df_c5 saves those in the control arm in a 5ippy setting and df_t5, saves those in the treated arm in a 5ippy setting and so on

df_c5<-as.data.frame(time) 
df_c5$group<- "Untreated"
df_c5$transmission<- "5ippy"

df_t5<-as.data.frame(time)
df_t5$group<- "Treated"
df_t5$transmission<- "5ippy"

df_c1<-as.data.frame(time)
df_c1$group<- "Untreated"
df_c1$transmission<- "1ippy"

df_t1<-as.data.frame(time)
df_t1$group<- "Treated"
df_t1$transmission<- "1ippy"

df_c3<-as.data.frame(time)
df_c3$group<- "Untreated"
df_c3$transmission<- "3ippy"

df_t3<-as.data.frame(time)
df_t3$group<- "Treated"
df_t3$transmission<- "3ippy"


df_c5$prop<-(1-control_5ippy)
df_t5$prop<-(1-treated_5ippy)
df_c1$prop<-(1-control_1ippy)
df_t1$prop<-(1-treated_1ippy)
df_c3$prop<-(1-control_3ippy)
df_t3$prop<-(1-treated_3ippy)

df<- rbind(df_c5, df_t5,df_c1, df_t1, df_c3,df_t3)  ##combines all data

###plot_A plots deterministic model output for control group and treated group with a drug of 20 days protection in a 3ippy setting

plot_A<-ggplot() + theme_bw()+ 
  geom_line(data=df[which(df$group=="Untreated" & df$transmission=="3ippy"),], aes(x=time, y=prop, lty= group),color = "black", size =  1.5) +
  geom_line(data=df[which(df$group=="Treated" & df$transmission=="3ippy"),], aes(x=time, y=prop, lty= group),color = "black",  size =  1.5) +
  ylab("Proportion infected")  + xlab("Days since treatment") + ggtitle("A")+
  geom_rect(aes(xmin=0, xmax=20, ymin=0, ymax=0.6), alpha=0.2)  + 
  theme(legend.key.width = unit(2, 'cm'))+ labs(lty= 'Drug protection: \n 20 days \n \n Transmission: \n 3 ippy \n in both groups \n \n \n Group:')

plot_A

#colour_object<-c("chocolate2","chocolate2","darkcyan", "darkcyan")
#override.linetype<-c("dashed", "solid","dashed", "solid")

###plot_B is similar to plot A stratified by a low transmission setting (1ippy) and a higher transmission setting (5ippy) 

plot_B<- ggplot() + theme_bw()+ 
  geom_line(data=df[which(df$group=="Untreated" & df$transmission=="5ippy"),], aes(x=time, y=prop, colour= transmission, lty= group), size = 1.5) +
  geom_line(data=df[which(df$group=="Treated" & df$transmission=="5ippy"),], aes(x=time, y=prop, colour= transmission, lty= group),  size = 1.5) +
  geom_line(data=df[which(df$group=="Untreated" & df$transmission=="1ippy"),], aes(x=time, y=prop, colour= transmission, lty= group), size = 1.5) +
  geom_line(data=df[which(df$group=="Treated" & df$transmission=="1ippy"),], aes(x=time, y=prop, colour= transmission, lty= group),  size = 1.5) +
  ylab("Proportion infected")  + xlab("Days since treatment")+ ggtitle("B")+
  geom_vline(xintercept = 23, linetype="dashed", color = "#FF8E33", size=1)+
  geom_vline(xintercept = 36, linetype="solid", color = "#FF8E33", size=1)+
  geom_vline(xintercept = 28, linetype="dashed", color = "#006666", size=1)+
  geom_vline(xintercept = 38, linetype="solid", color = "#006666", size=1)+
  scale_colour_manual(values = c( "5ippy"="#FF8E33", "1ippy"="#006666"))+
  scale_linetype_manual(values = c( "Untreated"="dashed", "Treated"="solid")) +
  geom_rect(aes(xmin=0, xmax=20, ymin=0, ymax=0.6), alpha=0.2)  + theme(legend.key.width = unit(2, 'cm'))+
  labs(color='Transmission:') +labs(lty= 'Drug protection: \n 20 days \n \n Group:')

plot_B

#Shaded area= Mean duration of drug protection.

############################################################################################################################
#############################################    PANELs C and D  ###########################################################
############################################################################################################################

####to store for different scenarios with different frequency of resistance and different mean durations of protection

df_30d_freq0.3<-as.data.frame(time)
df_30d_freq0.3$freq<- "0.3" ## linetype
df_30d_freq0.3$duration<- "30 days(S)"
df_10d_freq0.3<-as.data.frame(time)
df_10d_freq0.3$freq<- "0.3" ## linetype
df_10d_freq0.3$duration<- "10 days(R)"

df_30d_freq0.5<-as.data.frame(time)
df_30d_freq0.5$freq<- "0.5" ## linetype
df_30d_freq0.5$duration<- "30 days"
df_10d_freq0.5<-as.data.frame(time)
df_10d_freq0.5$freq<- "0.5" ## linetype
df_10d_freq0.5$duration<- "10 days"

df_30d_freq0.7<-as.data.frame(time)
df_30d_freq0.7$freq<- "0.7" ## linetype
df_30d_freq0.7$duration<- "30 days(S)"
df_10d_freq0.7<-as.data.frame(time)
df_10d_freq0.7$freq<- "0.7" ## linetype
df_10d_freq0.7$duration<- "10 days(R)"

inc<-5/365  ### incidence assumed to be 5ippy for all scenarios 

## weibull drug protection parameters
w_S<-5 ## shape parameter for protection against sensitive parasite
w_R<-5 ## shape parameter for protection against resistant parasite

mean_protect_S<-30   ## set to 30 days mean duration of protection
mean_protect_R<-10   ## set to 10 days mean duration of protection

#w<-5  ##shape parameter 

lambda_S<- mean_protect_S/ (gamma(1+(1/w_S)))            ### calculate gamma (scale parameter) based on the mean
lambda_R<- mean_protect_R/ (gamma(1+(1/w_R)))


p_protect_R<- exp(-((time)/lambda_R)^w_R)  ## probability of being protected against a resistant parasite
p_protect_S<- exp(-((time)/lambda_S)^w_S)  ## probability of being protected against a sensitive parasite

# ###frequency of resistant parasite
freq_scenarios<-c(0.3,0.5,0.7)   ## all frequency scenarios for the plots 

for (f in 1:length(freq_scenarios)) { ##  loops over the 3 frequency scenarios
  
  freq<-freq_scenarios[f]  
  
  ## simulate reinfection in a control vs chemoprevention group in a simple difference equation set up.
  treated<-treated_I_R<-treated_I_S<-vector(length=length(time))
  
  ## everyone is susceptible in the first time point
  treated[1]<-1
  
  ### nobody is infected in the first time point
  treated_I_S[1]<-0
  treated_I_R[1]<-0
  
  
  prob_inf<-1-exp(-inc*dt)  ## prob of infection at each time step
  
  for(i in 2:length(time)) {
    treated_I_S[i]<- treated_I_S[i-1]+ (prob_inf*(1-freq)*treated[i-1]*(1-p_protect_S[i]))  ## proportion of new infections with S in treated group
    treated_I_R[i]<- treated_I_R[i-1]+(prob_inf*(freq)*treated[i-1]*(1-p_protect_R[i]))     ## proportion of new infections with R in treated group
    treated[i]<-treated[i-1] -  (prob_inf*(1-freq)*treated[i-1]*(1-p_protect_S[i]))  -  
                                (prob_inf*(freq)*treated[i-1]*(1-p_protect_R[i]))           ## proportion without an infection in treated group
    
  }
  
  ### to store output for each scenario
  
  eval(parse(text = paste0("df_30d_freq",freq,"$prop<- treated_I_S")))
  eval(parse(text = paste0("df_10d_freq",freq,"$prop<- treated_I_R")))
  
}

##store all scenarios in a single dataframe

df_freq<- rbind(df_30d_freq0.7, df_10d_freq0.7,df_30d_freq0.3, df_10d_freq0.3,df_30d_freq0.5, df_10d_freq0.5)

####panel C plots proportion infected when treated with two different drugs (one that offers a 10 day protection, and one that offers 30 day protection)
#### this may also represent  the proportion with each genotype when treated with a single drug that provides 10 days protection against the resistant parasite and 30-days protection against the sensitive parasite

plot_C<- ggplot() + theme_bw()+
          geom_line(data=df_freq[which(df_freq$freq=="0.5" & df_freq$duration=="30 days"),], aes(x=time, y=prop, colour= duration), size = 1.5)+
          geom_line(data=df_freq[which(df_freq$freq=="0.5" & df_freq$duration=="10 days"),], aes(x=time, y=prop, colour= duration), size = 1.5) +
          ylab("Proportion infected")  + xlab("Days since treatment")+ ylim(0,0.4)+
          scale_colour_manual(values = c( "30 days"= "#290AD8","10 days"="#D82632"))+
          theme(legend.key.width = unit(2, 'cm'))+ ggtitle("C")+
          labs(color='Drug Protection:    ') 

### similar to panel C, stratified by frequency of resistant and sensitive strains


# df_freq$freq is the frequency of the resistant strain
df_freq$freqstrain<-df_freq$freq  ## df_freq$freqstrain is the frequency of the strain being plotted 
df_freq$freqstrain[which(df_freq$duration=="30 days(S)")]<- (1-as.numeric(df_freq$freq[which(df_freq$duration=="30 days(S)")]))

plot_D<-ggplot() + theme_bw()+
         geom_line(data=df_freq[which(df_freq$freqstrain=="0.7" & df_freq$duration=="30 days(S)"),], aes(x=time, y=prop, colour= duration, lty= freqstrain),  size = 1.5) +
         geom_line(data=df_freq[which(df_freq$freqstrain=="0.3" & df_freq$duration=="30 days(S)"),], aes(x=time, y=prop, colour= duration, lty= freqstrain), size = 1.5) +
         geom_line(data=df_freq[which(df_freq$freqstrain=="0.3" & df_freq$duration=="10 days(R)"),], aes(x=time, y=prop, colour= duration, lty= freqstrain), size = 1.5) +
         geom_line(data=df_freq[which(df_freq$freqstrain=="0.7" & df_freq$duration=="10 days(R)"),], aes(x=time, y=prop, colour= duration, lty= freqstrain),  size = 1.5) +
         ylab("Proportion infected")  + xlab("Days since treatment")+ ylim(0,0.4)+ ggtitle("D")+
         scale_colour_manual(values = c( "30 days(S)"= "#290AD8","10 days(R)"="#D82632"))+
         scale_linetype_manual(values = c( "0.3"="dashed", "0.7"="solid"))   + theme(legend.key.width = unit(2, 'cm'))+
         labs(color='Drug Protection: ') +labs(lty='Frequency of  \n strain: ')
      


Fig1<- plot_grid(plot_A,plot_B, plot_C, plot_D, ncol = 2)

# check the plot by CVD simulator
cvdPlot(Fig1)

pdf(file = "Fig1.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 7) # The height of the plot in inches
Fig1
dev.off()