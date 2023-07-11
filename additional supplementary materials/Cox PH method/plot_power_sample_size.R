df<-read.csv("additional supplementary materials/Cox PH method/combos_power.csv")


A<-ggplot(df[which(df$protection==20),]) + 
  geom_line(aes(x=N_per_arm, y=power, group=factor(transmission), colour=factor(transmission)), size=1.5)+
  theme_bw()+ggtitle("Protection: 20 days") +
  scale_x_continuous(breaks=c(25,50,75,100,125,150,175,200,225,250)) +ylim(0,1)+
  scale_colour_discrete(name = "Infections per \n person per year")+
  ylab("Power")+ xlab("Sample size (per arm)")

B<-ggplot(df[which(df$protection==30),]) + 
  geom_line(aes(x=N_per_arm, y=power, group=factor(transmission), colour=factor(transmission)), size=1.5)+
  theme_bw()+ggtitle("Protection: 30 days") +
  scale_x_continuous(breaks=c(25,50,75,100,125,150,175,200,225,250))+ylim(0,1)+
  scale_colour_discrete(name = "Infections per \n person per year")+
  ylab("Power")+ xlab("Sample size (per arm)")

C<-ggplot(df[which(df$transmission==2),]) + 
  geom_line(aes(x=N_per_arm, y=power, group=factor(protection), colour=factor(protection)), size=1.5)+
  theme_bw()+ggtitle("2 ippy")+
  scale_x_continuous(breaks=c(25,50,75,100,125,150,175,200,225,250))+ylim(0,1)+
  scale_colour_discrete(name = "Duration of \n protection (days)")+
  ylab("Power")+ xlab("Sample size (per arm)")

D<-ggplot(df[which(df$transmission==4),]) + 
  geom_line(aes(x=N_per_arm, y=power, group=factor(protection), colour=factor(protection)), size=1.5)+
  theme_bw()+ggtitle("4 ippy")+
  scale_x_continuous(breaks=c(25,50,75,100,125,150,175,200,225,250))+ylim(0,1)+
  scale_colour_discrete(name = "Duration of \n protection (days)")+
  ylab("Power")+ xlab("Sample size (per arm)")


plot_grid(A,B,C,D, labels = c('A', 'B', 'C', 'D'),label_size = 15,ncol = 2)
