#spaghetti plots (missings)
ggplot(data = SRS_combined_long %>% filter(type=="global" & ID %in%Castor_NA  & Set==2), 
       aes(x=as.numeric(time),y=value, col=as.factor(Treatment)))+
  geom_point()+
  geom_line()+#+geom_point()+
  facet_wrap(~ID)+scale_x_continuous(trans='log')+
  scale_color_discrete(name="")

ggplot(data = SRS_combined_long  %>% filter(type=="medurge" & ID %in%Castor_NA  & Set==2), 
       aes(x=as.numeric(time),y=value, col=as.factor(Treatment)))+
  geom_point()+
  geom_line()+#+geom_point()+
  facet_wrap(~ID)+scale_x_continuous(trans='log')+
  scale_color_discrete(name="")

