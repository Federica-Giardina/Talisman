#spaghetti plots (missings)
ggplot(data = test %>% filter(type=="global" & ID %in%Castor_NA  & Set==2), 
       aes(x=as.numeric(time),y=value, col=as.factor(Treatment)))+
  geom_point()+
  geom_line()+#+geom_point()+
  facet_wrap(~ID)+scale_x_continuous(trans='log')+
  scale_color_discrete(name="")

ggplot(data = test %>% filter(type=="medurge" & ID %in%Castor_NA  & Set==2), 
       aes(x=as.numeric(time),y=value, col=as.factor(Treatment)))+
  geom_point()+
  geom_line()+#+geom_point()+
  facet_wrap(~ID)+scale_x_continuous(trans='log')+
  scale_color_discrete(name="")

