library(reshape2)
smalloutput<-output[,seq(1,10000,10)]
melted_wood<-melt(smalloutput)
head(melted_wood)
theme_set(theme_grey(base_size=15))
ggplot() + 
    geom_tile(data = melted_wood, aes(x=Var1/50, y=Var2/2000,fill=value),show.legend = T)+
    scale_fill_continuous(limit=c(0,0.5))+
    xlab("Time (year)")+
    ylab("Longitudinal position (meter)")+
    labs(fill='Moisture (percentage)')+
    theme(legend.position="bottom",legend.key.width=unit(1,"cm"))
ggsave(file='ripple.png')
