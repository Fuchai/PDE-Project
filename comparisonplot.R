require(ggplot2)
setwd('~/Desktop/PDE project/')
load('halfaveragemoisture')
load('quarteraveragemoisture')

d2maxtime<-5
d2precision<-200
fullmaxtime<-10
fullprecision<-500
theme_set(theme_grey(base_size=15))

ggplot()+
    geom_line(aes(x=seq(fullmaxtime/fullprecision,fullmaxtime,length.out=fullprecision),
                  y=fullaveragemoisture,color="Unsplit"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=halfaveragemoisture,color="Half Split"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=quarteraveragemoisture,color="Quarter Split"))+
    geom_vline(xintercept=0.7,color='red')+
    geom_vline(xintercept = 0.63,color='green')+
    geom_vline(xintercept = 2.9,color='blue')+
    xlab("Time (year)")+
    ylab("Moisture (percentage)")+
    labs(color="Shape")+
    theme(legend.position="bottom")

ggsave("splitvariant.png")
