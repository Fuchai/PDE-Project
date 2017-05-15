require(ggplot2)
setwd('~/Desktop/PDE project/')
load('halfaveragemoisture')
factor1<-halfaveragemoisture
load('halfaveragemoisture 0.5')
factor05<-halfaveragemoisture
load('halfaveragemoisture 2')
factor2<-halfaveragemoisture
load('halfaveragemoisturelength 2')
doublelength<-halfaveragemoisture
load('halfaveragemoistureradius 2')
doubleradius<-halfaveragemoisture

d2maxtime<-5
d2precision<-200
fullmaxtime<-10
fullprecision<-500
theme_set(theme_grey(base_size=15))

ggplot()+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=factor1,color="Normal"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=factor2,color="Big"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=factor05,color="Small"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=doublelength,color="Long"))+
    geom_line(aes(x=seq(d2maxtime/d2precision,d2maxtime,length.out=d2precision),
                  y=doubleradius,color="Wide"))+
    xlab("Time (year)")+
    ylab("Moisture (percentage)")+
    labs(color="Shape")+
    theme(legend.position="bottom")

ggsave("sizevariant.png")
