require(ggplot2)
require(dplyr)
require(data.table)

# This is the number of samples at each time step
N<-101
# The distance between spatial nodes
h<-1/(N-1)
tau<-.05
tmax<-7

x<-c()
# Initiation of x
for (i in 1:N){
    x<-c(x,h*(i-1))
}

u<-data.frame(matrix(ncol=N+1,nrow=0))
colna<-c("t",x)
colnames(u)<-colna
u<-data.table(u)

ux0<-function(x){
    5/(1+x^2)
}

tetris<-function(a,b,c,x,t){
    a+tau*(0.5*(c-2*b+a)/h/h+x/(1+t**2))
}

t<-seq(0,tmax,by=tau)
b<-data.table(t(c(0,ux0(x))))
colnames(b)<-colna
u<-bind_rows(u,b)

for (time in t[2:length(t)]){
    timestepdata<-c(t=time)
    for (xx in x){
        if (xx==0 || xx==1){
            timestepdata<-c(timestepdata,0)
        }
        else{
            lasttime<-time-tau
            x1<-toString(xx-h)
            x2<-toString(xx)
            x3<-toString(xx+h)
            lastrun<-u[t==lasttime,]
            output<-tetris(lastrun[x1],lastrun[x2],lastrun[x3],xx,lasttime)
            timestepdata<-c(timestepdata,unlist(output))
        }
    }
    u<-rbind(u,timestepdata)
}

