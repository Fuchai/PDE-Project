require(ggplot2)
require(data.table)
require(dplyr)

# This is the number of samples at each time step
N<-101
# The distance between spatial nodes
h<-1/(N-1)
tau<-.05
tmax<-7
k<-0.1

# Make the matrix A
coe1<-1+2*tau*k/h/h
coe2<--tau*k/h/h
coe3<--tau*k/h/h

A<-diag(N-2)*coe1
A<-A+rbind(0,cbind(diag(N-3),0))*coe2+rbind(cbind(0,diag(N-3)),0)*coe2
inverse<-solve(A)

# Initiation of x
x<-c()
for (i in 1:N){
    x<-c(x,h*(i-1))
}

u<-data.frame(matrix(ncol=N+1,nrow=0))
colna<-c("t",x)
colnames(u)<-colna
u<-data.table(u)

ux0<-function(x){
    sin(pi*x)
}

S<-function(x,tp1){
    sin(x*t)
}

gennextrow<-function(lastrow,currenttime){
    sliced<-lastrow[4:length(lastrow)-1]
    inverse*(sliced+S(sliced,rep(currenttime+tau,N-2)))
}

t<-seq(0,tmax,by=tau)
b<-data.table(t(c(0,ux0(x))))
colnames(b)<-colna
u<-bind_rows(u,b)
easyslice=0

for (time in t[2:length(t)]){
    easyslice=easyslice+1
    timestepdata<-c(t=time)
    timestepdata<-c(timestepdata,gennextrow(u[easyslice,],time))

    if(FALSE){
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
    }

    u<-bind_rows(u,timestepdata)
}

u[-1,'0']<-0
u[-1,'1']<-0

# The matrix A could be wrong. Seems like everything goes down to zero.


qplot(x=t,y=u[,'0.5'])
