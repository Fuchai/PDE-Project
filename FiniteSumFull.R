## =============================================================================
## Finite Sum for Full log
## =============================================================================

require(ggplot2)
no_terms_used<-100
k<-1.30243680*10^-2
maxtime <- 10
finiteprecision <-1000
fullprecision<-500

u<-function(z,t){
    sum<-0
    for (n in 1:no_terms_used){
        sum=sum+singleterm(z,t,n)
    }
    sum+0.14*sin(2*pi*t)+0.16
}

upperlower<-function(n){
    upper<-(0.159155-0.159155*cos(pi*n))/n
    lower<-0.25-0.0397887*sin(2*pi*n)/n
    upper/lower
}

lambda<-function(n){
    -4*pi*pi*n*n
}

testfunction<-function(x){
    sum<-0
    for (n in 1:no_terms_used){
        sum<-sum+cn(n)*sin(2*pi*n*x)
    }
    sum
}

cn<-function(n){
    0.34*upperlower(n)
}

An<-function(n,t){
    (-0.28*pi*upperlower(n))*(integralwithnoconstant(t,n)-integralwithnoconstant(0,n))+cn(n)
}

explosioncontrol<-function(t,n){
    if (T){
        if (exp(-k*lambda(n)*t)>10000000){
            10000000
        }
        else{
            exp(-k*lambda(n)*t)
        }
    }
    else{
        exp(-k*lambda(n)*t)
    }

}

integralwithnoconstant<-function(t,n){
    explosioncontrol(t,n)*(2*pi*sin(2*pi*t)-k*lambda(n)*cos(2*pi*t))/(k*k*lambda(n)*lambda(n)+4*pi*pi)
}

singleterm<-function(x,t,n){
    An(n,t)*exp(k*lambda(n)*t)*sin(2*n*pi*x)
}

averagemoisture<-function(t){
    integrate(function(m){u(m,t)},0,0.5)$value/0.5
}

x<-seq(1/finiteprecision,maxtime,length.out = finiteprecision)
y<-unlist(lapply(x,averagemoisture))

setwd("~/Desktop/PDE project/")
load('fullwoodoutput')

theme_set(theme_grey(base_size=15))
ggplot()+
    geom_line(aes(x=x,y=y,color="Analytical"),size=1.5)+
    geom_line(aes(x=seq(1/fullprecision,maxtime,length.out=fullprecision),
                  y=fullaveragemoisture,color="Numerical"),size=1.5)+
    xlab("Time (year)")+
    ylab("Moisture (percentage)")+
    labs(color="Method")+
    geom_hline(yintercept=0.2,color='black',size=1.5)+
    geom_vline(xintercept=2.9,color='black',size=1.5)
ggsave("full.png")
