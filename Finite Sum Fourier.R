require(ggplot2)
no_terms_used<-2
k<-1.30243680*10^-2
maxtime <- 2
timestepprecision <-2

u<-function(z,t){
    sum<-0
    for (n in 1:no_terms_used){
        sum=sum+singleterm(z,t,n)
    }
    sum+0.14*sin(2*pi*t)+0.16
}


singleterm<-function(z,t,n){
    (0.34*integrate(function(x){sin(2*pi*n*x)},0,0.5)$value/
         integrate(function(x){sin(2*pi*n*x)^2},0,0.5)$value+
         integrate(function(s){qn(s,n)*exp(4*n*n*pi*pi*s)},0,t)$value)*
        exp(-4*k*n*n*pi*pi*t)*sin(2*pi*n*z)
}

singleterm<-function(z,t,n){
    
}
TT<-function(t){
    0.14*sin(2*t)
}
TTas<-function(t){
    0.28*cos(2*t)
}

averagemoisture<-function(t){
    integrate(function(m){u(m,t)},0,0.5)$value/0.5
}
h<-seq(1/timestepprecision,maxtime,length.out = timestepprecision)
x<-seq(1/timestepprecision,maxtime,length.out = timestepprecision)
y<-unlist(lapply(x,averagemoisture))

qplot(x,y)
