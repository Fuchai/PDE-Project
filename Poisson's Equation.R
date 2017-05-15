N<-5
xmax<-1
ymax<-1
sourceterm<-function(x,y){
    x^2+y^2
}
boundaryterm<-function(x,y){
    exp(x-y)+x
}

bigma<-matrix(0,nrow=(N-1)^2,ncol=(N-1)^2)


for (i in 1:(N-1)){
    for (j in 1:(N-1)){
        # (i,j) is the index of the core equation
        rownm<-(j-1)*(N-1)+i
        bigma[rownm,rownm]<- -4
        if (i>1){
            bigma[rownm,(j-1)*(N-1)+i-1]<-1}
        if (i<N-1){
            bigma[rownm,(j-1)*(N-1)+i+1]<-1}
        if (j>1){
            bigma[rownm,(j-2)*(N-1)+i]<-1}
        if (j<N-1){
            bigma[rownm,j*(N-1)+i]<-1}
    }
}

rhs<-rep(0,(N-1)^2)
# interior
for (i in 2:(N-2)){
    for (j in 2:(N-2)){
        rhs[(j-1)*(N-1)+i]<- (-sourceterm(i*xmax/N,j*ymax/N)/N/N)
    }
}
# i==1
for (j in 2:(N-2)){
    rhs[(j-1)*(N-1)+1] <- (-sourceterm(1*xmax/N,j*ymax/N)/N/N-boundaryterm(0,j*ymax/N))
}

# i==N-1
for (j in 2:(N-2)){
    rhs[(j-1)*(N-1)+N-1] <- (-sourceterm((N-1)*xmax/N,j*ymax/N)/N/N-boundaryterm(xmax,j*ymax/N))
}

# 4 j==1
for (i in 2:(N-2)){
    rhs[i] <- (-sourceterm(i*xmax/N,1*ymax/N)/N/N-boundaryterm(i*xmax/N,0))
}

# 5 j==N-1
for (i in 2:(N-2)){
    rhs[(N-2)*(N-1)+i] <- (-sourceterm(i*xmax/N,(N-1)*ymax/N)/N/N-boundaryterm(i*xmax/N,ymax))
}

# 6 i=1, j=1
rhs[1] <- (-sourceterm(1,1)/N/N-boundaryterm(0,ymax/N)-boundaryterm(xmax/N,0))

# 7 i=1, j=N-1
rhs[(N-2)*(N-1)+1] <- (-sourceterm(1,N-1)/N/N-boundaryterm(0,ymax/N)-boundaryterm(xmax/N,ymax))

# 8
rhs[N-1] <- (-sourceterm((N-1)*xmax/N,ymax/N)/N/N-boundaryterm(xmax,ymax/N)-boundaryterm((N-1)*xmax/N,0))

# i=j=N-1
rhs[(N-2)*(N-1)+N-1] <- (-sourceterm((N-1)*xmax/N,(N-1)*ymax/N)/N/N-boundaryterm(xmax,(N-1)*ymax/N)-boundaryterm(xmax,(N-1)*ymax/N))

meshvalue<-solve(bigma)%*%rhs
