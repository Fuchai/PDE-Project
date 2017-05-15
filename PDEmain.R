library(ReacTran)

N     <- 100         # number of grid cells
rr    <- 0.005       # consumption rate
D     <- 400

r       <- seq (2, 4, len = N+1)
theta   <- seq (0, 2*pi, len = N+1)
z       <- seq (0, 3, len = N+1)
phi     <- seq (0, 2*pi, len = N+1)

# The model equations
Diffcylin <- function (t, y, parms)  {
    CONC  <- array(dim = c(N, N, N), data = y)
    tran  <- tran.cylindrical(CONC, 
                              D.r = D, D.theta = D, D.z = D,
                              r = r, theta = theta, z = z,
                              C.r.up = 0,  C.r.down = 1,
                              cyclicBnd = 2)
    dCONC <- tran$dC  - rr * CONC
    return (list(dCONC))
}

# initial condition: 0 everywhere, except in central point
y   <- array(dim = c(N, N, N), data = 0)
N2  <- ceiling(N/2)

y[N2, N2, N2] <- 100  # initial concentration in the central point...

require(deSolve)
times<-seq(0,100,by=1)
system.time(
    out<-ode.3D(y=y,times=times,func=Diffcylin,parms=NULL,dimens=c(N,N,N),nspec=1,lrw=333530003)
)
timespaceoutput<-array(dim=c(nrow(out),N,N,N),data=out[,-1])
