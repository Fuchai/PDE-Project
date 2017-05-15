## =============================================================================
## The parallel processing code for half-split wood moisture diffusion
## =============================================================================

require(reshape2)
require(ReacTran)
require(ggplot2)
require(parallel)
require(foreach)
require(doParallel)
require(abind)

# Parameters
N <- 100  # number of grid cells
rsectional<-seq(0.0025,0.25,length.out=N)
maxtime <- 5
timestepprecision<-200

paramain<-function(r){
    # Variables
    Dz <- 1.30243680*10^-2 # diffusion in z axis,  4.13*10^-10m^2/s, transformed to year 
    Dphi <- Dz/r/r # radius based difussion in phi axis 
    
    # Space setup
    z.grid    <- setup.grid.1D(x.up = 0, x.down = 0.5, N = N)
    phi.grid    <- setup.grid.1D(x.up = 0, x.down = pi, N = N)
    grid2D    <- setup.grid.2D(z.grid, phi.grid)
    D.grid    <- setup.prop.2D(value = Dz, y.value = Dphi, grid = grid2D)
    A.grid    <- setup.prop.2D(value = 1, grid = grid2D)
    
    # The model equations - using the grids
    tenv<-function(t){ # t is in unit of a year
        0.14*sin(2*pi*t)+0.16
    }
    
    Diff2Db <- function (t, y, parms)  { # what is this y value? Y dimension?
        CONC  <- matrix(nrow = N, ncol = N, data = y)
        dCONC <- tran.2D(CONC, grid = grid2D, D.grid = D.grid, A.grid = A.grid,
                         C.x.up=tenv(t), C.x.down=tenv(t), C.y.up=tenv(t), C.y.down=tenv(t))$dC
        return (list(dCONC))
    }
    
    # initial condition
    y <- matrix(nrow = N, ncol = N, data = 0.5)
    # solve for 30 time units
    times <- seq(maxtime/timestepprecision,maxtime,length.out=timestepprecision)
    array(ode.2D (y = y, func = Diff2Db, t = times, parms = NULL,
                  dim = c(N, N), lrw = 284900300),
          dim=c(timestepprecision,N,N))
}

# Code to visualize the concentration at a surface at a time

#snapshot<-matrix(outc[90,-1],nrow=N,ncol=N)
#melted_wood<-melt(snapshot)
#head(melted_wood)
#ggplot(data = melted_wood, aes(x=Var1, y=Var2, fill=value)) + 
#    geom_tile()


no_cores<-detectCores()-1
cl<-makeCluster(no_cores)
registerDoParallel(cl)  

# halfoutput is a big matrix
# first index is time 
# second index is the radius
# third and fourth index is the surface location where the box is

abindcombine<-function(a,b){
    abind(a,b,along=4)
}
# time, radius, x, y

halfoutput<-foreach(r=rsectional, 
                .combine = abindcombine, .packages='ReacTran')  %dopar%  
    paramain(r)

stopImplicitCluster()

save(halfoutput,file=paste('halfoutput',timestepprecision,sep=' '))
# Reshuffled to time, radius, phi and z (Phi and Z? A different N would make things a lot easier)
halfoutput<-aperm(halfoutput,c(1,4,2,3))

# Then we do weighted sum. We have a total of timeprecision*rsectional*N*N grids, 
# each grid carries a weight proportional to the radius
# average to the correct value, then we got it
bigr<-array(0,dim=c(timestepprecision,N,N,N))
rweight<-rsectional/sum(rsectional)
for (i in 1:N){
    bigr[,i,,]<-array(rweight[i],dim=c(timestepprecision,N,N))
}
halfoutput<-halfoutput*bigr
halfoutput<-array(halfoutput,dim=c(timestepprecision,N*N*N))
summa<-matrix(1/N/N,nrow=N*N*N)
halfaveragemoisture<-halfoutput%*%summa
qplot(x=seq(1/timestepprecision,maxtime,length.out = timestepprecision),y=halfaveragemoisture)
save(halfaveragemoisture,file='halfaveragemoisture')