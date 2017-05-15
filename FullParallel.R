## =============================================================================
## The parallel processing code for half-split wood moisture diffusion
## =============================================================================

require(reshape2)
require(ReacTran)
require(ggplot2)
require(parallel)
require(foreach)
require(abind)

# Parameters
N <- 10000  # number of grid cells
maxtime <- 10
fullprecision <-500

paramain<-function(){
    # Variables
    Dz <- 1.30243680*10^-2 # diffusion in z axis,  4.13*10^-10m^2/s, transformed to year 

    # Space setup
    z.grid    <- setup.grid.1D(x.up = 0, x.down = 0.5, N = N)
    D.grid    <- setup.prop.1D(value = Dz, grid = z.grid)
    A.grid    <- setup.prop.1D(value = 1, grid = z.grid)
    
    # The model equations - using the grids
    tenv<-function(t){ # t is in unit of a year
        0.14*sin(2*pi*t)+0.16
    }
    
    Diff1Db <- function (t, y, parms)  { # what is this y value? Y dimension?
        dCONC <- tran.1D(y, dx = z.grid, D= Dz, A = 1,
                         C.up=tenv(t), C.down=tenv(t))$dC
        return (list(dCONC))
    }
    
    # initial condition
    y <- rep(0.5, length.out=N)
    # solve for 30 time units
    times <- seq(maxtime/fullprecision,maxtime,length.out=fullprecision)
    array(ode.1D (y = y, func = Diff1Db, t = times, parms = NULL,
                  dimens = N),
          dim=c(fullprecision,N))
}

# Code to visualize the concentration at a surface at a time

#snapshot<-matrix(outc[90,-1],nrow=N,ncol=N)
#melted_wood<-melt(snapshot)
#head(melted_wood)
#ggplot(data = melted_wood, aes(x=Var1, y=Var2, fill=value)) + 
#    geom_tile()

output<-paramain()

save(output,file='fullwoodoutput')

# For one dimensional log, the integration is simply the average of all blocks

fullaveragemoisture<-output%*%matrix(1/N,nrow=N)
ggplot()+
    geom_line(aes(x=seq(1/fullprecision,maxtime,length.out = fullprecision),y=fullaveragemoisture))+
    geom_vline(xintercept = 2.9)
