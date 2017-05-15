## =============================================================================
## The code for half-split wood moisture diffusion
## =============================================================================

require(reshape2)
require(ReacTran)
require(ggplot2)

# Parameters
N <- 100  # number of grid cells
Dz <- 1.30243680*10^-2 # diffusion in z axis,  4.13*10^-10m^2/s, transformed to year
r <- 0.25
maxtime <- 10
Dphi <- Dz/r/r # radius based difussion in phi axis

# Space setup

z.grid    <- setup.grid.1D(x.up = 0, x.down = 0.5, N = N)
phi.grid    <- setup.grid.1D(x.up = 0, x.down = pi, N = N)
grid2D    <- setup.grid.2D(z.grid, phi.grid)

#### You need to look at those conditions, I did not even enter boundary conditions.
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

# initial condition: 0 everywhere, except in central point
y <- matrix(nrow = N, ncol = N, data = 0.5)
# y[N2,N2] <- ini  # initial concentration in the central point...

# solve for 30 time units
times <- seq(maxtime/30,maxtime,length.out=30)
print(system.time(
outc <- ode.2D (y = y, func = Diff2Db, t = times, parms = NULL,
                dim = c(N, N), lrw = 284900300))
)
# Code to visualize the concentration at a surface at a time

snapshot<-matrix(outc[30,-1],nrow=N,ncol=N)
melted_wood<-melt(snapshot)
head(melted_wood)
ggplot(data = melted_wood, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
