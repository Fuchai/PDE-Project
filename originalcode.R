## =============================================================================
## Same 2-D model, but now with spatially-variable diffusion coefficients
## =============================================================================

N     <- 51          # number of grid cells
r     <- 0.005       # consumption rate
ini   <- 1           # initial value at x=0
N2    <- ceiling(N/2)

D.grid <- list()

# Diffusion on x-interfaces
D.grid$x.int <- matrix(nrow = N+1, ncol = N, data = runif(N*(N+1)))

# Diffusion on y-interfaces
D.grid$y.int <- matrix(nrow = N, ncol = N+1, data = runif(N*(N+1)))

dx <- 10/N
dy <- 10/N

# The model equations

Diff2Dc <- function (t, y, parms)  {
    
    CONC  <- matrix(nrow = N, ncol = N, data = y)
    
    dCONC <- tran.2D(CONC, dx = dx, dy = dy, D.grid = D.grid)$dC + r * CONC
    
    return (list(dCONC))
}

# initial condition: 0 everywhere, except in central point
y <- matrix(nrow = N, ncol = N, data = 0)
y[N2, N2] <- ini  # initial concentration in the central point...

# solve for 8 time units
times <- 0:8
outc <- ode.2D (y = y, func = Diff2Dc, t = times, parms = NULL,
                dim = c(N, N), lrw = 160000)

outtimes <- c(1, 3, 5, 7)
image(outc, ask = FALSE, mfrow = c(2, 2), main = paste("time", outtimes),
      legend = TRUE, add.contour = TRUE, subset = time %in% outtimes)