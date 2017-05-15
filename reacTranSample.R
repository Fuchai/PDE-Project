library(ReacTran)
Grid <- setup.grid.1D(N = 1000, L = 10)

pde1D <-function(t, C, parms)  {
    tran <- tran.1D(C = C, D = D,
                    C.down = Cext, dx = Grid)$dC
    list(tran - Q)  # return value: rate of change
}

D <- 1 # Diffusion Constant
Q <- 1 # uptake rate?
Cext <- 20

require(deSolve)
times<-seq(0,100,by=1)
system.time(
    out <- ode.1D(y = rep(1, Grid$N),
                  times = times, func = pde1D,
                  parms = NULL, nspec = 1)  # ode.1D is a function from deSolve
)
image(out, xlab = "time, days",
      ylab = "Distance, cm",
      main = "PDE", add.contour = TRUE)