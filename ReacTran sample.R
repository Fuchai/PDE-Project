N <- 50
Grid <- setup.grid.1D(x.up = 0, x.down = 1, N = N)
x1ini <- 1 + sin(2 * pi * Grid$x.mid)
x2ini <- rep(x = 3, times = N)
yini <- c(x1ini, x2ini)
brusselator1D <- function(t, y, parms) {
    X1 <- y[1:N]
    X2 <- y[(N+1):(2*N)]
    dX1 <- 1 + X1^2*X2 - 4*X1 +
        tran.1D (C = X1, C.up = 1, C.down = 1,
                 D = 0.02, dx = Grid)$dC
    dX2 <- 3*X1 - X1^2*X2 +
        tran.1D (C = X2, C.up = 3, C.down = 3,
                 D = 0.02, dx = Grid)$dC
    list(c(dX1, dX2))
}
times <- seq(from = 0, to = 10, by = 0.1)
print(system.time(
    out <- ode.1D(y = yini, func = brusselator1D,
                  times = times, parms = NULL, nspec = 2,
                  names = c("X1", "X2"), dimens = N)
))
user system elapsed
0.23 0.00 0.25
par(mfrow = c(2, 2))
image(out, mfrow = NULL, grid = Grid$x.mid,
      which = "X1", method = "contour")
image(out, mfrow = NULL, grid = Grid$x.mid,
      which = "X1")
par(mar = c(1, 1, 1, 1))
image(out, mfrow = NULL, grid = Grid$x.mid,
      which = "X1", method = "persp", col = NA)
image(out, mfrow = NULL, grid = Grid$x.mid,
      which = "X1", method = "persp", border = NA,
      shade = 0.3 )