distance.weight <- function(x, xy, tau) {
  # x is a vector location
  # xy is an array of locations, one per row
  # tau is the bandwidth
  # Returns a vector of weights
  apply(xy, 1, function(z) exp(-(z-x) %*% (z-x) / (2 * tau^2)))
}

covariance <- function(y, weights) {
  # y is an m by n matrix
  # weights is length m
  # Returns the weighted covariance matrix of y (by columns).
  if (missing(weights)) return (cov(y))
  w <- zapsmall(weights / sum(weights)) # Standardize the weights
  y.bar <- apply(y * w, 2, sum)         # Compute column means
  z <- t(y) - y.bar                     # Remove the means
  z %*% (w * t(z))  
}

correlation <- function(y, weights) {
  z <- covariance(y, weights)
  sigma <- sqrt(diag(z))       # Standard deviations
  z / (sigma %o% sigma)
}

gw.pca <- function(x, xy, y, tau) {
  # x is a vector denoting a location
  # xy is a set of locations as row vectors
  # y is an array of attributes, also as rows
  # tau is a bandwidth
  # Returns a `princomp` object for the geographically weighted PCA
  # ..of y relative to the point x.
  w <- distance.weight(x, xy, tau)
  princomp(covmat=correlation(y, w))
}



n.data <- 4933
n.vars <- 83
xy1 <- matrix(rnorm(n.data * 2), ncol=2)
y1 <- matrix(rnorm(n.data * n.vars), ncol=n.vars)
xy<-Coords
y<- as.matrix(data1)

xmin <- min(xy[,1]); xmax <- max(xy[,1]); n.cols <- 30
ymin <- min(xy[,2]); ymax <- max(xy[,2]); n.rows <- 20
dx <- seq(from=xmin, to=xmax, length.out=n.cols)
dy <- seq(from=ymin, to=ymax, length.out=n.rows)
points <- cbind(rep(dx, length(dy)),
                as.vector(sapply(rev(dy), function(u) rep(u, length(dx)))))
x<-1:83
# Illustrate GWPCA by obtaining all eigenvalues at each grid point.
system.time(z <- apply(points, 1, function(x) gw.pca(x, xy, y, 1)$sdev))


library("raster")
to.raster <- function(u) raster(matrix(u, nrow=n.cols), 
                                xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
maps <- apply(z, 1, to.raster)
par(mfrow=c(2,2))
tmp <- lapply(maps, function(m) {plot(m); points(xy, pch=19)})