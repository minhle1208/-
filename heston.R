target <- 192.40
v0 <- 0.2
vT <- 0.2
rho <- 0.5
k <- 0.2
sig <- 0.05

library(NMOF)
x <- c(v0, vT, rho, k, sig)
optim_func <- function(x) {
    v0 <- x[1]
    vT <- x[2]
    rho <- x[3]
    k <- x[4]
    sig <- x[5]
    mse <- (callHestoncf(315.01, 120, 2.095776, 0.0013, 0.0106, v0, vT, rho, k, sig) - target)^2
    return(mse)
}

optim(x, optim_func)

mat <- matrix(nrow = 5, ncol = 3)
mat[1, ] <- c("v0", "current variance", 0.20672451)
mat[2, ] <- c("vT", "long-run variance", 0.22078271)
mat[3, ] <- c("rho", "correlation between spot and variance", 0.50266063)
mat[4, ] <- c("k", "speed of mean-reversion", 0.22241348)
mat[5, ] <- c("sigma", "volatility of variance", 0.03788036)
df <- as.data.frame(mat)
colnames(df) <- c("Parameters", "Meaning", "Value")
print(df)

callHestoncf(311.41, 120, 2.095776, 0.0013, 0.0106, 0.20672451, 0.22078271, 0.50266063, 0.22241348, 0.03788036)
