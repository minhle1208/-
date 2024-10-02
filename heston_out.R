target <- 12.36
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
    mse <- (callHestoncf(315.01, 480, 2.095776, 0.0013, 0.0106, v0, vT, rho, k, sig) - target)^2
    return(mse)
}

optim(x, optim_func)

mat <- matrix(nrow = 5, ncol = 3)
mat[1, ] <- c("v0", "current variance", 0.03401212)
mat[2, ] <- c("vT", "long-run variance", 0.19923177)
mat[3, ] <- c("rho", "correlation between spot and variance", 0.54979724)
mat[4, ] <- c("k", "speed of mean-reversion", 0.30583280)
mat[5, ] <- c("sigma", "volatility of variance", 0.08600963)
df <- as.data.frame(mat)
colnames(df) <- c("Parameters", "Meaning", "Value")
print(df)

callHestoncf(311.41, 485, 2.095776, 0.0013, 0.0106, 0.03401212, 0.19923177, 0.54979724, 0.30583280, 0.08600963)
