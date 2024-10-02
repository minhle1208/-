suppressPackageStartupMessages(library(quantmod))
suppressMessages(suppressWarnings(getSymbols("AAPL", from = Sys.Date() - 180, auto.assign = TRUE)))

AAPL.OPTS <- getOptionChain("AAPL", NULL)
AAPL.OPTS <- AAPL.OPTS[[length(AAPL.OPTS)]]

call <- AAPL.OPTS$calls
S <- as.numeric(AAPL$AAPL.Close)[1]
C <- call$Last
K <- call$Strike
R <- 0.0013
exp_date <- call$Expiration[1]
# US treasury bill 2 year rate= 0.13%

# Start approximation
v0 <- 0.2
vT <- 0.2
rho <- 0.5
k <- 0.2
sig <- 0.05

year <- substring(exp_date, 1, 4)
month <- substring(exp_date, 6, 7)
day <- substring(exp_date, 9, 10)
date <- paste0(year, "-", month, "-", day)
time <- as.numeric(difftime(date, Sys.Date(), units = "days") / 365)


library(NMOF)
x <- c(v0, vT, rho, k, sig)
optim_func <- function(x) {
    v0 <- x[1]
    vT <- x[2]
    rho <- x[3]
    k <- x[4]
    sig <- x[5]
    mse <- (callHestoncf(
        S, K[7], time, R, 0.0106,
        v0, vT, rho, k, sig
    ) - C[7])^2
    return(mse)
}

x <- optim(x, optim_func)$par # Optimizing parametrs by first option
v0 <- x[1]
vT <- x[2]
rho <- x[3]
k <- x[4]
sig <- x[5]

pred <- c()
for (i in 1:nrow(call)) {
    pred[i] <- callHestoncf(
        S, K[i], time, R, 0.0106,
        v0, vT, rho, k, sig
    )
}

df_c <- as.data.frame(cbind(K, pred))
names(df_c)[1] <- "Strike"
suppressPackageStartupMessages(library(ggplot2))
g1 <- ggplot(data = df_c) +
    geom_point(aes(x = Strike, y = pred)) +
    xlab("Strike") +
    ylab("Predicted price") +
    ggtitle("Prediction")

df_c <- as.data.frame(cbind(K, C))
names(df_c)[1] <- "Strike"
suppressPackageStartupMessages(library(ggplot2))
g2 <- ggplot(data = df_c) +
    geom_point(aes(x = Strike, y = C)) +
    xlab("Strike") +
    ylab("True price") +
    ggtitle("Reality")

library(gridExtra)

grid.arrange(g1, g2, ncol = 2)
