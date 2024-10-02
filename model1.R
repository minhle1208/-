callbs <- function(S, K, R, T, sig, q = 0) {
  d1 <- (log(S / K) + (R + q + sig^2 / 2) * T) / (sig * sqrt(T))
  d2 <- d1 - sig * sqrt(T)
  c <- S * exp(-q * T) * pnorm(d1) - K * exp(-R * T) * pnorm(d2)
  return(max(0, c))
}
put_bs <- function(S, K, R, T, sig, q = 0) {
  d1 <- (log(S / K) + (R + q + sig^2 / 2) * T) / (sig * sqrt(T))
  d2 <- d1 - sig * sqrt(T)
  p <- K * exp(-R * T) * pnorm(-d2) - S * exp(-q * T) * pnorm(-d1)
  return(max(0, p))
}

suppressPackageStartupMessages(library(quantmod))
suppressMessages(suppressWarnings(getSymbols("AAPL", from = Sys.Date() - 3, auto.assign = TRUE)))
S <- as.numeric(AAPL$AAPL.Close)[1]

suppressPackageStartupMessages(library(quantmod))
AAPL.OPTS <- getOptionChain("AAPL", NULL)
AAPL.OPTS <- AAPL.OPTS[[length(AAPL.OPTS)]]

call <- AAPL.OPTS$calls
C <- call$Last
K <- call$Strike
R <- 0.0013
# US treasury bill 2 year rate= 0.13%

year <- "2026"
month <- "12"
day <- "18"
date <- paste0(year, "-", month, "-", day)
T <- as.numeric(difftime(date, Sys.Date(), units = "days") / 365)

sig_impl <- function(S, K, R, T, C) {
  high <- 1
  low <- 0
  while ((high - low) > 0.000001) {
    if (callbs(S, K, R, T, (high + low) / 2) > C) {
      high <- (high + low) / 2
    } else {
      low <- (high + low) / 2
    }
  }
  return((high + low) / 2)
}

impl_vol <- c()
for (i in 1:nrow(call)) {
  impl_vol[i] <- sig_impl(S, K = call$Strike[i], R, T, C = call$Last[i])
}

df_c <- as.data.frame(cbind(call$Strike, impl_vol))
names(df_c)[1] <- "Strike"
suppressPackageStartupMessages(library(ggplot2))
g1 <- ggplot(data = df_c) +
  geom_point(aes(x = Strike, y = impl_vol)) +
  xlab("Strike Price") +
  ylab("Implied Volatility") +
  ggtitle("Volatility Skew for Call options")

library(gridExtra)

grid.arrange(g1, ncol = 1)
