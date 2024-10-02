callbs <- function(S, K, R, time, sig, q = 0) {
  d1 <- (log(S / K) + (R + q + sig^2 / 2) * time) / (sig * sqrt(time))
  d2 <- d1 - sig * sqrt(time)
  c <- S * exp(-q * time) * pnorm(d1) - K * exp(-R * time) * pnorm(d2)
  return(max(0, c))
}

suppressPackageStartupMessages(library(quantmod))
suppressMessages(suppressWarnings(getSymbols("AAPL", from = Sys.Date() - 180, auto.assign = TRUE)))


AAPL2 <- getSymbols("AAPL", auto.assign = FALSE, from = Sys.Date() - 180)
ret <- diff(log(AAPL2$AAPL.Close))
ret[1] <- 0
vol <- sd(ret)

AAPL.OPTS <- getOptionChain("AAPL", NULL)
AAPL.OPTS <- AAPL.OPTS[[length(AAPL.OPTS)]]

call <- AAPL.OPTS$calls
S <- as.numeric(AAPL$AAPL.Close)[1]
C <- call$Last
K <- call$Strike
R <- 0.0013
exp_date <- call$Expiration[1]
# US treasury bill 2 year rate= 0.13%

year <- substring(exp_date, 1, 4)
month <- substring(exp_date, 6, 7)
day <- substring(exp_date, 9, 10)
date <- paste0(year, "-", month, "-", day)
time <- as.numeric(difftime(date, Sys.Date(), units = "days") / 365)



pred <- c()
for (i in 1:nrow(call)) {
  pred[i] <- callbs(S, K[i], R, time, vol)
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