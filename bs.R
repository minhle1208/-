AAPL2 <- getSymbols("AAPL", auto.assign = FALSE, from = Sys.Date() - 180)
ret <- diff(log(AAPL2$AAPL.Close))
ret[1] <- 0
vol <- sd(ret)
callbs(311.41, 120, 0.0013, 2.095776, vol, 0.0106)

AAPL2 <- getSymbols("AAPL", auto.assign = FALSE, from = Sys.Date() - 180)
ret <- diff(log(AAPL2$AAPL.Close))
ret[1] <- 0
vol <- sd(ret)
callbs(311.41, 485, 0.0013, 2.095776, vol, 0.0106)
