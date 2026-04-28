library(quantmod)
library(PerformanceAnalytics)
library(lubridate)

# TASK 2: Download the last 10 years of daily data for a stock of your choice 
#         and the S&P 500.
#         Calculate the rolling 252-day (1-year) beta of your stock.


getSymbols(c("NVDA", "^GSPC"), from = Sys.Date() - years(10), to = Sys.Date(), adjust = TRUE)

returns <- data.frame(Date=index(NVDA), Market=coredata(dailyReturn(Ad(GSPC))), Nvidia=coredata(dailyReturn(Ad(NVDA))))
colnames(returns) <- c("Date","Market", "Nvidia")
head(returns)

rolling_corr <- runCor(returns$Nvidia, returns$Market, n=252)
# Formula: Beta = Cov(r_i, r_m) / Var(r_m)
rolling_cov <- runCov(returns$Nvidia, returns$Market, n=252)
rolling_var <- runVar(returns$Market, n=252)
rolling_var_nvda <- runVar(returns$Nvidia, n=252)

rolling_beta <- rolling_cov / rolling_var
roll_beta_xts <- xts(coredata(rolling_beta), order.by = index(NVDA))
roll_beta_xts <- na.omit(roll_beta_xts)
plot(roll_beta_xts, 
     main = "NVDA 252-Day Rolling Beta", 
     ylab = "Beta", 
     col = "darkred")

# 2a) Identify a specific period where the beta experienced a sudden "regime shift" (a sharp, sustained increase or decrease).
# Regime shift at around roll_beta_xts[700:740] from a beta of 2.2 to 1.4
# What real-world event caused this? Covid-19
rolling_cov_no_na <- na.omit(xts(coredata(rolling_cov), order.by = index(NVDA)))
rolling_var_no_na <- na.omit(xts(coredata(rolling_var), order.by = index(NVDA)))
rolling_corr_no_na <- na.omit(xts(coredata(rolling_corr), order.by = index(NVDA)))
rolling_var_nvda_no_na <- na.omit(xts(coredata(rolling_var_nvda), order.by = index(NVDA)))
plot(rolling_corr_no_na, 
     main = "NVDA 252-Day Rolling corr", 
     ylab = "Correlation", 
     col = "darkgreen")

#2b) Decompose the Beta: During your regime shift, was the change in Beta driven
# primarily by a change in the stock's correlation to the market (ρ), or 
# a change in the stock's standalone volatility (σ_stock)?
plot(rolling_var_nvda_no_na, 
     main = "NVDA 252-Day Rolling var", 
     ylab = "Variance", 
     col = "darkblue")

plot(rolling_var_no_na, 
     main = "SPY 252-Day Rolling var", 
     ylab = "Variance", 
     col = "darkblue")

# Verdict: The beta's shift was caused by a massive increase in market variance 
# and reduction in the stock's variance.
# Everything clearly corresponds to the onset of the Covid-19 pandemic's panic.
