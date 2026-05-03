library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
# TASK 3: Financial models often wrongly assume returns are normally distributed. 
#         Choose an index (e.g., SPY) and calculate  
#         its daily returns over the last 10 years.

getSymbols(c("^GSPC"), from = Sys.Date() - years(10), to = Sys.Date(), adjust = TRUE)
GSPC$daily_returns <- dailyReturn(Ad(GSPC))

# 3a) Calculate the 99% Daily Value at Risk (VaR) using 
#     the Parametric (Normal Distribution) method.
confidence_level <- 0.99
# Get normal distribution parameters
mu <- mean(GSPC$daily_returns)
sigma <- sd(GSPC$daily_returns)
# Get cutoff point for the left tail
z_score <- qnorm(1 - confidence_level)
# Calculate VaR as as percentage return
var_manual <-  mu + (z_score * sigma)
# Do it again with the PerformanceAnalytics library for comparison
var_library <-  as.numeric(VaR(GSPC$daily_returns, p=confidence_level, method="gaussian"))

# 3b) Count how many times the actual daily losses exceeded the Normal 99% VaR threshold. 
#     Theoretically, it should only be breached 1% of the time. 
#     What was the actual breach percentage? 
#     What does this teach you about relying strictly on normal distributions in financial modelling?
number_of_breaks <- sum(GSPC$daily_returns < var_manual)
number_of_days <- length(GSPC)
pct_of_breaks <-  number_of_breaks / number_of_days
# The actual breach % is 0.28% instead of 1%
# Own testing: 99.9% VaR
var_999 <- as.numeric(VaR(GSPC$daily_returns, p=0.999, method="gaussian"))
pct_of_breaks_999 <- sum(GSPC$daily_returns < var_999) / number_of_days
# 99.9% VaR was only broken 0.125% of times as opposed to 0.1%
# This showcases that normal distributions heavily overpredict the number
# of bad days, and also heavily underpredict the number of catastrophic days

# So in essence, real markets have a high kurtosis, where on most days
# nothing noteworthy is happening, but when something does happen,
# it is extreme, like a market crash, or an immediate jump in value.