library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
# TASK 1: Download the daily returns of two historically correlated 
#         competing stocks in the same sector (e.g., Coke/Pepsi)
#         over the last 5 years.
getSymbols(c("GOOG", "MSFT"), from = Sys.Date() - years(5), to = Sys.Date(), adjust = TRUE)

r_g <- dailyReturn(Ad(GOOG))
r_m <- dailyReturn(Ad(MSFT))

# 1a) Identify the three specific days with the largest divergence in their returns (i.e., Stock A return minus Stock B return).
r_diff <- abs(r_g - r_m)
# Convert to data frame to break the chronological constraint
r_diff_df <- data.frame(Date=index(r_diff), Diff=coredata(r_diff))
r_diff_df$Diff <- as.numeric(r_diff_df$daily.returns)
# Sort the data frame descending by the difference column
sorted_diff <- r_diff_df[order(r_diff_df$Diff, decreasing = TRUE), ]

# View the highest absolute differences
head(sorted_diff, 3)

# 1b) Research and summarize the real-world news on those exact dates that caused the divergence.
# Check the md file for an answer

# 1c) Calculate a rolling 60-day correlation between the two stocks.
#     Explain mathematically how these specific outliers impacted the correlation formula
# Since correlation is defined as cor(x,y) = cov(x,y)/(sd(x)*sd(y))
# Expanding it yields: 
# cor(x,y) = (sum(x - mean(x)) * sum(y - mean(y))) / (sqrt(sum(x - mean(x))^2) * sqrt(sum(y - mean(y))^2))
# Thus, correlation is heavily dependent on each stock's divergence from its mean returns
# Covariance  specifically can only be positive so long as the two stocks move in the same direction
# Since cov(x,y) = sum(x - mean(x)) * sum(y - mean(y))
# If X successfully launches a new product, significantly increasing stock value,
# and Y simultaneously gets into a scandal which makes its shares plummet,
# then covariance will yield a huge negative number, overwhelming any previous positives
# Finally, the deviations in the denominator act as a normalizer, but since we
# must square the terms before taking their square root having such large outliers
# will cause the standard deviations have most of their weight from the outlier. 

# Calculate the 60-day rolling correlation
# runCor takes the two series and the rolling window 'n'
rolling_cor_60 <- runCor(r_g, r_m, n = 60)
tail(rolling_cor_60)
plot(rolling_cor_60, 
     main = "60-Day Rolling Correlation", 
     ylab = "Correlation", 
     col = "blue")

rolling_cov_60 <- runCov(r_g, r_m, n = 60)
tail(rolling_cov_60)
plot(rolling_cov_60, 
     main = "60-Day Rolling Covariance", 
     ylab = "Covariance", 
     col = "blue")

plot(r_g*100, main="Daily returns (%)", ylab = "Returns (%)", col="green")
lines(r_m*100, col="red")
