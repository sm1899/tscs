# Complete Time Series Analysis Techniques Cheat Sheet

## 1. Smoothing with a Finite Moving Average Filter

Smoothing can help reduce noise and highlight trends in time series data.

```r
library(fpp2)
# Assuming 'elecsales' is your time series data
# Apply a moving average filter with q = 5
ma_smoothed <- ma(elecsales, 5)

# Visualize original and smoothed data
autoplot(elecsales, series="Data") + 
  autolayer(ma_smoothed, series="5-MA")
```

Key points:
- The `ma()` function from the fpp2 package is used for moving average smoothing.
- The second argument (5) specifies the order of the moving average (q = 5 in this case).
- Adjusting the order affects the level of smoothing: higher values result in smoother curves but may obscure short-term fluctuations.

## 2. Simple Exponential Smoothing (SES)

SES gives more weight to recent observations in smoothing.

```r
library(fpp2)
# Assuming 'elecsales' is your time series data
# Apply Simple Exponential Smoothing with alpha = 0.8
fc <- ses(elecsales, alpha=0.8)

# Visualize original and smoothed data
autoplot(elecsales, series="Data") + 
  autolayer(fitted(fc), series="SES")
```

Key points:
- The `ses()` function is used for Simple Exponential Smoothing.
- The `alpha` parameter determines the weight given to more recent observations.
- A higher alpha (closer to 1) gives more weight to recent data and produces a less smooth result.

## 3. Time Series Decomposition

### 3.1 Classical Decomposition

```r
library(stats)

# Additive decomposition
decomp_add <- decompose(your_ts_data, type = "additive")
plot(decomp_add)

# Multiplicative decomposition
decomp_mult <- decompose(your_ts_data, type = "multiplicative")
plot(decomp_mult)
```

Key points:
- Additive decomposition: Components are added together (Trend + Seasonal + Random).
- Multiplicative decomposition: Components are multiplied (Trend * Seasonal * Random).

### 3.2 X11 Decomposition

```r
library(seasonal)

x11_decomp <- seas(your_ts_data, x11 = "")
plot(x11_decomp)
```

### 3.3 SEATS Decomposition

```r
library(seasonal)

seats_decomp <- seas(your_ts_data)
plot(seats_decomp)
```

### 3.4 STL Decomposition

```r
library(stats)

stl_decomp <- stl(your_ts_data, s.window = "periodic")
plot(stl_decomp)
```

## 4. Trend Elimination by Differencing

```r
library(fpp2)
library(tseries)

# First-order differencing
diff_data <- diff(your_ts_data)

# Seasonal differencing (e.g., for monthly data)
diff_seasonal <- diff(your_ts_data, lag = 12)

# Check stationarity
adf.test(diff_data)
```

Key points:
- First-order differencing removes linear trends.
- Seasonal differencing removes seasonal patterns.
- ADF test checks for stationarity (p-value < 0.05 suggests stationarity).

## 5. Checking for White Noise and Gaussian Noise

### 5.1 Sample ACF

```r
library(forecast)

ggAcf(your_residuals)
```

### 5.2 Box-Pierce and Ljung-Box Tests

```r
Box.test(your_residuals, type = "Box-Pierce")
Box.test(your_residuals, type = "Ljung-Box")
```

### 5.3 Additional Diagnostic Tests

```r
# McLeod-Li test
Box.test(your_residuals^2, type = "Ljung-Box")

# Turning points test
library(randtests)
turning.point.test(your_residuals)

# Difference sign test
diff_signs <- diff(sign(your_residuals))
table(diff_signs)

# Rank test
rank_test <- rank(your_residuals)
```

### 5.4 Jarque-Bera Test for Gaussian Noise

```r
library(tseries)
jarque.bera.test(your_residuals)
```

### 5.5 Visual Inspection: Q-Q Plots and Histograms

```r
library(ggplot2)
library(gridExtra)

# Q-Q plot
qq_plot <- ggplot(data.frame(residuals = your_residuals), aes(sample = residuals)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot of Residuals")

# Histogram
hist_plot <- ggplot(data.frame(residuals = your_residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle("Histogram of Residuals")

grid.arrange(qq_plot, hist_plot, ncol = 2)

# Shapiro-Wilk test
shapiro.test(your_residuals)
```

Key points for all tests:
- Null hypothesis generally assumes randomness, independence, or normality of residuals.
- p-value > 0.05 typically suggests failing to reject the null hypothesis (residuals behave as expected).
- Combine multiple tests and visual inspections for comprehensive diagnostics.

Remember: This cheat sheet provides a quick reference. Always refer to detailed documentation and consider the specific context of your time series analysis when applying these techniques.