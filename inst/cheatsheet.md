# R Time Series Analysis Cheat Sheet

## 1. Simple Random Walk

```r
set.seed(123)
TT <- 100
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
## compute values through TT
for (t in 2:TT) {
  xx[t] <- xx[t - 1] + ww[t]
}
par(mfrow = c(1, 2))
plot.ts(xx, ylab = expression(italic(x[t])))
```

This code generates and plots a simple random walk.

## 2. Smoothing with a Moving Average Filter

```r
library(fpp2)
ma(elecsales,5)
autoplot(elecsales, series="Data") + autolayer(ma(elecsales,5), series="5-MA")
```

This code demonstrates smoothing using a finite moving average filter with q = 5.

## 3. Simple Exponential Smoothing

```r
library(fpp2)
fc <- ses(elecsales, alpha=0.8)
autoplot(elecsales, series="Data") + autolayer(fitted(fc), series="SES")
```

This code applies Simple Exponential Smoothing (SES) for estimating trends in time series data.

## 4. Additive Time Series Model

```r
library(fpp2)
autoplot(a10)
plot(decompose(a10, type = "additive"))
```

This code creates and visualizes an additive time series model, decomposing the series into its components.



## 5. Time Series Decomposition Methods

### 5.1 X11 Decomposition

```r
library(seasonal)
x11_decomp <- seas(elecsales, x11 = "")
autoplot(x11_decomp) + ggtitle("X11 Decomposition of Electrical Equipment Sales")
```

X11 decomposition is a specific technique used for seasonal adjustment, particularly popular with official statistics agencies. It's an iterative method that estimates trend-cycle, seasonal, and irregular components.

### 5.2 SEATS Decomposition

```r
library(seasonal)
seats_decomp <- seas(elecsales)
autoplot(seats_decomp) + ggtitle("SEATS Decomposition of Electrical Equipment Sales")
```

SEATS (Seasonal Extraction in ARIMA Time Series) decomposition is based on ARIMA modelling. It provides a model-based alternative to X11, often used in conjunction with TRAMO (Time Series Regression with ARIMA Noise, Missing Observations and Outliers).

### 5.3 STL Decomposition

```r
library(fpp2)
stl_decomp <- stl(elecsales, s.window = "periodic")
autoplot(stl_decomp) + ggtitle("STL Decomposition of Electrical Equipment Sales")
```

STL (Seasonal and Trend decomposition using Loess) is a versatile and robust method for decomposing time series. It can handle any type of seasonality, and the smoothness of the trend-cycle can be controlled by the user.

Each of these methods has its strengths:
- X11 is widely used in official statistics.
- SEATS offers a model-based approach.
- STL is flexible and robust, handling different types of seasonality well.

The choice between them often depends on the specific characteristics of your time series and the requirements of your analysis.

## 6. Checking for White Noise: The Sample ACF

```r
library(itsmr)
plotc(wine)
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
test(e)
```

### Remarks:

1. This code demonstrates how to check for white noise in a time series using the Sample Autocorrelation Function (ACF).

2. `library(itsmr)`: Loads the 'itsmr' package, which provides functions for time series analysis.

3. `plotc(wine)`: Plots the 'wine' time series data.

4. `M = c("log","season",12,"trend",1)`: Specifies a model for the time series:
   - "log": Apply log transformation
   - "season",12: Seasonal component with period 12 (e.g., monthly data)
   - "trend",1: Linear trend

5. `e = Resid(wine,M)`: Calculates the residuals after fitting the specified model to the 'wine' data.

6. `test(e)`: Performs statistical tests on the residuals, including the ACF and PACF (Partial Autocorrelation Function).

7. The ACF and PACF plots help identify if the residuals are white noise:
   - If the residuals are white noise, the ACF and PACF should show no significant correlations at any lag (except at lag 0).
   - Significant correlations at certain lags suggest that there's still some structure in the data that hasn't been captured by the model.

8. White noise residuals are desirable because they indicate that the model has captured all the systematic patterns in the data, leaving only random fluctuations.

9. If the residuals are not white noise, you may need to modify your model or consider using a different type of model to better capture the data's structure.

## 7. Box-Pierce Test for Residuals

```r
library(itsmr)
plotc(wine)
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
library(feasts)
box_pierce(e)
```

### Remarks:

1. This code demonstrates how to perform a Box-Pierce test on the residuals of a time series model.

2. `library(itsmr)`: Loads the 'itsmr' package for time series analysis.

3. `plotc(wine)`: Plots the 'wine' time series data.

4. `M = c("log","season",12,"trend",1)`: Specifies a model for the time series:
   - "log": Apply log transformation
   - "season",12: Seasonal component with period 12 (e.g., monthly data)
   - "trend",1: Linear trend

5. `e = Resid(wine,M)`: Calculates the residuals after fitting the specified model to the 'wine' data.

6. `library(feasts)`: Loads the 'feasts' package, which provides the Box-Pierce test function.

7. `box_pierce(e)`: Performs the Box-Pierce test on the residuals.

8. The output shows two values:
   - bp_stat: The Box-Pierce test statistic (15.09339 in this case)
   - bp_pvalue: The p-value of the test (0.0001023208 in this case)

9. Interpretation:
   - The Box-Pierce test checks whether a set of residuals are independently distributed (i.e., white noise).
   - The null hypothesis is that the residuals are independently distributed.
   - A small p-value (typically < 0.05) suggests that the residuals are not white noise.
   - In this case, the very small p-value (0.0001023208) indicates strong evidence against the null hypothesis.
   - This suggests that there may still be some autocorrelation in the residuals, and the model might not have captured all the patterns in the data.

10. If the Box-Pierce test indicates that the residuals are not white noise, you may need to:
    - Modify your model (e.g., add more terms or use a different type of model)
    - Investigate other factors that might be influencing your time series
    - Consider using a more complex time series model (e.g., ARIMA models)


## 8. Comprehensive Residual Diagnostics

```r
library(itsmr)
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
test(e)
```

Output:
```
Null hypothesis: Residuals are iid noise.
Test                Distribution Statistic  p-value
Ljung-Box Q         Q ~ chisq(20)    71.57  0 *
McLeod-Li Q         Q ~ chisq(20)    12.07  0.9138
Turning points T    (T-93.3)/5 ~ N(0,1)  93  0.9468
Diff signs S     (S-70.5)/3.5 ~ N(0,1)  70  0.8848
Rank P       (P-5005.5)/283.5 ~ N(0,1) 5136  0.6453
```

### Remarks:

1. This code performs a series of diagnostic tests on the residuals of a time series model.

2. `library(itsmr)`: Loads the 'itsmr' package for time series analysis.

3. `M = c("log","season",12,"trend",1)`: Specifies a model for the time series (log transformation, seasonal component with period 12, and linear trend).

4. `e = Resid(wine,M)`: Calculates the residuals after fitting the specified model to the 'wine' data.

5. `test(e)`: Performs multiple diagnostic tests on the residuals.

6. The output includes several tests, each testing different aspects of the residuals:

   a. Ljung-Box Q test:
      - Tests for autocorrelation in the residuals.
      - Significant p-value (0 *) suggests the residuals are not independent.

   b. McLeod-Li Q test:
      - Tests for heteroscedasticity (changing variance over time).
      - Non-significant p-value (0.9138) suggests constant variance.

   c. Turning points T test:
      - Tests for randomness in the sequence of residuals.
      - Non-significant p-value (0.9468) suggests random sequencing.

   d. Diff signs S test:
      - Another test for randomness, based on the signs of consecutive differences.
      - Non-significant p-value (0.8848) supports randomness.

   e. Rank P test:
      - Tests whether the ranks of the residuals are random.
      - Non-significant p-value (0.6453) supports randomness.

7. Interpretation:
   - The Ljung-Box Q test suggests there might still be some autocorrelation in the residuals.
   - However, the other tests (McLeod-Li Q, Turning points, Diff signs, and Rank) all suggest that the residuals exhibit properties of independent, identically distributed (iid) noise.
   - This mixed result indicates that while the model captures most of the structure in the data, there might be some remaining patterns that could potentially be modeled.

8. Next steps:
   - Investigate the nature of the remaining autocorrelation (e.g., at which lags).
   - Consider modifying the model or trying a different type of model (e.g., ARIMA) to capture the remaining patterns.
   - If the autocorrelation is minor, and the other tests show good results, the current model might be sufficient depending on the analysis goals.

## 9. Checking for Gaussian Noise

### 9.1 QQ Plot

It can be done using the QQ plot. If the normal assumption is correct, the Gaussian QQ plot should be approximately linear.

```r
library(itsmr)
M = c("log", "season", 12, "trend", 1)
e = Resid(wine, M)
qqnorm(e)
qqline(e, col = "red")
```

### Remarks on QQ Plot:
- The QQ (Quantile-Quantile) plot is a graphical tool to assess whether a set of data plausibly came from a theoretical distribution (in this case, the normal distribution).
- If the points on the QQ plot fall approximately on a straight line, it suggests that the data follow a normal distribution.
- Deviations from the straight line indicate departures from normality:
  - S-shaped curves suggest skewness
  - Curves at the ends suggest heavy or light tails compared to the normal distribution

### 9.2 Jarque-Bera Test

```r
library(itsmr)
M = c("log", "season", 12, "trend", 1)
e = Resid(wine, M)
library(tseries)
jarque.bera.test(e)
```

### Remarks on Jarque-Bera Test:
1. The Jarque-Bera test is a statistical test of the hypothesis that sample data have the skewness and kurtosis matching a normal distribution.

2. Null hypothesis: The data is normally distributed.

3. Interpretation of results:
   - If the p-value is less than your chosen alpha level (typically 0.05), reject the null hypothesis. This suggests the data is not normally distributed.
   - If the p-value is greater than your chosen alpha level, fail to reject the null hypothesis. This suggests the data could plausibly come from a normal distribution.

4. The test statistic is calculated based on the sample skewness and kurtosis.

5. Limitations:
   - The test is more sensitive to deviations from normality in large samples.
   - It may not be very powerful for small sample sizes.

6. It's often good practice to use both graphical methods (like QQ plots) and formal tests (like Jarque-Bera) when assessing normality.

7. If the residuals are found to be non-normal, you might consider:
   - Transforming your data
   - Using a different model that doesn't assume normality
   - Using robust statistical methods that are less sensitive to departures from normality

# R Time Series Analysis Cheat Sheet

[Previous content remains unchanged]

## 10. Stationary and Non-stationary Time Series

```r
n <- 100
T1 <- rnorm(n,0,1)  ##IID Noise
T2 <- cumsum(Noise)  ##Random walk
Z <- T1+0.5
T3 <- cumsum(Z)  ##Sum of iid general normal variables with mean 0.5
plot.ts(T1, ylim=c(-15,45), ylab="Stationary and Non Stationary")
lines(T2, col=5)  ##Plotting T2
lines(T3,lty=6, col=4)  ##Plotting T3
lines(0.5*(1:n), col=3, lty="dashed")  ##Plotting mean of T3
lines(0.0*(1:n), col=2,lwd=2)  ## Plotting mean of noise
```

### Remarks:

1. This code generates and plots three different types of time series to illustrate the concepts of stationarity and non-stationarity.

2. `T1 <- rnorm(n,0,1)`: 
   - Generates IID (Independent and Identically Distributed) Noise.
   - This is a stationary series with constant mean (0) and variance (1).

3. `T2 <- cumsum(Noise)`:
   - Creates a Random Walk by cumulatively summing the noise.
   - This is a non-stationary series as its variance increases over time.

4. `T3 <- cumsum(Z)` where `Z <- T1+0.5`:
   - Generates a sum of IID general normal variables with mean 0.5.
   - This is also non-stationary, with both mean and variance changing over time.

5. The `plot.ts()` function creates a time series plot of T1 (the stationary noise).

6. `lines()` functions are used to add the other series to the plot:
   - T2 is plotted in color 5 (cyan by default)
   - T3 is plotted in color 4 (blue) with line type 6 (twodash)
   - The mean of T3 (0.5*(1:n)) is plotted as a dashed line in color 3 (green)
   - The mean of the noise (0.0*(1:n)) is plotted as a thick line in color 2 (red)

7. Interpretation:
   - T1 (noise) will fluctuate around 0 with constant variance - stationary
   - T2 (random walk) will wander away from its starting point - non-stationary
   - T3 will show an upward trend due to its positive mean - non-stationary
   - The dashed line shows the deterministic trend of T3
   - The thick horizontal line at 0 represents the constant mean of T1

8. This visualization helps to understand:
   - Stationary series (T1) have constant statistical properties over time
   - Non-stationary series (T2, T3) have statistical properties that change over time
   - Different types of non-stationarity: random walk (T2) vs. deterministic trend (T3)

9. Stationarity is an important concept in time series analysis because many statistical procedures assume that the data is stationary. Non-stationary data often needs to be transformed (e.g., differencing) before analysis.

# R Code for ACF Plot

```R
# Set the number of observations
n <- 100

# Generate white noise (IID Noise)
T1 <- rnorm(n, 0, 1)  # Mean 0, standard deviation 1

# Create a random walk by cumulative sum of white noise
T2 <- cumsum(T1)  # Random walk

# Add a constant to T1
z <- T1 + 0.5

# Create a third series: cumulative sum of z
T3 <- cumsum(z)  # Sum of iid general normal variables with mean 0.5

# Plot ACF for T1 (White Noise)
acf(T1, type = "correlation", plot = T)

# Plot ACF for T2 (Random Walk)
acf(T2, type = "correlation", plot = T)

# Plot ACF for T3 (Cumulative Sum with Drift)
acf(T3, type = "correlation", plot = T)
```

## Remarks

1. The code generates three different time series:
   - T1: White noise (independent and identically distributed normal random variables)
   - T2: A random walk (cumulative sum of white noise)
   - T3: Cumulative sum of T1 plus a constant (0.5)

2. For each series, it plots the autocorrelation function (ACF) using the `acf()` function.

3. The ACF plots will show how each series correlates with itself at different lag times:
   - For T1 (white noise), we expect no significant autocorrelation except at lag 0.
   - For T2 (random walk), we expect high autocorrelation that decays slowly.
   - For T3 (cumulative sum with drift), we expect high autocorrelation that decays even more slowly than T2.

4. The `type = "correlation"` argument specifies that we want correlation-based ACF (as opposed to covariance-based).

5. The `plot = T` argument tells R to display the plot.

This code is useful for demonstrating the different autocorrelation structures of various time series types.


# MA(q) Process: R Code for Simulated Data and ACF Plots

```R
# Set the number of observations
n <- 500

# Simulate MA(1) process: X_t = Z_t + 0.7Z_{t-1}
ma1 <- arima.sim(list(order=c(0,0,1), ma=0.7), n)
ts.plot(ma1)
acf(ma1, type = "correlation", plot = T)

# Simulate MA(2) process: X_t = Z_t - 0.3Z_{t-1} + 0.8Z_{t-2}
ma2 <- arima.sim(list(order=c(0,0,2), ma=c(-0.3,0.8)), n)
ts.plot(ma2)
acf(ma2, type = "correlation", plot = T)

# Generate white noise for comparison
wn <- rnorm(n, 0, 1)
ts.plot(wn)
acf(wn, type = "correlation", plot = T)
```

## Remarks

1. **Sample Size**: We're using 500 observations (`n <- 500`) for each process, which should provide a good representation of the process characteristics.

2. **MA(1) Process**:
   - Model: X_t = Z_t + 0.7Z_{t-1}
   - We use `arima.sim()` with `order=c(0,0,1)` and `ma=0.7` to simulate this process.
   - The ACF plot for an MA(1) process should show a significant spike at lag 1, then drop to near zero for higher lags.

3. **MA(2) Process**:
   - Model: X_t = Z_t - 0.3Z_{t-1} + 0.8Z_{t-2}
   - We use `arima.sim()` with `order=c(0,0,2)` and `ma=c(-0.3,0.8)` to simulate this process.
   - The ACF plot for an MA(2) process should show significant spikes at lags 1 and 2, then drop to near zero for higher lags.

4. **White Noise**:
   - Included for comparison.
   - The ACF plot for white noise should show no significant autocorrelation at any lag except 0.

5. **Plotting**:
   - `ts.plot()` is used to visualize the time series data.
   - `acf()` with `type = "correlation"` and `plot = T` creates and displays the autocorrelation function plot.

6. **Interpretation**:
   - The MA(1) process will show some short-term dependency (one lag).
   - The MA(2) process will show slightly longer short-term dependency (two lags).
   - Comparing these to white noise helps illustrate the autocorrelation structures introduced by the MA processes.

This code allows you to simulate and visualize MA(q) processes, comparing their behavior and autocorrelation structures to each other and to white noise.


# AR(p) Process: R Code for Simulated Data, ACF, and PACF Plots

```R
# Set the number of observations
n <- 500

# Simulate AR(1) process: X_t = 0.9X_{t-1} + Z_t
AR1 <- arima.sim(list(order=c(1,0,0), ar=0.9), n=500)
ts.plot(AR1, main="AR(1) Process: X_t = 0.9X_{t-1} + Z_t")
acf(AR1, main="ACF of AR(1) Process")
pacf(AR1, main="PACF of AR(1) Process")

# Simulate AR(2) process: X_t = 0.5X_{t-1} - 0.3X_{t-2} + Z_t
AR2 <- arima.sim(list(order=c(2,0,0), ar=c(0.5, -0.3)), n=500)
ts.plot(AR2, main="AR(2) Process: X_t = 0.5X_{t-1} - 0.3X_{t-2} + Z_t")
acf(AR2, main="ACF of AR(2) Process")
pacf(AR2, main="PACF of AR(2) Process")
```

## Remarks

1. **Sample Size**: We're using 500 observations for each process, providing a good representation of the process characteristics.

2. **AR(1) Process**:
   - Model: X_t = 0.9X_{t-1} + Z_t
   - We use `arima.sim()` with `order=c(1,0,0)` and `ar=0.9` to simulate this process.
   - The time series plot shows clear autocorrelation, with values tending to persist above or below the mean for extended periods.
   - The ACF plot for an AR(1) process should show a gradual, exponential decay.
   - The PACF plot should show a significant spike at lag 1, then drop to near zero for higher lags.

3. **AR(2) Process**:
   - Model: X_t = 0.5X_{t-1} - 0.3X_{t-2} + Z_t
   - We use `arima.sim()` with `order=c(2,0,0)` and `ar=c(0.5, -0.3)` to simulate this process.
   - The time series plot may show more complex patterns due to the interaction of two lags.
   - The ACF plot for an AR(2) process typically shows a mixture of exponential decay and damped oscillation.
   - The PACF plot should show significant spikes at lags 1 and 2, then drop to near zero for higher lags.

4. **Plotting Functions**:
   - `ts.plot()` visualizes the time series data.
   - `acf()` creates the autocorrelation function plot.
   - `pacf()` creates the partial autocorrelation function plot.

5. **Interpretation**:
   - AR processes show dependency on past values, with the order (p) indicating how many past values directly influence the current value.
   - The ACF of AR processes typically shows a more gradual decay compared to MA processes.
   - The PACF is particularly useful for identifying the order of an AR process, as it "cuts off" after lag p for an AR(p) process.

6. **Comparison to MA Processes**:
   - Unlike MA processes, AR processes can show long-term dependency structures.
   - The ACF of AR processes typically decays more slowly than that of MA processes.
   - The PACF of AR processes shows a sharp cutoff, whereas the ACF of MA processes shows this behavior.

This code allows you to simulate and visualize AR(p) processes, comparing their behavior and correlation structures through time series plots, ACF, and PACF. The combination of these plots is crucial for understanding and identifying the characteristics of autoregressive processes in time series analysis.

Model	ACF	PACF
AR(p)	Tails off	Cuts off after lag p
MA(q)	Cuts off after lag q	Tails off
ARMA(p, q)	Tails off	Tails off

if PACF cuts off and ACF tails off: AR(p)
If ACF cuts off and PACF tails off: MA(q)
If both ACF tails off and PACF cuts off: ARMA(p, q)


# Simulate ARIMA(1, 1, 0)
AR1_1 <- arima.sim(list(order=c(1,1,0), ar=0.5), n=500)
ts.plot(AR1_1, main="ARIMA(1, 1, 0) Process: X_t = 0.5X_{t-1} + Z_t")
acf(AR1_1, main="ACF of ARIMA(1, 1, 0) Process")
pacf(AR1_1, main="PACF of ARIMA(1, 1, 0) Process")

# Simulate ARIMA(2, 1, 0)
AR2_1 <- arima.sim(list(order=c(2,1,0), ar=c(0.5, -0.3)), n=500)
ts.plot(AR2_1, main="ARIMA(2, 1, 0) Process: X_t = 0.5X_{t-1} - 0.3X_{t-2} + Z_t")
acf(AR2_1, main="ACF of ARIMA(2, 1, 0) Process")
pacf(AR2_1, main="PACF of ARIMA(2, 1, 0) Process")

| Model         | Stationarity Condition                                        | Invertibility Condition                                   | Causality Condition                                     | Roots                                   | Unit Circle                     |
|---------------|---------------------------------------------------------------|----------------------------------------------------------|--------------------------------------------------------|-----------------------------------------|----------------------------------|
| AR(p)        | Roots must lie outside the unit circle (absolute value > 1) | Not applicable (AR processes are not invertible)        | Roots must lie outside the unit circle                 | 1 - φ₁z - φ₂z² - ... - φₚzᵖ = 0       | Roots > 1                       |
| MA(q)        | Always stationary (finite memory)                            | Roots must lie outside the unit circle                   | Not applicable (MA processes are inherently causal)    | 1 + θ₁z + θ₂z² + ... + θᵩzᵩ = 0      | Roots > 1                       |
| ARMA(p, q)   | AR roots must lie outside the unit circle                    | MA roots must lie outside the unit circle                | AR roots must lie outside the unit circle              | AR: 1 - φ₁z - ... - φₚzᵖ = 0          | AR roots > 1 <br> MA roots > 1  |


forecast with arma11

library(forecast)

# Load your dataset
data <- USAccDeaths

# Fit the ARMA(1,1) model
fit <- Arima(data, order = c(1,0,1), method = "ML")

# for fit 
fit 

# plot
plot(residuals(fit))


# Create forecasts
forecast <- forecast(fit, h = 30)

# Plot the results
plot(forecast)

estimators 


library(itsmr)

# Assuming you have a time series named "lake"
data <- lake

# Plot the time series
plotc(data)

# Calculate and plot the ACF
acf(data)

# Calculate and plot the PACF
pacf(data)

# Estimate coefficients for an AR(2) model
yw_estimates <- yw(data, 2)
print(yw_estimates)
hannan(lake, 2 ,1)


# for arima 

diff()


#adf

library(tseries)

# Generate a simulated ARMA(2,1,1) process
arma111 <- arima.sim(list(order=c(2,1,1), ar=c(0.8,-0.9), ma=.7), 500)

# Plot the time series
ts.plot(arma111)

# Perform the ADF test with a lag length of 7
adf_result_7 <- adf.test(arma111, k=7)
print(adf_result_7)

# Perform the ADF test with a lag length of 2
adf_result_2 <- adf.test(arma111, k=2)
print(adf_result_2)

The ADF test is used to determine if a time series is stationary.
The lag order (k) in the ADF test specifies the number of lagged differences included in the regression equation.
Choosing an appropriate lag length is crucial for the accuracy of the ADF test.
The p-value from the ADF test indicates the significance of the test. A p-value less than the chosen significance level suggests that the series is stationary.
By comparing the results of the ADF test with different lag lengths, you can assess the sensitivity of the test to the choice of lag order and make informed decisions about the stationarity of your time series.

library(bootUR)
adf(arma111)
pp.test(ar2)
kpss.test


# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Step 1: Load and plot the data
# Replace 'your_data' with your actual data
data <- your_data  # Your time series data

# Plot the original time series
autoplot(data) + ggtitle("Original Time Series")

# Step 2: Transform the data using Box-Cox transformation if needed
lambda <- BoxCox.lambda(data)
transformed_data <- BoxCox(data, lambda)

# Plot the transformed data
autoplot(transformed_data) + ggtitle("Transformed Time Series")

# Step 3: Check for stationarity and take differences if necessary
adf_test <- adf.test(transformed_data)
kpss_test <- kpss.test(transformed_data)
print(adf_test)
print(kpss_test)

# Differencing the data until it becomes stationary
d <- 0  # Initialize differencing order
while ((adf_test$p.value > 0.05 || kpss_test$p.value < 0.05) && d < 2) {
  d <- d + 1
  transformed_data <- diff(transformed_data)
  adf_test <- adf.test(transformed_data)
  kpss_test <- kpss.test(transformed_data)
  print(adf_test)
  print(kpss_test)
}

# Step 4: Examine ACF/PACF
acf(transformed_data, main = "ACF of Differenced Data")
pacf(transformed_data, main = "PACF of Differenced Data")

# Step 5: Use auto.arima to select the best ARIMA model
fit <- auto.arima(data, lambda = lambda)

# Display the summary of the model
summary(fit)

# Step 6: Check residuals
checkresiduals(fit)

# Step 7: Forecasting
forecasts <- forecast(fit, h = 10)  # Adjust 'h' for the desired forecast horizon
autoplot(forecasts) + ggtitle("Forecasts from ARIMA Model")

# Print the forecast values
print(forecasts)
