# Pair Trading Strategy App

[https://2o1out-neil-luetz.shinyapps.io/pairwise/](https://2o1out-neil-luetz.shinyapps.io/pairwise/)

## Overview

This Shiny app implements a **statistical arbitrage pair trading strategy** applied to S&P 500 stocks. The application allows users to select any two tickers from the S&P 500, visualize price behavior, evaluate trading signals generated from their spread, and backtest a mean-reversion strategy. Additionally, a correlation matrix of all S&P 500 stocks helps identify highly correlated pairs for optimal strategy selection.

Explore the live application [here](https://2o1out-neil-luetz.shinyapps.io/pairwise/). **Note: Hosting is limited by rshiny.io free-tier constraints.**

## Features

### 1. Pairwise Strategy Simulation

- Select any two S&P 500 tickers.
- Choose historical lookback period (e.g., 1 year, 2 years).
- Visualize log-price behavior and spread.
- Backtest a mean-reversion trading strategy using z-score of the price ratio.
- Evaluate performance via cumulative PnL, number of trades, average return per trade, Sharpe Ratio, and Profit Factor.

### 2. Correlation Matrix Tab

- Displays a heatmap correlation matrix of all S&P 500 stock returns over the selected window.
- Helps identify candidate pairs based on high correlation.

### 3. User Interface (Dark Mode)

- Responsive dashboard with dark-themed UI.
- Tab-based navigation for clarity and ease of use.

## How It Was Made

### Strategy Logic

1. **Data Acquisition**:
   - The app automatically scrapes the full list of S&P 500 tickers from Wikipedia.
   - Historical daily closing prices are pulled via `quantmod::getSymbols()` from Yahoo Finance (starting January 2020).

2. **Preprocessing**:
   - Stocks are merged into a clean price matrix.
   - A correlation matrix is computed using daily returns.
   - Pairs with correlation > 0.75 are shortlisted.

3. **Cointegration Filter**:
   - Each correlated pair is tested for cointegration using the **Engle-Granger approach** (`lm()` + `adf.test()` on residuals).
   - Only pairs with statistically significant mean-reverting relationships (ADF p-value < 0.05) are retained.

4. **Z-score Signal Logic** (based on **price ratio**, not spread):
   - Compute price ratio: `Price1 / Price2`
   - Rolling mean and standard deviation (40-day window) of the ratio.
   - Z-score: `(Ratio - Mean) / SD`
   - **Entry**:
     - Long spread (buy A, sell B) if z-score < -2
     - Short spread (sell A, buy B) if z-score > +2
   - **Exit**: Close position when |z-score| < 0.5

5. **Backtesting**:
   - Trade signals generate daily position vectors.
   - Daily returns from trades are aggregated to compute profit and loss.
   - Key metrics like cumulative profit, Sharpe Ratio, and Profit Factor are computed for each pair.

### Parameters and Justification

- **Entry Threshold (±2)**: Triggers trades only on more significant price divergences, reducing false signals.
- **Exit Threshold (±0.5)**: Closes trades near the mean, capturing profit before full mean reversion.
- **Rolling Window (40 Days)**: A common window in time-series trading, balancing short-term responsiveness with robustness.

### Data Sources

- Historical price data is fetched directly from Yahoo Finance via the `quantmod` package.

### Technical Stack

- **R/Shiny**: App framework
- **Tidyverse (dplyr, ggplot2, etc.)**: Data wrangling and visualization
- **quantmod**: Financial data fetching
- **shinythemes**: Dark mode
- **heatmaply / corrplot**: Correlation visualization

### Limitations

1. **Cointegration Checked (Engle-Granger Only)**:
   - Each highly correlated pair undergoes an ADF test on the residuals of the linear model.
   - However, this is a **pairwise method** — multivariate cointegration (e.g. Johansen test) is not used.

2. **Transaction Costs**:
   - Backtest does not currently account for commissions, bid-ask spreads, or slippage.

3. **Lookahead Bias**:
   - The z-score calculation uses rolling statistics, but care is taken to avoid using future data.

4. **API Limits**:
   - The Yahoo Finance API may occasionally rate-limit or return missing data for certain tickers.


