Pair Trading Strategy App

https://2o1out-neil-luetz.shinyapps.io/pairwise/

Overview

This Shiny app implements a statistical arbitrage pair trading strategy applied to S&P 500 stocks. The application allows users to select any two tickers from the S&P 500, visualize price behavior, evaluate trading signals generated from their spread, and backtest a mean-reversion strategy. Additionally, a correlation matrix of all S&P 500 stocks helps identify highly correlated pairs for optimal strategy selection.

Explore the live application here. Note: Hosting is limited by rshiny.io free-tier constraints.

Features

1. Pairwise Strategy Simulation

Select any two S&P 500 tickers.

Choose historical lookback period (e.g., 1 year, 2 years).

Visualize log-price behavior and spread.

Backtest a mean-reversion trading strategy using z-score of the price spread.

Evaluate performance via cumulative PnL, number of trades, average return per trade, and Sharpe Ratio.

2. Correlation Matrix Tab

Displays a heatmap correlation matrix of all S&P 500 stock returns over the selected window.

Helps identify candidate pairs based on high correlation.

3. User Interface (Dark Mode)

Responsive dashboard with dark-themed UI.

Tab-based navigation for clarity and ease of use.

How It Was Made

Strategy Logic

Pair Selection: Users select two S&P 500 stocks. Historical price data is fetched using quantmod::getSymbols().

Log Prices & Spread: Calculates the log spread between the two assets.

Z-score Signal: Calculates z-score of the spread over a rolling window.

Trading Logic:

Long spread when z < -entry threshold

Short spread when z > entry threshold

Exit when z reverts towards 0

Backtesting:

Executes trades based on the above logic.

Tracks position PnL, number of trades, Sharpe Ratio, and average holding period.

Data Sources

Historical price data is fetched directly from Yahoo Finance via the quantmod package.

Technical Stack

R/Shiny: App framework

Tidyverse (dplyr, ggplot2, etc.): Data wrangling and visualization

quantmod: Financial data fetching

shinythemes: Dark mode

heatmaply / corrplot: Correlation visualization

Limitations

No Cointegration Check:

Strategy assumes mean-reverting spread, but does not test for cointegration.

Future work could include Engle-Granger test or Johansen test.

Transaction Costs:

Backtest does not currently account for commissions, bid-ask spreads, or slippage.

Lookahead Bias:

The z-score calculation uses rolling statistics, but care is taken to avoid using future data.

API Limits:

The Yahoo Finance API may occasionally rate-limit or return missing data for certain tickers.

Installation

Clone the repository:

git clone https://github.com/YOURUSERNAME/pair-trading-strategy-app.git

Open the R project in RStudio.

Install required packages:

install.packages(c("shiny", "ggplot2", "dplyr", "quantmod", "scales", "xts", "zoo", "shinythemes", "heatmaply"))

Usage

Run the app:

shiny::runApp("app.R")

Navigate tabs to explore backtest results and correlation heatmaps.

Enter your selected pair tickers and lookback period to simulate the strategy.

File Structure

1.Pairwise.R: Contains strategy logic and utility functions.

2.aPairwiseDark.R: Shiny UI and server code with dark-themed interface.



Acknowledgements

Yahoo Finance (via quantmod)

RStudio / Shiny Team

Community contributors to heatmaply, corrplot, and other visualization tools.

Contributing

Pull requests and suggestions are welcome. Open an issue or fork the repository to suggest changes.

Contact

neilluetz@gmail.com
