# Load necessary libraries
library(quantmod)
library(rvest)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(pheatmap)
library(tseries)  # For cointegration test
library(urca)     # Unit root test
library(zoo)      # For rolling calculations

# Step 1: Scrape S&P 500 tickers
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
webpage <- read_html(url)
sp500_table <- html_table(webpage, fill = TRUE)[[1]]
tickers <- sp500_table$Symbol

# Step 2: Download historical stock data
stock_data_list <- list()
for (ticker in tickers) {
  stock_data <- tryCatch({
    getSymbols(ticker, from = "2020-01-01", to = "2025-06-01", src = "yahoo", auto.assign = FALSE)
  }, error = function(e) {
    message(paste("Error fetching data for:", ticker))
    return(NULL)
  })
  
  if (!is.null(stock_data)) {
    stock_data_list[[ticker]] <- Cl(stock_data)
  }
}

# Step 3: Merge and clean price data
prices <- do.call(merge, stock_data_list)
prices <- na.omit(prices)
colnames(prices) <- gsub("\\.Close$", "", colnames(prices))

# Step 4: Prepare sector mapping
tickers_sectors <- sp500_table %>%
  select(Symbol, `GICS Sector`)

# Step 5: Reshape to long format
prices_df <- as.data.frame(prices)
prices_df$date <- as.Date(rownames(prices_df))
prices_long <- prices_df %>%
  pivot_longer(cols = -date, names_to = "Symbol", values_to = "stock_price")

df2 <- merge(prices_long, tickers_sectors, by = "Symbol", all.x = TRUE)
df2$date <- as.numeric(df2$date)

library(xts)
library(quantmod)
library(TTR)
library(tseries)
library(zoo)
library(dplyr)
library(tidyr)

# --- Step 6: Convert to wide format and clean ---
prices_df <- df2 %>%
  select(date, Symbol, stock_price) %>%
  pivot_wider(names_from = Symbol, values_from = stock_price)

prices_df <- prices_df[complete.cases(prices_df), ]
prices_df$date <- as.Date(prices_df$date)

# --- Step 7: Convert to xts object ---
prices_xts <- xts(prices_df[,-1], order.by = prices_df$date)

# --- Step 8: Calculate returns ---
returns <- ROC(prices_xts, type = "discrete")
returns <- returns[, colSums(!is.na(returns)) > 0]  # Drop empty columns
returns <- returns[complete.cases(returns), ]       # Drop rows with NA

# --- Step 9: Correlation matrix and select pairs > 0.75 ---
cor_matrix <- cor(returns, use = "pairwise.complete.obs")
cor_pairs <- which(cor_matrix > 0.75 & lower.tri(cor_matrix), arr.ind = TRUE)

correlated_pairs <- data.frame(
  stock1 = colnames(cor_matrix)[cor_pairs[,1]],
  stock2 = colnames(cor_matrix)[cor_pairs[,2]],
  correlation = cor_matrix[cor_pairs]
)

# --- Step 10: Cointegration test (Engle-Granger) ---
cointegrated_pairs <- list()

for (i in 1:nrow(correlated_pairs)) {
  s1 <- correlated_pairs$stock1[i]
  s2 <- correlated_pairs$stock2[i]
  
  y <- prices_xts[, s1]
  x <- prices_xts[, s2]
  
  valid_idx <- complete.cases(y, x)
  y <- y[valid_idx]
  x <- x[valid_idx]
  
  if (length(y) > 50) {
    model <- lm(y ~ x)
    resids <- residuals(model)
    adf <- adf.test(resids)
    
    if (adf$p.value < 0.05) {
      cointegrated_pairs[[length(cointegrated_pairs)+1]] <- list(
        stock1 = s1,
        stock2 = s2,
        pval = adf$p.value
      )
    }
  }
}

# --- Step 11: Z-score trading logic based on price ratio ---
zscore_threshold_entry <- 2
zscore_threshold_exit <- 0.5
z_window <- 40

zscore_signals <- list()

for (pair in cointegrated_pairs) {
  s1 <- pair$stock1
  s2 <- pair$stock2
  
  price1 <- prices_xts[, s1]
  price2 <- prices_xts[, s2]
  
  ratio <- price1 / price2
  ratio <- na.omit(ratio)
  
  if (length(ratio) < z_window) next
  
  roll_mean <- rollapply(ratio, width = z_window, mean, align = "right", fill = NA)
  roll_sd   <- rollapply(ratio, width = z_window, sd, align = "right", fill = NA)
  zscore    <- (ratio - roll_mean) / roll_sd
  
  # Signal logic
  position <- rep(0, length(zscore))  # 1 = long spread, -1 = short spread, 0 = no position
  
  for (i in seq_along(zscore)) {
    if (i < z_window) next
    
    if (is.na(zscore[i])) next
    
    if (position[i-1] == 0) {
      if (zscore[i] > zscore_threshold_entry) {
        position[i] <- -1  # Short spread: short s1, long s2
      } else if (zscore[i] < -zscore_threshold_entry) {
        position[i] <- 1   # Long spread: long s1, short s2
      }
    } else if (abs(zscore[i]) < zscore_threshold_exit) {
      position[i] <- 0  # Exit position
    } else {
      position[i] <- position[i-1]  # Hold
    }
  }
  
  zscore_signals[[paste(s1, s2, sep = "-")]] <- xts(position, order.by = index(zscore))
}

# --- Step 12: Output some signals for visual inspection ---
for (name in names(zscore_signals)[1:min(3, length(zscore_signals))]) {
  cat("Signal preview for pair:", name, "\n")
  print(tail(zscore_signals[[name]], 10))
}

library(ggplot2)
library(scales)

# --- Step 12: Visualize strategy for top 1–3 pairs ---
num_pairs_to_plot <- min(3, length(zscore_signals))

for (name in names(zscore_signals)[1:num_pairs_to_plot]) {
  cat("Plotting pair:", name, "\n")
  
  symbols <- strsplit(name, "-")[[1]]
  s1 <- symbols[1]
  s2 <- symbols[2]
  
  # Pull series
  if (!(s1 %in% colnames(prices_xts)) || !(s2 %in% colnames(prices_xts))) {
    cat("One of the symbols not found in price data.\n")
    next
  }
  
  price1 <- prices_xts[, s1]
  price2 <- prices_xts[, s2]
  common_dates <- index(price1)[index(price1) %in% index(price2)]
  
  price1 <- price1[common_dates]
  price2 <- price2[common_dates]
  
  ratio <- na.omit(price1 / price2)
  
  # Calculate rolling stats
  roll_mean <- rollapply(ratio, 40, mean, align = "right", fill = NA)
  roll_sd <- rollapply(ratio, 40, sd, align = "right", fill = NA)
  zscore <- (ratio - roll_mean) / roll_sd
  
  signal <- zscore_signals[[name]]
  signal <- signal[index(zscore)]
  
  # Prepare plotting data frame
  df_plot <- data.frame(
    Date = index(zscore),
    Ratio = as.numeric(ratio),
    Zscore = as.numeric(zscore),
    Mean = as.numeric(roll_mean),
    Upper = as.numeric(roll_mean + 2 * roll_sd),
    Lower = as.numeric(roll_mean - 2 * roll_sd),
    Signal = as.numeric(signal)
  )
  
  df_plot$Trade <- ifelse(df_plot$Signal == 1, "LONG",
                          ifelse(df_plot$Signal == -1, "SHORT", NA))
  
  p <- ggplot(df_plot, aes(x = Date)) +
    geom_line(aes(y = Ratio), color = "black", linewidth = 0.8) +
    geom_line(aes(y = Mean), color = "blue", linetype = "dashed") +
    geom_line(aes(y = Upper), color = "red", linetype = "dotted") +
    geom_line(aes(y = Lower), color = "green", linetype = "dotted") +
    geom_point(data = subset(df_plot, Trade == "LONG"), aes(y = Ratio), color = "green", shape = 24, size = 2, fill = "green") +
    geom_point(data = subset(df_plot, Trade == "SHORT"), aes(y = Ratio), color = "red", shape = 25, size = 2, fill = "red") +
    ggtitle(paste("Price Ratio and Trading Signals:", s1, "/", s2)) +
    ylab("Price Ratio") +
    scale_x_date(labels = date_format("%Y-%m")) +
    scale_x_date(date_labels = "%b %Y") +
    theme_minimal()
  
  
  print(p)
}

# Define thresholds (adjust as needed)
entry_threshold <- 1.75
exit_threshold <- 1

# Initialize list to store profit results per pair
pair_profits <- list()

for (pair_name in names(zscore_signals)) {
  symbols <- strsplit(pair_name, "-")[[1]]
  s1 <- symbols[1]
  s2 <- symbols[2]
  
  # Extract prices and align dates
  price1 <- prices_xts[, s1]
  price2 <- prices_xts[, s2]
  common_dates <- index(price1)[index(price1) %in% index(price2)]
  price1 <- price1[common_dates]
  price2 <- price2[common_dates]
  
  # Compute ratio and rolling stats
  ratio <- na.omit(price1 / price2)
  roll_mean <- rollapply(ratio, 40, mean, align = "right", fill = NA)
  roll_sd <- rollapply(ratio, 40, sd, align = "right", fill = NA)
  zscore <- (ratio - roll_mean) / roll_sd
  zscore <- na.omit(zscore)
  
  # Get signals aligned with zscore dates
  signal <- zscore_signals[[pair_name]]
  signal <- signal[index(zscore)]
  
  # Calculate returns for each stock aligned with zscore dates
  ret1 <- na.omit(Delt(price1)[index(zscore)])
  ret2 <- na.omit(Delt(price2)[index(zscore)])
  
  # Make sure lengths match
  n <- min(length(signal), length(ret1), length(ret2))
  signal <- signal[1:n]
  ret1 <- ret1[1:n]
  ret2 <- ret2[1:n]
  
  # Initialize profit vector
  profit_vec <- numeric(n)
  
  # Calculate profit per time step based on position:
  # If long ratio (zscore < -entry_threshold): buy s1, sell s2
  # If short ratio (zscore > entry_threshold): sell s1, buy s2
  for (t in 2:n) {
    if (signal[t-1] == 1) {
      # Long spread: long s1, short s2
      profit_vec[t] <- ret1[t] - ret2[t]
    } else if (signal[t-1] == -1) {
      # Short spread: short s1, long s2
      profit_vec[t] <- ret2[t] - ret1[t]
    } else {
      # No position
      profit_vec[t] <- 0
    }
  }
  
  cumulative_profit <- cumsum(profit_vec)
  
  pair_profits[[pair_name]] <- data.frame(
    Date = index(zscore)[1:n],
    Profit = profit_vec,
    CumulativeProfit = cumulative_profit
  )
}

# Example: Plot cumulative profit for first pair
library(ggplot2)
first_pair <- names(pair_profits)[1]
df_profit <- pair_profits[[first_pair]]

ggplot(df_profit, aes(x = Date, y = CumulativeProfit)) +
  geom_line(color = "blue") +
  ggtitle(paste("Cumulative Profit for Pair:", first_pair)) +
  ylab("Cumulative Profit") +
  scale_x_date(date_labels = "%b %Y") +
  theme_minimal()

library(dplyr)

performance_metrics <- data.frame(
  Pair = character(),
  SharpeRatio = numeric(),
  ProfitFactor = numeric(),
  TotalProfit = numeric(),
  TotalLoss = numeric(),
  stringsAsFactors = FALSE
)

for (pair_name in names(pair_profits)) {
  profits <- pair_profits[[pair_name]]$Profit
  
  # Skip pairs with no profits or constant zero profit
  if (length(profits) == 0 || all(profits == 0)) next
  
  # Calculate Sharpe Ratio: mean profit / sd profit (assume risk-free rate = 0)
  sharpe_ratio <- ifelse(sd(profits) != 0, mean(profits) / sd(profits), NA)
  
  # Calculate total profit and loss separately
  total_profit <- sum(profits[profits > 0], na.rm = TRUE)
  total_loss <- abs(sum(profits[profits < 0], na.rm = TRUE))
  
  # Profit Factor: total profit / total loss (NA if no losses)
  profit_factor <- ifelse(total_loss > 0, total_profit / total_loss, NA)
  
  performance_metrics <- performance_metrics %>%
    add_row(
      Pair = pair_name,
      SharpeRatio = sharpe_ratio,
      ProfitFactor = profit_factor,
      TotalProfit = total_profit,
      TotalLoss = total_loss
    )
}

# Sort pairs by Sharpe Ratio descending
performance_metrics <- performance_metrics %>%
  arrange(desc(SharpeRatio))

print(performance_metrics)
