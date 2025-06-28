# app.R

library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(xts)
library(zoo)
library(bslib)

# UI with dark theme
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "darkly",  # GitHub-like dark theme
    base_font = font_google("Fira Code")
  ),
  
  titlePanel("📈 Pairs Trading Strategy Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pair", "Choose a cointegrated pair:",
                  choices = names(zscore_signals)),
      br(),
      
      h4("📊 Performance Metrics"),
      verbatimTextOutput("metrics"),
      
      br(),
      helpText("Performance metrics provide key statistics about the trading strategy for the selected pair, including profitability, risk, and consistency measures.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("How It Works",
                 fluidRow(
                   column(12,
                          h3("🧠 Introduction to Pairs Trading"),
                          p("Pairs trading is a market-neutral strategy that involves trading two related assets whose prices move together. The strategy profits from short-term deviations from their long-term relationship."),
                          
                          h4("🔗 Cointegration and Price Ratio"),
                          p("We identify cointegrated pairs—assets whose price ratio tends to be stable. The price ratio, not the price itself, is tracked over time."),
                          
                          h4("📏 Z-Score: Measuring Deviation"),
                          p("The z-score tells us how far the price ratio is from its 40-day average, in standard deviations. A high z-score means the ratio is high (potential short), and a low score indicates the opposite (potential long)."),
                          
                          h4("📈 Trading Signals"),
                          tags$ul(
                            tags$li(strong("Z-score > +2:"), " SHORT the spread (sell expensive, buy cheap)"),
                            tags$li(strong("Z-score < -2:"), " LONG the spread (buy cheap, sell expensive)"),
                            tags$li(strong("Z-score ≈ 0:"), " Exit the position (mean reversion expected)")
                          ),
                          
                          h4("⚙️ Parameters: 40-Day Lookback & Thresholds"),
                          p("A 40-day rolling window balances recency and stability. The ±2 standard deviation threshold is based on statistical significance (5% tail events)."),
                          
                          h4("⚠️ Risks"),
                          p("Assumes long-term stability of the pair (cointegration). Unexpected market changes or structural breaks can reduce effectiveness.")
                   )
                 )
        ),
        
        tabPanel("Z-score Plot",
                 p("This chart shows the standardized price ratio (z-score) between the two stocks."),
                 p("Green triangles = LONG entry (z-score < -2), Red triangles = SHORT entry (z-score > +2)."),
                 plotOutput("zPlot")
        ),
        
        tabPanel("Cumulative P&L",
                 p("This chart shows how profit or loss has accumulated over time for the selected pair."),
                 p("Use it to understand profitability, drawdowns, and strategy consistency."),
                 plotOutput("profitPlot")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$zPlot <- renderPlot({
    req(input$pair)
    
    symbols <- strsplit(input$pair, "-")[[1]]
    s1 <- symbols[1]
    s2 <- symbols[2]
    
    price1 <- prices_xts[, s1]
    price2 <- prices_xts[, s2]
    ratio <- na.omit(price1 / price2)
    
    roll_mean <- rollapply(ratio, 40, mean, align = "right", fill = NA)
    roll_sd <- rollapply(ratio, 40, sd, align = "right", fill = NA)
    zscore <- (ratio - roll_mean) / roll_sd
    zscore <- na.omit(zscore)
    
    signal <- zscore_signals[[input$pair]]
    signal <- signal[index(zscore)]
    
    df <- data.frame(
      Date = index(zscore),
      Zscore = as.numeric(zscore),
      Mean = 0,
      Upper = 2,
      Lower = -2,
      Signal = as.numeric(signal)
    )
    
    df$Trade <- ifelse(df$Signal == 1, "LONG",
                       ifelse(df$Signal == -1, "SHORT", NA))
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Zscore), color = "#F8F9FA") +
      geom_hline(yintercept = 0, linetype = "solid", color = "#5bc0de") +
      geom_hline(yintercept = 2, linetype = "dashed", color = "#e74c3c") +
      geom_hline(yintercept = -2, linetype = "dashed", color = "#2ecc71") +
      geom_point(data = subset(df, Trade == "LONG"), aes(y = Zscore), color = "#2ecc71", shape = 24, fill = "#2ecc71", size = 3) +
      geom_point(data = subset(df, Trade == "SHORT"), aes(y = Zscore), color = "#e74c3c", shape = 25, fill = "#e74c3c", size = 3) +
      labs(
        title = paste("Z-score & Trading Signals:", input$pair),
        y = "Z-score",
        x = "Date",
        caption = "Z-score thresholds: ±2. Long: green, Short: red."
      ) +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal(base_family = "Fira Code") +
      theme(
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major = element_line(color = "#444444"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.position = "none"
      )
  })
  
  output$profitPlot <- renderPlot({
    req(input$pair)
    
    df_profit <- pair_profits[[input$pair]]
    
    ggplot(df_profit, aes(x = Date, y = CumulativeProfit)) +
      geom_line(color = "#5bc0de", size = 1) +
      ggtitle(paste("Cumulative Profit for:", input$pair)) +
      ylab("Cumulative Profit") +
      xlab("Date") +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal(base_family = "Fira Code") +
      theme(
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major = element_line(color = "#444444"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white")
      )
  })
  
  output$metrics <- renderPrint({
    req(input$pair)
    perf <- performance_metrics %>% filter(Pair == input$pair)
    
    cat("📌 Performance Summary for:", input$pair, "\n\n")
    cat("🔹 Total Trades:", perf$TotalTrades, "\n")
    cat("🔹 Win Rate:", percent(perf$WinRate), "\n")
    cat("🔹 Avg Return per Trade:", dollar(perf$AvgReturn), "\n")
    cat("🔹 Sharpe Ratio:", round(perf$SharpeRatio, 2), "\n")
    cat("🔹 Max Drawdown:", percent(perf$MaxDrawdown), "\n")
  })
}

shinyApp(ui = ui, server = server)
