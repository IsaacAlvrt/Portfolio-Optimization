library(tidyquant)
library(ggplot2)
library(dplyr) #run if R doesn´t support group_by()
library(tseries)

#tseries data for optimization
ibm = get.hist.quote(instrument = "ibm", start = "2000-01-01", end = "2019-10-01", quote = "Close")
wfc = get.hist.quote(instrument = "wfc", start = "2000-01-01", end = "2019-10-01", quote = "Close")
msft = get.hist.quote(instrument = "msft", start = "2000-01-01", end = "2019-10-01", quote = "Close")


#Getting returns by diff() method.
ribm = diff(log(ibm))
rwfc = diff(log(wfc))
rmsft = diff(log(msft))

returns = cbind(ribm, rwfc, rmsft)
rerurns = na.remove(returns)

matriz.covarianzas = cov(returns)

#Portfolio weight = pw
portafolio1 = portfolio.optim(returns, covmat = matriz.covarianzas)
portafolio1$pw


#Portfolio optimal weigts
#IBM = 0.3237408, WFC = 0.2539673, MSFT = 0.4222919


#Monthly returns: IBM, Wells Fargo, Microsoft
stock_returns_monthly <- c("IBM", "WFC", "MSFT") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2019-10-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

#(Tech) Market returns
baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2019-10-01") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

#Sequential index n = 3
stock_returns_monthly_multi <- stock_returns_monthly %>%
  tq_repeat_df(n = 3)

#Weights
weights <- c(
  0.3237408, 0.2539673, 0.4222919, #Optimal
  0.25, 0.50, 0.25,
  0.25, 0.25, 0.50
)
stocks <- c("IBM", "WFC", "MSFT")
weights_table <-  tibble(stocks) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)

#Expand for portfolio
portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

#Merge porfolio and Market
RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")




#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#MAIN INDICATORS

#CAPM
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

#Sharpe Ratio
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

#Stats
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)

#AnnualizedReturns
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)

#Correlation
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.Correlation)

#Downside risk
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk)

#DownsideRiskRatio
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRiskRatio)

#DownsideRiskRatio
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRiskRatio)

#Higher Moments
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.HigherMoments)

#Variability
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Variability)

#Value-at-Risk (VaR)
RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)

#Full list
tq_performance_fun_options()



#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#PLOT

#Optimal
wts <- c(0.3237408, 0.2539673, 0.4222919) 

#Portfolio Returns
portfolio_returns_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col  = symbol, 
                 returns_col = Ra, 
                 weights     = wts, 
                 col_rename  = "Ra")


portfolio_returns_monthly %>%
    ggplot(aes(x = date, y = Ra)) +
    geom_bar(stat = "identity", fill = palette_light()[[1]]) +
    labs(title = "Portfolio Returns",
         subtitle = "32.37% IBM, 25.40% WFL, and 42.23% MSFT",
         caption = "Shows an above-zero trend meaning positive returns",
         x = "Year", y = "Monthly Returns") +
    geom_smooth(method = "lm") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent)

#Portafolio growth, initial investmente --> PV = $100,000
pv = 100000
portfolio_growth_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = Ra, 
                 weights      = wts, 
                 col_rename   = "investment.growth",
                 wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * pv)


portfolio_growth_monthly %>%
    ggplot(aes(x = date, y = investment.growth)) +
    geom_line(size = 2, color = palette_light()[[1]]) +
    labs(title = "Portfolio Growth, Initial investment = $100,000",
         subtitle = "32.37% IBM, 25.40% WFL, and 42.23% MSFT",
         caption = "Now we can really visualize performance!",
         x = "", y = "Portfolio Value") +
    geom_smooth(method = "loess") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)
tail(portfolio_growth_monthly)

#Multiple Portfolios
weights <- c(
  0.3237408, 0.2539673, 0.4222919, #Optimal
  0.25, 0.50, 0.25,
  0.25, 0.25, 0.50
)
stocks <- c("IBM", "WFC", "MSFT")
weights_table <-  tibble(stocks) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)


portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = Ra, 
                 weights      = weights_table, 
                 col_rename   = "investment.growth",
                 wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * pv)

portfolio_growth_monthly_multi %>%
    ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
    geom_line(size = 2) +
    labs(title = "Portfolio Growth, Initial investment = $100,000",
         subtitle = "Comparing Multiple Portfolios",
         caption = "",
         x = "", y = "Portfolio Value",
         color = "Portfolio") +
    geom_smooth(method = "loess") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)
tail(portfolio_growth_monthly_multi)