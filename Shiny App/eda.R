# library(crypto2)
# library(zoo)
# library(tidyverse)
# #> 
# #> Attache Paket: 'dplyr'
# #> Die folgenden Objekte sind maskiert von 'package:stats':
# #> 
# #>     filter, lag
# #> Die folgenden Objekte sind maskiert von 'package:base':
# #> 
# #>     intersect, setdiff, setequal, union
# 
# # List all active coins
# coins <- crypto_list(only_active=TRUE)
# 
# 
# coins <- coins |>
#   filter(symbol %in% c("BTC", "ETH", "BNB", "USDT", "PAXG", "ADA", "SOL", "XRP", "USDC"))
# 
# # retrieve historical data for all (the first 3) of them
# coin_hist2 <- crypto_history(coins, convert="USD", start_date="20180101", end_date="20210512") |>
#   mutate(date = as.Date(timestamp))
# 
# 
# # write_csv(coin_hist2, "coin_hist2.csv")
# 
# df_daily <- coin_hist2 |>
#   select(date, name, close) |>
#   group_by(name) |>
#   arrange(date, .by_group = T) |>
#   mutate(close=rollapply(close, 30, mean,align='right',fill=NA)) |>
#   mutate(pct_change = (close/lead(close) - 1))
# 
# df_daily |>
#   group_by(name) |>
#   e_charts(date) |>
#   e_line(pct_change, symbol = "none", lineStyle = list(width = 3)) |>
#   e_theme_custom("echarts_theme.json") |>
#   e_tooltip(trigger = "axis", e_tooltip_pointer_formatter(style = "percent", digits = 1)) |>
#   e_title("Crypto") |>
#   e_legend(orient = 'vertical', left = "1%", top = "10%") |>
#   e_theme_custom("echarts_theme.json") |>
#   e_grid(left = "20%")
# 
