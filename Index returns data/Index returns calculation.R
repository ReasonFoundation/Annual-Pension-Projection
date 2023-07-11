library(tidyverse)
library(lubridate)
library(quantmod)
library(rio)

#Update these values

#Get MSCI ACWI ex US Index
FolderName <- "Index returns data"
InputFileName <- "acwi_exUS_price_net.xls"
OutputFileName <- "index_returns.csv"
AssetPriceName <- "ACWI ex USA Standard (Large+Mid Cap)"
symbols <- c("IWV", "VBTIX")

#Calculate the range for slicing
#This range will
MinYear <- 1987
MaxYear <- 2023
Range <- (MaxYear - MinYear)*12 + 1

#Range for calculating yearly price data
PriceStartDate <- "1998-12-31"
PriceEndDate <- "2023-01-01"
###############################################################

acwi_exUS <- import(paste(FolderName, "/", InputFileName,sep = ""), skip = 6)
acwi_exUS_price <- acwi_exUS %>%  
  slice(1:Range) %>% # last few rows are not numbers
  rename(acwi_exUS = AssetPriceName,
         date = Date) %>% 
  mutate(acwi_exUS = as.numeric(acwi_exUS),
         date = mdy(date),
         month = month(date),
         fy = year(date)) %>% 
  select(-date)

#Get iShares Russell 3000 ETF (IWV) and Vanguard Total Bond Market Index Fund Institutional Shares (VBTIX) prices


prices <- getSymbols(symbols,
                     src = "yahoo",
                     from = PriceStartDate,
                     to = PriceEndDate,
                     auto.assign = T,
                     warnings = F) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- to.monthly(prices, OHLC = F)    #get monthly prices

#Add MSCI ACWI ex-US prices
index_price <- prices_monthly %>% 
  data.frame(date = index(.)) %>% 
  remove_rownames() %>% 
  mutate(fy = year(date), month = month(date)) %>% 
  left_join(acwi_exUS_price) %>% 
  select(fy, month, acwi_exUS, IWV, VBTIX)

#Calculate annual returns for the individual securities
index_returns <- index_price %>% 
  pivot_longer(cols = 3:5,
               names_to = "index",
               values_to = "price") %>% 
  arrange(index, month, fy) %>% 
  group_by(index, month) %>% 
  mutate(index_returns = price/lag(price) - 1) %>% 
  select(-price) %>% 
  pivot_wider(names_from = index, values_from = index_returns) %>% 
  ungroup()

export(index_returns, paste(FolderName, "/", OutputFileName,sep = ""))
#export(index_returns,OutputFileName)
