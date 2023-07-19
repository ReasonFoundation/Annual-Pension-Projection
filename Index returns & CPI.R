#This R script is used for two purposes:
#1. Extract and clean index data and then calculate the index returns, which will be used to estimate missing returns before the projection year
#2. Extract and process CPI data for inflation adjustment 

###############################################################
symbols <- c("IWV", "VBTIX")
#VNQ

#Get MSCI ACWI ex US Index (representing large & mid cap developed markets equities)
acwi_exUS <- import(paste(FolderName, "/", acwi_exUS_file,sep = ""), skip = 6)
inter_developed_price <- acwi_exUS %>%  
  rename(acwi_exUS = acwi_exUS_name,
         date = Date) %>% 
  mutate(acwi_exUS = as.numeric(acwi_exUS),
         date = mdy(date),
         month = month(date),
         fy = year(date)) %>% 
  filter(!is.na(date)) %>% 
  select(-date)

#Get iShares Russell 3000 ETF (IWV, representing US equities) and Vanguard Total Bond Market Index Fund Institutional Shares (VBTIX, representing US investment-grade bonds) prices
us_prices <- getSymbols(symbols,
                     src = "yahoo",
                     from = PriceStartDate,
                     to = PriceEndDate,
                     auto.assign = T,
                     warnings = F) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)

us_prices_monthly <- to.monthly(us_prices, OHLC = F)    #get monthly prices

#Join MSCI ACWI ex-US prices
index_prices <- us_prices_monthly %>% 
  data.frame(date = index(.)) %>% 
  remove_rownames() %>% 
  mutate(fy = year(date), month = month(date)) %>% 
  left_join(inter_developed_price) %>% 
  select(fy, month, acwi_exUS, IWV, VBTIX)

#Calculate annual returns for the individual securities
index_returns <- index_prices %>% 
  pivot_longer(cols = -(fy:month),
               names_to = "index",
               values_to = "price") %>% 
  arrange(index, month, fy) %>% 
  group_by(index, month) %>% 
  mutate(index_returns = price/lag(price) - 1) %>% 
  select(-price) %>% 
  pivot_wider(names_from = index, values_from = index_returns) %>% 
  ungroup()

export(index_returns, paste(FolderName, "/", index_returns_output,sep = ""))

#Get inflation data
cpi <- fredr(series_id = "CPIAUCSL", observation_start = cpi_start_date, observation_end = cpi_end_date) %>% 
  select(1,3) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  rename(cpi = value) %>% 
  select(year, month, cpi)

export(cpi, paste0(FolderName, "/", "cpi.csv"))  

