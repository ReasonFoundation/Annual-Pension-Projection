#Folder and file names
FolderName <- "Data"
PPDLatest <- "ppd-data-latest.csv"
acwi_exUS_file <- "acwi_exUS_price_net.xls"
acwi_exUS_name <- "ACWI ex USA Standard (Large+Mid Cap)"
index_returns_output <- "index_returns.csv"
ReturnsDataFile <- "Latest Returns.csv"

#Specify fiscal years
current_fy <- 2022
pre_fy <- current_fy - 1
latest_update_year <- 2023

#Date range for getting US equities and fixed income data
PriceStartDate <- ymd("1998-12-31")
PriceEndDate <- make_date(year = latest_update_year, month = 1, day = 1)

#Date range for inflation adjustment
cpi_start_date <- ymd("2000-01-01")
cpi_end_date <- make_date(year = latest_update_year, month = 12, day = 1)



