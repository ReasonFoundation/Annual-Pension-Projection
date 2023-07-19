library(shiny)
library(shinyWidgets)
library(echarts4r)
library(dplyr)
library(tidyr)
library(reactable)
library(echarts4r)
library(shinycssloaders)
# library(tidyverse)
library(readxl)
# library(plotly)
# library(formattable)
library(scales)
library(lubridate)
library(quantmod)
library(quadprog)
library(rio)


#---------------------------FUNCTIONS & SETTINGS--------------------------------
options(spinner.color = "#ff6633", spinner.size = 1, spinner.type = 8)
source("utilities/utilities.R")


#-------------------------------Data Prep----------------------------------------
ppd_full <- read.csv("data/ppd-data-latest-05-2022.csv") # pension data
index_returns <- read.csv("data/index_returns/index_returns.csv") # index returns data
uspop <- import("data/State population.xlsx", sheet = "Sheet1")  # US population data
cpi_raw <- import("data/index_returns/cpi.csv") %>% rename(fy = year) #inflation data
json <- jsonlite::read_json("https://code.highcharts.com/mapdata/countries/us/us-all.geo.json")

#Processing inflation data
cpi_impute <- tibble(fy = rep(2022,7), 
                     month = 6:12,
                     cpi = NA)

cpi <- bind_rows(cpi_raw, cpi_impute) %>% 
  arrange(month) %>% 
  group_by(month) %>% 
  mutate(inflation_rate = cpi/lag(cpi) - 1) %>% 
  ungroup() %>% 
  mutate(inflation_rate2 = ifelse(fy == 2022 & is.na(inflation_rate),        #impute missing inflation data in 2022 with the inflation rate in May 2022
                                  inflation_rate[fy == 2022 & month == 5], 
                                  inflation_rate),
         cpi2 = ifelse(is.na(cpi),
                       lag(cpi) * (1 + inflation_rate2),
                       cpi)) %>% 
  select(fy, month, cpi2)

#Specify fiscal years
pre_fy <- 2020
current_fy <- 2021

#Filter and clean PPD data
ppd <- ppd_full |> 
  mutate(PlanFullName = gsub("\x92", "'", PlanFullName),    #clean plan names and full names
         PlanName = gsub("\x92", "'", PlanName)) |> 
  filter(AdministeringGovt == 0, fy > 2000, !(PlanName %in% c("Colorado State and School", 
                                                              "Oklahoma Municipal Employees"))) |>    #select state plans only and get data after 2000. Filter out two plans that don't have enough data.
  select(fy, fye, PlanName, PlanFullName, StateName, ActLiabilities_GASB, MktAssets_net,
         InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
         PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, 
         expense_TotBenefits) |> 
  rename(plan_name = PlanName,            #rename columns for easier reference
         plan_full_name = PlanFullName,
         state = StateName,             
         aal = ActLiabilities_GASB,
         mva = MktAssets_net,
         arr = InvestmentReturnAssumption_GASB,
         return = InvestmentReturn_1yr,
         payroll_growth = PayrollGrowthAssumption,
         nc = NormCostRate_tot,
         cont_rate = ReqContRate_tot,
         ben_pay = expense_TotBenefits) |> 
  mutate(fye = ymd(fye),
         month = month(fye), .after = fye)

#Impute missing normal costs for New York State Teachers, NY State & Local ERS, and NY State & Local Police & Fire
ppd <- ppd |> 
  mutate(nc = replace(nc, plan_name == "New York State Teachers" & fy == 2020, 0.1),
         nc = replace(nc, plan_name == "NY State & Local ERS" & fy == 2020, 0.17),
         nc = replace(nc, plan_name == "NY State & Local Police & Fire" & fy == 2020, 0.3))

#Fix wrong data:
ppd <- ppd %>% 
  mutate(aal = replace(aal, plan_name == "Nebraska Schools" & fy == 2001, 4576863))


# Funding Projection ------------------------------------------------------

#Add a 2022 row to each plan
ppd <- ppd %>% 
  group_by(plan_name) %>% 
  group_modify(~ add_row(.x)) %>% 
  ungroup()

#Calculate 5-year average payroll growth rates and benefit payment growth rates
ppd <- ppd %>% 
  group_by(plan_name) %>% 
  mutate(payroll_growth_avg = (payroll/lag(payroll, n = 5))^(1/5)-1,
         ben_pay_growth_avg = (ben_pay/lag(ben_pay, n = 5))^(1/5)-1) %>% 
  ungroup()

#Identify years that have missing AAL/MVA data or have duplicated AAL data
ppd <- ppd %>% 
  mutate(real_vs_estimate = case_when(is.na(aal) ~ "estimate",
                                      is.na(mva) ~ "estimate",
                                      aal == lag(aal) ~ "estimate", 
                                      TRUE ~ "real"), .before = aal)



#Initial projection
ppd_project <- ppd %>% 
  group_by(plan_name) %>% 
  mutate(fy = fy_f(fy),
         fye = fye_f(fye),
         across(.cols = c(month,
                          plan_full_name,
                          state,
                          arr,
                          payroll_growth_avg,
                          ben_pay_growth_avg,
                          nc,
                          cont_rate), 
                .fns = get_last_f),
         payroll_growth = payroll_growth_f(payroll_growth, payroll_growth_avg),
         payroll = growth_f(x = payroll, g = payroll_growth),
         ben_pay = growth_f(x = ben_pay, g = ben_pay_growth_avg)
  ) %>% 
  ungroup()



#Create synthetic benchmark portfolios to estimate returns in missing years with 3 indexes

#Function to create a synthetic benchmark portfolio using quadratic programming (to find the "best fit" benchmark portfolio)
#See examples in the two links below:
#https://henrywang.nl/quadratic-programming-with-r/
#https://henrywang.nl/another-quadratic-programming-example-with-r/

benchmark_portfolio <- function(return, x1, x2, x3) {
  
  end_pos <- max(which(!is.na(return)))       #find the latest year with available return data
  y_actual <- return[(end_pos - 9):end_pos]   #use only the last 10 years (with available return data) to create the benchmark
  x0 <- 1                                     #for the intercept (alpha)
  x1 <- x1[(end_pos - 9):end_pos]
  x2 <- x2[(end_pos - 9):end_pos]
  x3 <- x3[(end_pos - 9):end_pos]
  x = cbind(x0, x1, x2, x3)
  
  Dmat <- crossprod(x)
  dvec <- crossprod(y_actual, x)                  # vector to be minimized: product:transpose y_actual and x
  # Amat <- cbind(rep(1,3), diag(3))   
  Amat <- t(cbind(0, rbind(rep(1,3), diag(3))))   # matrix defining the constraints
  bvec <- c(1,0,0,0)                              # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
  
  return(list(result$solution))
}


#Join pension data with index returns and solve the benchmark portfolio for each plan, then use the benchmark portfolio to estimate the pension funds' returns
ppd_benchmark <- ppd_project %>% 
  left_join(index_returns) %>% 
  group_by(plan_name) %>% 
  mutate(benchmark = benchmark_portfolio(return, acwi_exUS, IWV, VBTIX),
         benchmark_return = benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX,
         predict_return = benchmark[[1]][1] + benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX) %>% 
  ungroup()

#Clean population data
uspop_long <- uspop %>% 
  mutate(`2022` = `2021`) %>% 
  pivot_longer(cols = 2:24, names_to = "fy", values_to = "pop") %>% 
  mutate(fy = as.numeric(fy)) %>% 
  rename(state = State) %>% 
  arrange(state, fy)

#--------------------------------MODULES----------------------------------------
source("modules/mod_return_select.R")
source("modules/mod_ual_fr_plan.R")
source("modules/mod_ual_fr_us.R")
source("modules/mod_ual_map.R")


