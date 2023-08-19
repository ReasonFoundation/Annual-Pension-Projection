
###############################################################
#Import pension data
ppd_full <- read.csv(paste(FolderName, "/", PPDLatest, sep = ""))
latest_returns_data <- read.csv(paste(FolderName, "/", ReturnsDataFile, sep = ""))

#Import index returns data
index_returns <- read.csv(paste(FolderName, "/", index_returns_output, sep = ""))

#Import population data
# uspop <- import(StatePopData)

#Filter and clean PPD data
# ppd <- ppd_full %>%
#   mutate(PlanFullName = gsub("\x92", "'", PlanFullName),    #clean plan names and full names
#          PlanName = gsub("\x92", "'", PlanName)) 
# 
# write.csv(ppd, "ppd.csv")

ppd <- read.csv("ppd.csv")

ppd <- ppd %>% 
  #Select state plans only and get data after 2000. Filter out three plans that don't have enough data.
  filter(AdministeringGovt == 0, fy > 2001, !(PlanName %in% c("Colorado State and School", 
                                                              "Oklahoma Municipal Employees",
                                                              "Missouri Local"))) %>%    
  select(fy, fye, PlanName, PlanFullName, StateName, ActLiabilities_GASB, MktAssets_net, TotalPensionLiability,
         InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
         PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, ReqContRate_tot_Stat, 
         expense_TotBenefits) %>% 
  #Set contribution rates equal to statutory rates if statutory rates are available; otherwise, use required contribution rates
  mutate(cont_rate = ifelse(!is.na(ReqContRate_tot_Stat), ReqContRate_tot_Stat, ReqContRate_tot), .before = ReqContRate_tot) %>% 
  #Rename columns for easier reference
  rename(plan_name = PlanName,            
         plan_full_name = PlanFullName,
         state = StateName,             
         aal = ActLiabilities_GASB,
         mva = MktAssets_net,
         tpl = TotalPensionLiability,
         arr = InvestmentReturnAssumption_GASB,
         return = InvestmentReturn_1yr,
         payroll_growth = PayrollGrowthAssumption,
         nc = NormCostRate_tot,
         ben_pay = expense_TotBenefits) %>% 
  mutate(fye = ymd(fye),
         month = month(fye), .after = fye)

#Impute missing normal costs for New York State Teachers, NY State & Local ERS, and NY State & Local Police & Fire
ppd <- ppd %>% 
  mutate(nc = replace(nc, plan_name == "New York State Teachers" & fy == 2020, 0.1),
         nc = replace(nc, plan_name == "NY State & Local ERS" & fy == 2020, 0.17),
         nc = replace(nc, plan_name == "NY State & Local Police & Fire" & fy == 2020, 0.3))

#Fix wrong data:
ppd <- ppd %>% 
  mutate(aal = replace(aal, plan_name == "Nebraska Schools" & fy == 2001, 4576863))

#Add a 2023 row to each plan
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


#Initial projection & add latest returns data
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
  left_join(latest_returns_data) %>% 
  ungroup()


#Identify years that have missing AAL/MVA data or have duplicated AAL data
#1 means the UAL number has been officially reported in the data set; 0 means the UAL number is estimated by the model
ppd_project <- ppd_project %>% 
  mutate(ual_official = case_when(is.na(aal) & is.na(tpl) ~ 0,
                                  is.na(mva) ~ 0,
                                  aal == lag(aal) & is.na(tpl) ~ 0, 
                                  TRUE ~ 1), .before = aal) %>% 
  #Identify years that have missing returns
  #1 means the returns have been officially reported in the data set or in the "latest return" file; 0 means the returns are missing.
  mutate(returns_official = ifelse(is.na(return) & is.na(latest_return), 0, 1), .before = return)


#Join pension data with index returns and solve the benchmark portfolio for each plan, then use the benchmark portfolio to estimate the pension funds' returns
ppd_benchmark <- ppd_project %>% 
  left_join(index_returns) %>% 
  group_by(plan_name) %>% 
  mutate(benchmark = benchmark_portfolio(return, acwi_exUS, IWV, VBTIX),
         benchmark_return = benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX,
         predict_return = benchmark[[1]][1] + benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX) %>%
  ungroup()


#Processing inflation data
cpi_raw <- read.csv(paste(FolderName, "/", "cpi.csv", sep = "")) %>% rename(fy = year)

#find the number of remaining months for the update year
latest_month <- cpi_raw$month[nrow(cpi_raw)]
remain_months <- 12 - latest_month

#create a data frame to impute missing cpi values for the rest of the update year
#note that this data frame is empty if there're no missing cpi values
if (remain_months != 0) {
  cpi_impute <- tibble(fy = rep(latest_update_year, remain_months),
                       month = (remain_months+1):12,
                       cpi = NA)
} else {
  cpi_impute <- data.frame()
}

#add the cpi_impute to the cpi_raw table and impute missing cpi values with the latest inflation rate
cpi <- bind_rows(cpi_raw, cpi_impute) %>% 
  arrange(month) %>% 
  group_by(month) %>% 
  mutate(inflation_rate = cpi/lag(cpi) - 1) %>% 
  ungroup() %>% 
  mutate(inflation_rate = ifelse(fy == latest_update_year & is.na(inflation_rate),
                                 inflation_rate[fy == latest_update_year & month == latest_month],
                                 inflation_rate),
         cpi = ifelse(is.na(cpi),
                      lag(cpi) * (1 + inflation_rate),
                      cpi))




#Notable plans that may have issues:
#Missouri Local Government Employees Retirement System
#Arkansas Public Employees Retirement System
#Nebraska Public Employees Retirement System - School Employees Plan
#North Carolina Local Governmental Employees' Retirement System
#Employees' Retirement System of the State of Hawaii
#Massachusetts State Retirement System
#State Employees' Retirement System of Illinois
#Massachusetts Teachers' Retirement System
#Teachers' and State Employees' Retirement System of North Carolina


