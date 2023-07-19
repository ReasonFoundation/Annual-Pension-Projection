rm(list = ls())
library(tidyverse)
library(readxl)
library(plotly)
library(formattable)
library(scales)
library(lubridate)
library(quantmod)
library(quadprog)
library(rio)
library(fredr)


#Get initial inputs
source("Pension projection initial inputs.R")

#Get index returns and cpi data (only run the code below for periodic updates)
# source("Index returns & CPI.R")

#Get projection functions
source("Pension projection functions.R")

#Data preparation
source("Pension projection data prep.R")

#To run the model, use function projection_f(). The function has three inputs:
#1. input_return: this is the return input by the user
#2. inf_adj: apply inflation adjustment or not. Default is FALSE (not applying inflation adjustment)
#3. output_type: "plan", "state", or "us" (national)


#Examples:
# plan_output <- projection_f(input_return = 0.2)
# plan_output_inf_adj <- projection_f(input_return = 0.2, inf_adj = T, output_type = "plan")
# 
# state_output <- projection_f(input_return = 0.05, output_type = "state")
# 
# us_output <- projection_f(input_return = 0.05, output_type = "us")






