rm(list = ls())
# library(tidyverse)
library(purrr)
library(dplyr)
library(tidyr)
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

#To run the model, use function projection_f(). The function has two inputs:
#1. input_return: this is the return input by the user
#2. inf_adj: apply inflation adjustment or not. Default is FALSE (not applying inflation adjustment)


#Examples:
output <- projection_f(input_return = 0.05)
# output_inf <- projection_f(input_return = 0.05, inf_adj = T)