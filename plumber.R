# Define a function to clear the workspace
clear_workspace <- function() {
  rm(list = ls())
  gc()
}

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  plumber::forward()
}

# make the model
source("Pension projection master.R")
#* @get /predict_funding
plan_output <- function(input_return_, inf_adj_){
  projection_f(
    input_return = as.numeric(input_return_), 
    inf_adj = as.logical(inf_adj_)
  )
}