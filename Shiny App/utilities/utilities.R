

# UI

text_card <- function(..., header = NULL) {
  div(
    class = "card",
    style = "margin: 0px; border-radius: 10px;",
    header, 
    div(class = "card-content", ..., style = " border-radius: 10px; color: #333; background-color: #fff; margin-top: 20px;", align = "center")
  )
}

data_card <- function(..., header = NULL) {
  div(
    class = "card",
    style = "margin: 0px;  border-radius: 10px;",
    header, 
    div(class = "card-content", ..., style = " border-radius: 10px; color: #333; background-color: #fff; margin-top: 20px;", 
        align = "left")
  )
}


# Projections


#Custom functions to project some key variables
fy_f <- function(fy){       
  for(i in 2:length(fy)) {
    if(is.na(fy[i])){
      fy[i] <- fy[i-1] + 1
    }
  }
  return(fy)
}


fye_f <- function(fye){       
  for(i in 2:length(fye)) {
    if(is.na(fye[i])){
      fye[i] <- fye[i-1] + years(1)
    }
  }
  return(fye)
}


get_last_f <- function(x) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}


payroll_growth_f <- function(payroll_growth, payroll_growth_avg) {    #if the payroll growth assumption isn't available in any year, use the average payroll growth rate. Otherwise, use the latest available payroll growth assumption
  if(sum(!is.na(payroll_growth)) == 0) {   
    payroll_growth <- payroll_growth_avg
  } else {
    payroll_growth <- get_last_f(payroll_growth)
  }
  return(payroll_growth)
}


growth_f <- function(x, g) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}


#Custom functions to project return, aal, and mva
#return function
return_f <- function(return, predict_return, fy, proj_return) {  
  for (i in 2:length(return)) {
    if (is.na(return[i]) && fy[i] <= current_fy) {   #if returns are missing in current or previous fy, use the returns estimated by the benchmark portfolio
      return[i] <- predict_return[i]
    } else if (fy[i] > current_fy) {
      return[i] <- proj_return
    }
  }
  return(return)
}

#AAL function
aal_f <- function(aal, arr, payroll, nc, ben_pay) {
  for(i in 2:length(aal)) {
    if(is.na(aal[i]) || isTRUE(aal[i] == aal[i-1])) {
      aal[i] <- aal[i-1]*(1 + arr[i]) + (nc[i]*payroll[i] + ben_pay[i])*(1 + arr[i])^0.5
    }
  }
  return(aal)
}

#MVA function
mva_f <- function(mva, return, payroll, cont_rate, ben_pay) {
  for(i in 2:length(mva)) {
    if(is.na(mva[i])) {
      mva[i] <- mva[i-1]*(1 + return[i]) + (cont_rate[i]*payroll[i] + ben_pay[i])*(1 + return[i])^0.5
    }
  }
  return(mva)
}

