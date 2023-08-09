library(shiny)
library(shinyWidgets)
library(echarts4r)
library(reactable)
source("Pension projection master.R")

#Create a plan look-up list
plan_lookup <- ppd_benchmark %>% select(state, plan_full_name) %>% distinct()

test <- projection_f(input_return = 0.05)

#Custom charting/table functions
#Funded ratio charting function
funded_ratio_echart <- function(data) {
  data %>% 
    mutate(funded_ratio_real = ifelse(ual_official == 1, funded_ratio, NA),
           fy = as.character(fy)) %>% 
    e_chart(fy) %>%  
    e_line(funded_ratio, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) %>% 
    e_line(funded_ratio_real, symbolSize = 7, lineStyle = list(width = 3)) %>% 
    e_y_axis(formatter = e_axis_formatter("percent")) %>% 
    e_color(c("#FF6633", "#FF6633")) %>% 
    e_tooltip(trigger = "item",
              confine = T,
              formatter = htmlwidgets::JS("function(params) {
            x = Math.round(params.value[1] * 100)
                return (params.value[0] + ': ' + x + '%')
                }
    ")) %>% 
    e_legend(F) %>% 
    e_grid(left = "10%", bottom = "8%") %>% 
    e_title("Funded Ratio")
}

#UAL charting function
ual_echart <- function(data) {
  data %>% 
    mutate(ual_real = ifelse(ual_official == 1, ual, NA),
           fy = as.character(fy)) %>% 
    e_chart(fy) %>%  
    e_line(ual, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) %>% 
    e_line(ual_real, symbolSize = 7, lineStyle = list(width = 3)) %>% 
    e_color(c("#FF6633", "#FF6633")) %>% 
    e_y_axis(formatter = htmlwidgets::JS("function (value) {
                                                 if(value >= 0) {
                                                  x = '$' + echarts.format.addCommas(value) + 'M';
                                                 } else {
                                                  x = '-' + '$' + echarts.format.addCommas(Math.abs(value)) + 'M';
                                                 }
                                                 return(x)}")) %>% 
    e_tooltip(trigger = "item",
              confine = T,
              formatter = htmlwidgets::JS("function (params) {
                                                    if(params.value[1] >= 0) {
                                                    x = '$' + echarts.format.addCommas(Math.round(params.value[1])) + 'M';
                                                    } else {
                                                    x = '-' + '$' + echarts.format.addCommas(Math.abs(Math.round(params.value[1]))) + 'M';
                                                    }
                                                    return(params.value[0] + ': ' + x)}")) %>% 
    e_legend(F) %>% 
    e_grid(left = "12%", bottom = "8%") %>% 
    e_title("Unfunded Liability - based on MVA")  
}

#Table function
pension_table <- function(data, type) {
  reactable(
    data = data,
    searchable = T,
    filterable = T,
    showSortable = T,
    striped = T,
    highlight = T,
    # minRows = 20,
    defaultPageSize = 22,
    columns = list(
      fy = colDef(name = "Fiscal Year"),
      plan_name = colDef(name = "Plan", show = ifelse(type == "plan", T, F)),
      state = colDef(name = "State"),
      aal = colDef(name = "Accrued Liability", format = colFormat(prefix = "$", separators = T, suffix = "M", digits = 0)),
      mva = colDef(name = "Market Assets", format = colFormat(prefix = "$", separators = T, suffix = "M", digits = 0)),
      ual = colDef(name = "Unfunded Liability", format = colFormat(prefix = "$", separators = T, suffix = "M", digits = 0)),
      funded_ratio = colDef(name = "Funded Ratio", format = colFormat(percent = T, digits = 0))
    )
  )
}


######################################################################
##### Shiny App ######################################################

ui <- fluidPage(
  tags$head(
    tags$style(HTML("

      .form-group.shiny-input-container {
        width: 500px;
        
      }

    "))
  ),
  
  
  
  fluidRow(
    column(6, pickerInput("state", "Select a state:", 
                          choices = unique(plan_lookup$state), 
                          selected = unique(plan_lookup$state)[1],
                          options = list(
                            "live-search" = T
                          ))),
    
    column(6, pickerInput("plan", "Select a plan:", 
                          choices = plan_lookup$plan_full_name[plan_lookup$state == "Alabama"],
                          options = list(
                            "live-search" = T
                          ))),
    
    column(6, numericInputIcon("return_2023", "Enter a rate of return for FY2023:",
                               value = 6, step = 0.5,
                               icon = list(NULL,icon("percent")))),
    
    column(6, radioGroupButtons("inf_adj", "Inflation Adjustment",
                                choiceNames = c("Yes", "No"),
                                choiceValues = c(T, F),
                                selected = F))
  ),
  
  tabsetPanel(
    tabPanel("Plan",
             fluidRow(
               column(6, echarts4rOutput("plan_fr")),
               column(6, echarts4rOutput("plan_ual"))
             ),
             reactableOutput("plan_table")
    ),
    
    tabPanel("State",
             fluidRow(
               column(6, echarts4rOutput("state_fr")),
               column(6, echarts4rOutput("state_ual"))
             ),
             reactableOutput("state_table")
    ),
    
    tabPanel("National",
             fluidRow(
               column(6, echarts4rOutput("us_fr")),
               column(6, echarts4rOutput("us_ual"))
             ))
    
  )
)

server <- function(input, output, session) {
  #Link state input with plan input 
  observeEvent(input$state, {
    updatePickerInput(session, "plan", choices = plan_lookup$plan_full_name[plan_lookup$state == input$state])
  })
  
  #Overall output
  model_output <- reactive({
    projection_f(input_return = input$return_2023/100, inf_adj = input$inf_adj) %>% 
      mutate(ual = ual/1000)
    })
  
  #Output for the selected plan
  plan_output <- reactive({
    model_output() %>% 
      filter(plan_full_name == input$plan)
  })
  
  #Output for the selected state
  state_output <- reactive({
    model_output() %>% 
      filter(state == input$state, type == "state")
  })
  
  #Output for the nation
  us_output <- reactive({
    model_output() %>% 
      filter(state == "USA")
  })
  
  #Output for all plans only (for the table below the charts)
  plans_table_output <- reactive({
    model_output() %>% 
      filter(type == "plan") %>% 
      select(-plan_full_name, -ual_official, -returns_official, - type)
  })
  
  #Output for all states only (for the table below the charts)
  states_table_output <- reactive({
    model_output() %>% 
      filter(type == "state") %>% 
      select(-plan_full_name, -ual_official, -returns_official, - type)
  })
  
  #Plan charts
  output$plan_fr <- renderEcharts4r({
    funded_ratio_echart(plan_output())
  })
  
  output$plan_ual <- renderEcharts4r({
    ual_echart(plan_output())
  })
  
  #Plans table
  output$plan_table <- renderReactable({
    pension_table(plans_table_output(), type = "plan")
  })
  
  #State charts
  output$state_fr <- renderEcharts4r({
    funded_ratio_echart(state_output())
  })
  
  output$state_ual <- renderEcharts4r({
    ual_echart(state_output())
  })
  
  #States table
  output$state_table <- renderReactable({
    pension_table(states_table_output(), type = "state")
  })
  
  #National charts
  output$us_fr <- renderEcharts4r({
    funded_ratio_echart(us_output())
  })
  
  output$us_ual <- renderEcharts4r({
    ual_echart(us_output())
  })
  
}

shinyApp(ui = ui, server = server)


