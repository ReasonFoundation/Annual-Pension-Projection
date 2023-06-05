mod_return_select_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(4),
      column(4,
             style = "position: absolute; left: 33.5%; top: 77px; z-index: 999; text-align: center;",
             div(
               style = "margin: 0; font-size: 12px;",
               shinyWidgets::radioGroupButtons(
                 ns("inf_adjustment"),
                 label = "Inflation Adjustment:",
                 choices = c("On" = T, "Off" = F),
                 selected = F)
             )
      ),
      
      
      column(3,
             style = "position: absolute; right: 1%; top: 65px; max-width: 45%; z-index: 999; text-align: right; font-size: 12px;",
             shinyWidgets::chooseSliderSkin("Flat"),
             shinyWidgets::setSliderColor(c("LightSlateGray", "LightSlateGray","LightSlateGray"), c(1, 2, 3)),
               sliderInput(ns("return"),
                           label = "Fiscal Year 2022's Investment Return:",
                           min = -20,
                           max = 20,
                           post = " %",
                           value = -6,
                           sep = "")
      )
    )
  )
}

mod_return_select_server <- function(id) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    ppd_project_final <- reactive({
      
      ppd_project %>% 
        left_join(ppd_benchmark) %>%
        left_join(cpi) %>% 
        group_by(plan_name) %>% 
        mutate(return = return_f(return, predict_return, fy, proj_return = (input$return /100)),
               inf_adj = input$inf_adjustment,
               aal = aal_f(aal, arr, payroll, nc, ben_pay),
               aal = ifelse(inf_adj == T, aal * cpi2[fy == 2022] / cpi2, aal),
               mva = mva_f(mva, return, payroll, cont_rate, ben_pay),
               mva = ifelse(inf_adj == T, mva * cpi2[fy == 2022] / cpi2, mva)) %>% 
        mutate(ual = aal - mva, 
               funded_ratio = mva/aal,
               .after = mva) %>% 
        ungroup()
    })
    
    return(ppd_project_final)
    
  }
  moduleServer(id, server)
}