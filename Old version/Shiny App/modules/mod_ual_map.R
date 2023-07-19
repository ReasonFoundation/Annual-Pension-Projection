mod_ual_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(6,
             style = "max-width: 50%;",
             h3("..", style = "color: #f5f5f5; text-align: left; margin-left: 10px;")
      ),
      column(6)
    ),
    
    fluidRow(
      style = "margin: 0; height: 46%",
      
      column(
        width = 12,
        style = "height: 100%; padding-top: -30px",
        data_card(
          shinycssloaders::withSpinner(echarts4rOutput(ns("chart_1"), height = "65vh"))
        )
      )
    )
  )
}

mod_ual_map_server <- function(id, projection_data) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    ppd_project_state <- reactive({     
      projection_data() %>%     
        group_by(state, fy) %>% 
        summarise(state_aal = sum(aal),
                  state_mva = sum(mva),
                  state_ual = state_aal - state_mva,
                  state_funded_ratio = state_mva/state_aal) %>% 
        ungroup() 
    })
    
    # Map
    output$chart_1 <- renderEcharts4r({
      
      ppd_project_state() %>% 
        group_by(fy) %>% 
        e_chart(state, 
                # width = "1200",
                timeline = T) %>% 
        e_map_register("USA", json) %>% 
        e_map(select = list(disabled = T),
              state_funded_ratio,
              map = "USA",
              zoom = 2.25,
              bottom = "16%",
              label = list(show = F),
              itemStyle = list(areaColor = "#A69FA1",
                               borderColor = "#fff",
                               borderWidth = 2),
              emphasis = list(itemStyle = list(areaColor = "#6699cc",
                                               borderColor = "#fff",
                                               borderWidth = 2)),
              aspectScale = 1,
              layoutCenter = c("100%", "30%"),
              roam = F) %>% 
        e_text_g(top = "51%",
                 right = "6%",
                 z = 999,
                 style = list(text = "Funded Ratio",
                              fontFamily = "'Open Sans', sans-serif",
                              fontStyle = "bolder",
                              color = '#333',
                              fontSize = 15)) %>% 
        e_visual_map(type = "piecewise",
                     top = "55%",
                     right = "5%",
                     align = "left",
                     splitList = list(
                       list(min = 1, label = ">100%"),
                       list(min = 0.9, max = 0.999999, label = "90%-100%"),
                       list(min = 0.8, max = 0.899999, label = "80%-90%"),
                       list(min = 0.7, max = 0.799999, label = "70%-80%"),
                       list(min = 0.6, max = 0.699999, label = "60%-70%"),
                       list(max = 0.599999, label = "<60%")
                     ),
                     textStyle = list(color = "#333",
                                      fontSize = 15),
                     formatter = e_axis_formatter(style = "percent"),
                     outOfRange = list(
                       color = "#A69FA1"
                     ),
                     inRange = list(
                       color = rev(c("#66bd63", "#d9ef8b","#fee08b",
                                     "#fdae61", "#f46d43","#d73027")))
        ) %>% 
        e_timeline_opts(tooltip = list(show = F),
                        autoPlay = F,
                        playInterval = 1000,
                        currentIndex = 21,
                        axis_type = "category",
                        right = "5%",
                        left = "5%",
                        bottom = "0%",
                        padding = c(0,0,0,0),
                        label = list(fontSize = 11)) %>% 
        e_tooltip(formatter = htmlwidgets::JS("function(params){
                                                       return (params.marker + params.name + ': ' + Math.round(params.value * 100) + '%')
                                                       }")) |>
        e_title("State Pension Funded Ratio")
      

      
      
    })

    
    
  }
  moduleServer(id, server)
}