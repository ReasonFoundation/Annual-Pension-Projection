mod_ual_fr_us_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(style = "height: 73px"),
    
    fluidRow(
      style = "margin: 0; height: 46%",
      
      column(
        width = 6,
        style = "height: 100%; padding-top: -20px",
        data_card(
          shinycssloaders::withSpinner(echarts4rOutput(ns("chart_1"), height = "60vh"))
        )
      ),
      
      column(
        width = 6,
        style = "height: 100%; padding-top: -20px",
        data_card(
          shinycssloaders::withSpinner(echarts4rOutput(ns("chart_2"), height = "60vh"))
        )
      )
    )
  )
}

mod_ual_fr_us_server <- function(id, projection_data) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    #Calculate the national funding metrics
    ppd_project_us <- reactive({
      projection_data() %>% 
        group_by(fy) %>% 
        summarise(us_aal = sum(aal, na.rm = T),
                  us_mva = sum(mva, na.rm = T),
                  us_ual = (us_aal - us_mva)/1000000,
                  us_funded_ratio = us_mva/us_aal)
    })
      
    
    # Plot UAL
    output$chart_1 <- renderEcharts4r({
      
      ppd_project_us() |> 
        mutate(us_ual_real = ifelse(fy <= 2020, us_ual, NA),
               fy = as.character(fy)) |>
        e_charts(fy) |>
        e_line(us_ual, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) |>     #plot the dotted line first and the solid line later so that the solid line will block the dots
        e_line(us_ual_real, symbolSize = 7, lineStyle = list(width = 3)) |>
        # e_theme_custom("echarts_theme.json") |>
        e_title("Aggregate Unfunded Liability") |>
        e_y_axis(formatter = htmlwidgets::JS("function (value) {
                                                 if(value >= 0) {
                                                  x = '$' + echarts.format.addCommas(value) + 'B';
                                                 } else {
                                                  x = '-' + '$' + echarts.format.addCommas(Math.abs(value)) + 'B';
                                                 }
                                                 return(x)}")) |>
        e_x_axis(axisTick = list(alignWithLabel = T)) |>
        e_tooltip(trigger = "item",
                  confine = T,
                  formatter = htmlwidgets::JS("function (params) {
                                                    if(params.value[1] >= 0) {
                                                    x = '$' + echarts.format.addCommas(Math.round(params.value[1])) + 'B';
                                                    } else {
                                                    x = '-' + '$' + echarts.format.addCommas(Math.abs(Math.round(params.value[1]))) + 'B';
                                                    }
                                                    return(params.value[0] + '<br/>' +
                                                           params.marker + 'Aggregate Unfunded Liability' + ': ' + x)}")) |>
        e_legend(show = F) |>
        e_grid(left = "12%", bottom = "8%") |>
        e_theme_custom("echarts_theme.json") |>
        e_color(c("#f63", "#f63")) |>
        e_group("grp")
        
        
    })
    
    # Plot Funded Ratio
    output$chart_2 <- renderEcharts4r({
    
      ppd_project_us() |> 
        mutate(us_funded_ratio_real = ifelse(fy <= 2020, us_funded_ratio, NA),
               fy = as.character(fy)) |>
        e_charts(fy) |>
        e_line(us_funded_ratio, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) |>     #plot the dotted line first and the solid line later so that the solid line will block the dots
        e_line(us_funded_ratio_real, symbolSize = 7, lineStyle = list(width = 3)) |>
        # e_theme_custom("echarts_theme.json") |>
        e_title("Aggregate Funded Ratio") |>
        e_tooltip(
          trigger = "item",
          confine = T,
          formatter = htmlwidgets::JS("function (params) {
                                        return(
                                        params.value[0] + '<br/>' +
                                        params.marker +
                                        'Aggregate Funded Ratio: ' +
                                       (params.value[1] * 100).toFixed(1) +
                                        '%'
                                                     )}")) |>
        e_legend(show = F) |>
        e_theme_custom("echarts_theme.json") |>
        e_color(c("#f63", "#f63")) |>
        e_y_axis(formatter = e_axis_formatter("percent")) |>
        e_x_axis(axisTick = list(alignWithLabel = T)) |>
        e_grid(left = "10%", bottom = "8%") |>
        e_group("grp") |>
        e_connect_group("grp")
      
    })
    
    
  }
  moduleServer(id, server)
}
