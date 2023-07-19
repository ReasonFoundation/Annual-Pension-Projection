mod_ual_fr_plan_ui <- function(id) {
    ns <- NS(id)

    tagList(
      
      
      fluidRow(
        
        div(
          style = "padding-left: 20px; max-width: 45%; z-index: 900",
          align = "left",
          shinyWidgets::pickerInput(
            ns("plan"),
            label = "Select State Plan(s):",
            choices = unique(ppd_project$plan_full_name),
            options = list(
              `actions-box` = T,
              `live-search` = T
            ),
            multiple = T,
            width = "80%",
            selected = "Employees' Retirement System of Alabama"
          )
        )
      ),
      
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

mod_ual_fr_plan_server <- function(id, projection_data) {
    server <- function(input, output, session) {
        ns <- session$ns
        
        # Plot UAL
        output$chart_1 <- renderEcharts4r({
          
          ##Select plans
          projection_data() |>
            mutate(ual = ual/1000) |>
            filter(plan_full_name %in% input$plan) |>
            group_by(plan_name) |>
            mutate(ual_real = ifelse(real_vs_estimate == "real", ual, NA),
                   fy = as.character(fy)) |>
            e_charts(fy) |>
            e_line(ual, symbolSize = 7, lineStyle = list(width = 3, type = "dotted"), bind = plan_name) |>     #plot the dotted line first and the solid line later so that the solid line will block the dots
            e_line(ual_real, symbolSize = 7, lineStyle = list(width = 3), bind = plan_name) |>
            e_y_axis(formatter = htmlwidgets::JS("function (value) {
                                                 if(value >= 0) {
                                                  x = '$' + echarts.format.addCommas(value) + 'M';
                                                 } else {
                                                  x = '-' + '$' + echarts.format.addCommas(Math.abs(value)) + 'M';
                                                 }
                                                 return(x)}")) |>
            e_theme_custom("echarts_theme.json") |>
            e_title("Unfunded Liability") |>
            e_tooltip(trigger = "item",
                      confine = T,
                      formatter = htmlwidgets::JS("function (params) {
                                                    if(params.value[1] >= 0) {
                                                    x = '$' + echarts.format.addCommas(Math.round(params.value[1])) + 'M';
                                                    } else {
                                                    x = '-' + '$' + echarts.format.addCommas(Math.abs(Math.round(params.value[1]))) + 'M';
                                                    }
                                                    return(params.value[0] + '<br/>' +
                                                           params.marker + params.name + ': ' + x)}")) |>
            e_legend(type = "scroll", top = "7%") |>
            e_grid(left = "12%", bottom = "8%") |>
            e_group("grp")
            
          
          
          
        })
        
        # Plot Funded Ratio
        output$chart_2 <- renderEcharts4r({
          
          ##Select plans
          projection_data() |> 
            filter(plan_full_name %in% input$plan) |>
            group_by(plan_name) |>
            mutate(funded_ratio_real = ifelse(real_vs_estimate == "real", funded_ratio, NA),
                   fy = as.character(fy)) |>
            e_charts(fy) |>
            e_line(funded_ratio, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) |>     #plot the dotted line first and the solid line later so that the solid line will block the dots
            e_line(funded_ratio_real, symbolSize = 7, lineStyle = list(width = 3)) |>
            e_theme_custom("echarts_theme.json") |>
            e_title("Funded Ratio") |>
            e_tooltip(trigger = "item",
                      confine = T,
                      formatter = e_tooltip_item_formatter("percent", digits = 1)) |>
            e_y_axis(formatter = e_axis_formatter("percent")) |>
            e_legend(type = "scroll", top = "7%") |>
            e_grid(left = "10%", bottom = "8%") |>
            e_group("grp") |>
            e_connect_group("grp")
          
        })
          
    }
    moduleServer(id, server)
}