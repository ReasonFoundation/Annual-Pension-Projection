server <- function(input, output, session) {
    mod_comm <- reactiveValues(
        app_data = NULL
    )
    
    
    final_data <- mod_return_select_server("return_select")
    mod_ual_fr_plan_server("ual_fr_plan", projection_data = final_data)
    mod_ual_fr_us_server("ual_fr_us", projection_data = final_data)
    mod_ual_map_server("ual_map", projection_data = final_data)


}