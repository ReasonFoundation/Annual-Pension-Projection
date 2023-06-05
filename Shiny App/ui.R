div(
  class = "navbar",
  navbarPage(
    title = div(img(src="https://reason.org/wp-content/themes/reason-dot-org-theme/resources/assets/img/logos/logo-horizontal.svg", height = "25px"), ""),
    theme = "css/custom.css",
    collapsible = T,
    position = "static-top",
    header = mod_return_select_ui("return_select"),
    
    tabPanel("State Plan Projections",
             mod_ual_fr_plan_ui("ual_fr_plan")
    ),
    
    tabPanel("National Projections",
             mod_ual_fr_us_ui("ual_fr_us")
    ),
    
    tabPanel("Map",
             mod_ual_map_ui("ual_map")
    )
  )
)