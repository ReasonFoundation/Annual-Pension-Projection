source(file = "Pension projection master.R")

library(stringr)

state_table_output <- function(input_return) {
  input_return_text <- as.character(input_return)
  table_output <- projection_f(input_return = input_return) %>% 
    filter(fy %in% c(2022, 2023),
           type == "state") %>% 
    select(fy, state, ual, funded_ratio) %>% 
    pivot_wider(values_from = c(ual, funded_ratio), names_from = fy) %>% 
    rename_with(.fn = ~ str_replace(.x, "2023", paste0("2023_", str_sub(input_return_text, str_length(input_return_text)))),
                .col = ends_with("2023"))
  
  return(table_output)
}


state_output_5 <- state_table_output(0.05)
state_output_9 <- state_table_output(0.09)

state_output <- state_output_5 %>% 
  left_join(state_output_9) %>% 
  relocate(ual_2023_9, .after = ual_2023_5) %>% 
  relocate(funded_ratio_2023_9, .after = funded_ratio_2023_5)
