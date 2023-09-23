source(file = "Pension projection master.R")

library(stringr)

state_table_output <- function(input_return) {
  input_return_text <- as.character(input_return)
  table_output <- projection_f(input_return = input_return) %>% 
    filter(fy %in% c(2023),
           type %in% c("state", "national")) %>% 
    select(fy, state, ual, funded_ratio) %>% 
    pivot_wider(values_from = c(ual, funded_ratio), names_from = fy) %>% 
    rename_with(.fn = ~ str_replace(.x, "2023", paste0("2023_", str_sub(input_return_text, str_length(input_return_text)))),
                .col = ends_with("2023"))
  
  return(table_output)
}


state_table_output_combined <- function(return_scen) {
  output_df <- tibble()
  for (i in 1:length(return_scen)) {
    output_single <- state_table_output(return_scen[i])
    if (i == 1) {
      output_df <- output_single
    } else {
      output_df <- output_df %>% left_join(output_single)
    }
  }
  return(output_df)
}


state_table_output_combined_2 <- function(return_scen) {
  output_list <- list()
  for (i in 1:length(return_scen)) {
    output_list[i] <- list(state_table_output(return_scen[i]))
  }
  output_df <- output_list %>% reduce(left_join)

  return(output_df)
}




state_output <- state_table_output_combined_2(return_scen = c(0.05, 0.07, 0.09)) %>% 
  relocate(ual_2023_7, .after = ual_2023_5) %>% 
  relocate(ual_2023_9, .after = ual_2023_7) %>% 
  mutate(across(ual_2023_5:ual_2023_9, ~ .x / 1000000))


# state_output <- state_output_5 %>% 
#   left_join(state_output_9) %>% 
#   relocate(ual_2023_9, .after = ual_2023_5) %>% 
#   relocate(funded_ratio_2023_9, .after = funded_ratio_2023_5) %>% 
#   mutate(across(ual_2022:ual_2023_9, ~ .x / 1000000))

export(state_output, "State Output Table.xlsx")

