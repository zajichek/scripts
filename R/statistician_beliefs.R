library(tidyverse)
library(googlesheets4)

gs4_deauth()
dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aNEirWNtuGRpyIg8phpkj2ExnGy6E-_yi3OT3YOx0UM/edit?usp=sharing")

dat |>
  mutate(
    `I am a self-proclaimed:` = 
      case_when(
        `I am a self-proclaimed:` %in% c("Frequentist", "Bayesian", "Undecided") ~ `I am a self-proclaimed:`,
        TRUE ~ paste0("Other (", `I am a self-proclaimed:`, ")")
      ) |>
        factor() |>
        fct_relevel(
          "Frequentist",
          "Bayesian",
          "Undecided"
        ),
    `Do you believe in God?` = 
      case_when(
        `Do you believe in God?` %in% c("Yes", "No", "Unsure") ~ `Do you believe in God?`,
        TRUE ~ paste0("Other (", `Do you believe in God?`, ")")
      ) |>
        factor() |>
        fct_relevel(
          "Yes",
          "No",
          "Unsure"
        )
  ) |>
  
  summarize(
    N = n(),
    .by = 
      c(
        `I am a self-proclaimed:`,
        `Do you believe in God?`
      )
  ) |>
  
  pivot_wider(
    names_from = `Do you believe in God?`,
    values_from = N,
    values_fill = 0
  ) |>
  
  knitr::kable(format = "html") |>
  kableExtra::kable_styling(full_width = FALSE) |>
  kableExtra::add_header_above(c("", "Do you believe in God?" = 4)) |>
  kableExtra::add_footnote(paste0("Results as of ", Sys.time()), notation = "none") |>
  kableExtra::add_footnote("Source: https://docs.google.com/spreadsheets/d/1aNEirWNtuGRpyIg8phpkj2ExnGy6E-_yi3OT3YOx0UM", notation = "none")
