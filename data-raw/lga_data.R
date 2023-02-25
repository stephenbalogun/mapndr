## code to prepare `lga_data` dataset goes here

library(rvest)
library(tidyverse)

url <- "https://www.citypopulation.de/en/nigeria/admin/"

html <- read_html(url)

lga_table <- html |>
  html_element("table") |>
  html_table() |>
  janitor::clean_names()

lga_abia <- lga_table |>
  head(18) |>
  slice(-1) |>
  select(1, 5) |>
  mutate(
    state = "Abia",
    .before = name
  )

names(lga_abia) <- c("state", "lga", "population")

lga_data <- lga_abia |>
  mutate(
    population = parse_number(population),
    prev_x = population / sum(population),
    prev_x = case_when(prev_x < 0.05 ~ "< 5%",
      between(prev_x, 0.05, 0.09) ~ "5% - 9%",
      .default = "10%+"
    ),
    prev_x = factor(prev_x, levels = c("< 5%", "5% - 9%", "10%+")),
    incidence_x = janitor::round_half_up(population * runif(1, max = 0.6)),
    lga = str_replace_all(lga, "-", " "),
    lga = ifelse(lga == "Osisioma Ngwa", "Osisioma", lga)
  )


usethis::use_data(lga_data, overwrite = TRUE)
