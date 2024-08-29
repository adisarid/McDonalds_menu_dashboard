library(tidyverse)

mcdonalds <- read_csv("data/mcdonaldata_UK.csv")

mcdonalds |> 
  mutate(calories = as.numeric(str_remove(product_calories, "kcal: ")))

