library(readr)
library(readxl)
library(dplyr)
library(stringr)


istat_all_hotels <- read_excel("data/istat_all_hotels.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "skip", "numeric"))
hotels <- read_csv("data/hotels_prov.csv")

table(floor(hotels$details.stars))

istat_clean <- istat_all_hotels |>
  mutate(TIPO_ALLOGGIO2=factor(TIPO_ALLOGGIO2)) |>
  mutate(details.stars = as.integer(str_extract(TIPO_ALLOGGIO2, "^\\d"))) |>
  mutate(total=Value) |>
  mutate(DEN_UTS=Territorio) |>
  select(DEN_UTS, details.stars, total) |>
  mutate(DEN_UTS = str_replace_all(DEN_UTS, "Valle d'Aosta / Vallée d'Aoste", "Aosta")) |>
  mutate(DEN_UTS = str_replace_all(DEN_UTS, "Bolzano / Bozen", "Bolzano")) |>
  mutate(DEN_UTS = str_replace_all(DEN_UTS, "Massa-Carrara", "Massa Carrara")) |>
  mutate(DEN_UTS = str_replace_all(DEN_UTS, "Forlì-Cesena", "Forli'-Cesena")) |>
  mutate(DEN_UTS = str_replace_all(DEN_UTS, "Cagliari", "Sud Sardegna"))

coverage <- hotels |>
  mutate(details.stars=floor(details.stars)) |>
  filter(details.stars>0) |>
  group_by(DEN_UTS, details.stars) |>
  mutate(cov=n()) |>
  ungroup() |>
  distinct(DEN_UTS, details.stars, cov) 
  

table(coverage$details.stars)

coverage_joined <- coverage %>%
  inner_join(istat_clean, by = c("DEN_UTS", "details.stars")) |>
  mutate(cov_rate=(cov/total)*100)

write_csv(coverage_joined, "results/coverage.csv")

hist(coverage_joined$cov_rate)
# unique(coverage$DEN_UTS)[!unique(coverage$DEN_UTS) %in% unique(coverage_joined$DEN_UTS)]


