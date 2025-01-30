library(readr)
library(dplyr)
library(stringr)
library(readxl)
istat_all_hotels <- read_excel("data/istat_all_hotels.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "skip", "numeric"))

df <- read_csv("data/cozycozy-1g1n1m-tc.csv", 
               col_types = cols(accommodationId = col_character(), 
                                uid = col_skip(), locationText = col_skip(), 
                                cityName = col_skip(), cancellationCategory = col_skip(), 
                                cancellationPolicy = col_skip(), 
                                string = col_skip(), highlightedResults.uid = col_skip(), 
                                highlightedResults.totalPrice.currencyCode = col_skip(), 
                                highlightedResults.shortText = col_skip(), 
                                details.address = col_skip()))

hotels <- df |>
  filter(scraped_at=="2024-12-10") |>
  select(accommodationId, coordinates.latitude, coordinates.longitude, 
         details.name, details.stars, details.address.first, details.address.last,
         title) |>
  distinct() |>
  filter(!is.na(details.name)) |>
  filter(str_detect(title, "hotel"))

table(hotels$details.stars)

write_csv(hotels, "data/hotels_raw.csv")
