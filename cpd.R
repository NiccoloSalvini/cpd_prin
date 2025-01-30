library(readr)
library(dplyr)
library(broom)
library(stringr)

hotels <- read_csv("data/hotels_prov.csv", col_types = cols(coordinates.latitude = col_skip(), 
                                                            coordinates.longitude = col_skip(), 
                                                            details.address.first = col_skip(), 
                                                            details.address.last = col_skip(), 
                                                            title = col_skip()))


prices <- read_csv("data/cozycozy-1g1n1m-tc.csv", 
                   col_types = cols(uid = col_skip(), title = col_skip(), 
                                    subTitle = col_skip(), locationText = col_skip(), 
                                    resultCount = col_skip(), cityName = col_skip(), 
                                    cancellationCategory = col_skip(), 
                                    cancellationPolicy = col_skip(), 
                                    string = col_skip(), highlightedResults.uid = col_skip(), 
                                    highlightedResults.accommodationId = col_skip(), 
                                    highlightedResults.totalPrice.currencyCode = col_skip(), 
                                    highlightedResults.shortText = col_skip(), 
                                    details.description = col_skip(), 
                                    details.address.first = col_skip(), 
                                    details.address.last = col_skip(), 
                                    details.address = col_skip(),
                                    details.name = col_skip(),
                                    details.stars = col_skip()
                   )) |>
  filter(scraped_at=="2024-12-10")


joined <- left_join(hotels, prices, by="accommodationId") |>
  distinct(accommodationId, highlightedResults.totalPrice.value, .keep_all = TRUE) |>
  mutate(details.stars=floor(details.stars)) |>
  filter(details.stars > 0)


df <- joined |>
  group_by(DEN_UTS, details.stars) |>
  mutate(avg_price=mean(highlightedResults.totalPrice.value)) |>
  ungroup() |>
  distinct(DEN_UTS, details.stars, avg_price) |>
  mutate(log_price = log(avg_price)) |>
  mutate(details.stars=factor(details.stars)) |>
  mutate(DEN_UTS=factor(DEN_UTS))

write_csv(df, "results/average_prices.csv")

levels(df$DEN_UTS)

contrasts(df$DEN_UTS)<-contr.sum(length(levels(df$DEN_UTS))) #MEDIA regione=100


cpd <- lm(log_price ~  DEN_UTS + details.stars, data = df)

summary(cpd)


coefs <- broom::tidy(cpd) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef <- data.frame(term = "DEN_UTS101", estimate = -1*sum(coefs$estimate))

# Append the row
all_coefs <- bind_rows(coefs, vt_coef) |>
  mutate(DEN_UTS=levels(df$DEN_UTS)) |>
  mutate(spi=exp(estimate)*100)



