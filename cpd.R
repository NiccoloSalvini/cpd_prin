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

# CPD alla "Luigi" ----
cpd_tutte_stelle <- lm(log_price ~  DEN_UTS + details.stars, data = df)
 
coefs_cpd_tutte_stelle <- broom::tidy(cpd_tutte_stelle) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef_tutte_stelle <- data.frame(term = "DEN_UTS101", estimate = -1*sum(coefs_cpd_tutte_stelle$estimate))


all_coefs_tutte_stelle <- bind_rows(coefs_cpd_tutte_stelle, vt_coef_tutte_stelle) |>
  mutate(DEN_UTS=levels(df$DEN_UTS)) |>
  mutate(spi=exp(estimate)*100)



# CPD Tiziana ----
cpd_tre_stelle = lm(log_price ~  DEN_UTS, data = df %>% filter(details.stars ==3))

coefs_cpd_tre_stelle <- broom::tidy(cpd_tre_stelle) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef_tre_stelle <- data.frame(term = "DEN_UTS101", estimate = -1*sum(coefs_cpd_tre_stelle$estimate))

all_coefs_tre_stelle <- bind_rows(coefs_cpd_tre_stelle, vt_coef_tre_stelle) |>
  mutate(DEN_UTS  = str_remove(term,"DEN_UTS")) %>% 
  mutate(spi=exp(estimate)*100)


# CPD mio ----
cpd_due_classi_stelle  = lm(log_price ~  DEN_UTS + bin_stelle, data = df %>% 
                              mutate(bin_stelle = if_else(details.stars %in% c(1:3), "A", "B")))

coefs_cpd_due_classi_stelle <- broom::tidy(cpd_due_classi_stelle) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef_due_classi_stelle <- data.frame(term = "DEN_UTS101", estimate = -1*sum(coefs_cpd_due_classi_stelle$estimate))

# Append the row
all_coefs_due_classi_stelle <- bind_rows(coefs_cpd_due_classi_stelle, vt_coef_due_classi_stelle) |>
  mutate(DEN_UTS=levels(df$DEN_UTS)) |>
  mutate(spi=exp(estimate)*100)


### NICCCOLò
prov_shape = st_read("data/ProvCM01012024_g_WGS84.shp")
hotels_prov <- st_join(prov_shape, hotels_final)

# TODO: Qui imputazione alla luigi
# TODO: prendi le osservazioni singole (CPD)
# non serve per ora
prices_by_prov_stars <- hotels_prov %>%
  group_by(DEN_UTS, title) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(mean))

prices_by_prov <- hotels_prov %>%
  group_by(DEN_UTS) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  filter(!is.na(mean_price)) 

# con nuovo dataset
provinces_3stars <- prices_by_prov_stars %>% 
  filter(title == "hotel 3*") %>% 
  pull(DEN_UTS) %>% 
  unique()

prices_3stars <- prices_by_prov_stars %>% 
  filter(title == "hotel 3*") %>%
  mutate(
    DEN_UTS = factor(DEN_UTS)  # Rifattorizziamo con solo i livelli presenti
  )

contrasts(prices_3stars$DEN_UTS) <- contr.sum(length(levels(prices_3stars$DEN_UTS)))

cpd_tre_stelle = lm(log_price ~ DEN_UTS, data = prices_3stars) #

coefs_cpd_tre_stelle <- broom::tidy(cpd_tre_stelle) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef_tre_stelle <- data.frame(
  term = paste0("DEN_UTS", length(levels(prices_3stars$DEN_UTS))), 
  estimate = -1*sum(coefs_cpd_tre_stelle$estimate)
)

all_coefs_tre_stelle <- bind_rows(coefs_cpd_tre_stelle, vt_coef_tre_stelle) %>%
  mutate(
    DEN_UTS = str_remove(term, "DEN_UTS"),
    spi = exp(estimate)*100
  ) 


# Creiamo un mapping tra numeri e nomi province
province_mapping <- prices_3stars %>%
  select(DEN_UTS) %>%
  distinct() %>%
  mutate(
    province_num = as.numeric(factor(DEN_UTS))
  )

# Applichiamo il mapping ai coefficienti
all_coefs_tre_stelle <- bind_rows(coefs_cpd_tre_stelle, vt_coef_tre_stelle) %>%
  mutate(
    province_num = as.numeric(str_remove(term, "DEN_UTS")),
    DEN_UTS = province_mapping$DEN_UTS[match(province_num, province_mapping$province_num)],
    spi = exp(estimate)*100
  ) %>%
  select(term, estimate, DEN_UTS, spi) %>%
  arrange(desc(spi))  # ordiniamo per SPI decrescente

# Riaggiungiamo la geometria unendo con il dataframe originale delle province
spi_map_data <- prov_shape %>%
  left_join(all_coefs_tre_stelle, by = "DEN_UTS")


# Versione che evidenzia le province mancanti
# TODO: gradiente con 100 al bianco (rosso cresce, blu si spegne)
tm_shape(spi_map_data) +
  tm_fill("spi", 
          style = "quantile",
          n = 5,
          palette = "viridis",
          title = "SPI (Media Regionale = 100)",
          missing = "grey80") +  # colore per NA
  tm_borders(alpha = 0.4) +
  tm_layout(
    title = "Spatial Price Index - Hotel 3 stelle",
    title.position = c("center", "top"),
    legend.position = c("right", "bottom"),
    frame = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.6
  ) +
  tm_text("DEN_UTS", size = 0.4)  # opzionale: aggiunge i nomi delle province



## NICCOLò versione con tuttle le obs:
hotels_prov <- st_join(hotels_final, prov_shape) %>% 
  filter(title =="hotel 3*") %>% 
  mutate(DEN_UTS  = as_factor(DEN_UTS)) 
  

contrasts(hotels_prov$DEN_UTS) <- contr.sum(length(levels(hotels_prov$DEN_UTS)))


# TODO:
# risistemare offerta camera
cpd_tre_stelle = lm(log(price) ~ DEN_UTS + offer + ratingScore + ratingCount, data = hotels_prov)

coefs_cpd_tre_stelle <- broom::tidy(cpd_tre_stelle) %>%
  filter(str_starts(term, "DEN_UTS"))

vt_coef_tre_stelle <- data.frame(
  term = paste0("DEN_UTS", length(levels(hotels_prov$DEN_UTS))), 
  estimate = -1*sum(coefs_cpd_tre_stelle$estimate)
)

all_coefs_tre_stelle <- bind_rows(coefs_cpd_tre_stelle, vt_coef_tre_stelle) %>%
  mutate(
    DEN_UTS = str_remove(term, "DEN_UTS"),
    spi = exp(estimate)*100
  ) 


# Creiamo un mapping tra numeri e nomi province
province_mapping <- hotels_prov %>%
  select(DEN_UTS) %>%
  distinct() %>%
  mutate(
    province_num = as.numeric(factor(DEN_UTS))
  )

# Applichiamo il mapping ai coefficienti
all_coefs_tre_stelle <- bind_rows(coefs_cpd_tre_stelle, vt_coef_tre_stelle) %>%
  mutate(
    province_num = as.numeric(str_remove(term, "DEN_UTS")),
    DEN_UTS = province_mapping$DEN_UTS[match(province_num, province_mapping$province_num)],
    spi = exp(estimate)*100
  ) %>%
  select(term, estimate, DEN_UTS, spi) %>%
  arrange(desc(spi))  # ordiniamo per SPI decrescente

# Riaggiungiamo la geometria unendo con il dataframe originale delle province
spi_map_data <- prov_shape %>%
  left_join(all_coefs_tre_stelle, by = "DEN_UTS")


tm_shape(spi_map_data) +
  tm_fill("spi", 
          style = "quantile",
          n = 5,
          palette = "viridis",
          title = "SPI (Media Regionale = 100)",
          missing = "grey80") +  # colore per NA
  tm_borders(alpha = 0.4) +
  tm_layout(
    title = "Spatial Price Index - Hotel 3 stelle",
    title.position = c("center", "top"),
    legend.position = c("right", "bottom"),
    frame = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.6
  ) +
  tm_text("DEN_UTS", size = 0.4)  # opzionale: aggiunge i nomi delle province

