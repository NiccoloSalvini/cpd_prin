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

cpd_tre_stelle = lm(log(mean_price) ~ DEN_UTS, data = prices_3stars) #

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
## questa versione definitiva
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
  select(term, DEN_UTS, estimate, std.error, statistic, p.value, spi) %>%
  arrange(desc(spi))  # ordiniamo per SPI decrescente

# Riaggiungiamo la geometria unendo con il dataframe originale delle province
spi_map_data <- prov_shape %>%
  left_join(all_coefs_tre_stelle , by = "DEN_UTS")


spi_map =tm_shape(spi_map_data %>%
           mutate(spi = ifelse(is.na(spi), 100, spi))) +  # Sostituisce i NA con 100
  tm_fill("spi", 
          style = "fixed",
          breaks = sort(c(100, quantile(spi_map_data$spi, probs = seq(0.2, 1, 0.2), na.rm = TRUE))),  
          palette = "Reds",  # Palette bianco → rosso
          title = "SPI (province mean = 100)") +
  tm_borders(alpha = 0.4) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE,
            legend.title.size = 0.6,
            legend.text.size = 0.4) +
  tm_text("DEN_UTS", size = 0.4)  # opzionale


tmap_save(
  tm = spi_map,   # Salva l'ultima mappa generata
  filename = "images/spi_map.pdf", 
  width = 2000, height = 1500, dpi = 300
)


## copertura ----
# Prepara i dati: conta hotel per provincia e categoria
# Calcola prima il totale degli hotel per provincia
total_by_prov <- hotels_prov %>%
  filter(title %in% c("hotel 1*", "hotel 2*", "hotel 3*", "hotel 4*", "hotel 5*")) %>%
  group_by(DEN_UTS) %>%
  summarise(total = n(), .groups = 'drop')

# Calcola le percentuali per categoria
hotels_density <- hotels_prov %>%
  filter(title %in% c("hotel 1*", "hotel 2*", "hotel 3*", "hotel 4*", "hotel 5*")) %>%
  group_by(DEN_UTS, title) %>%
  summarise(count = n(), .groups = 'drop') %>%
  st_join(total_by_prov) %>%
  mutate(percentage = (count/total)*100)

# Unisci con lo shape delle province mantenendo tutte le province
prov_density <- prov_shape %>%
  st_join(hotels_density) %>%
  mutate(percentage = replace_na(percentage, 0))  # Sostituisci NA con 0


tm_shape(prov_density) +
  tm_facets(by = "title", ncol = 3) +
  tm_fill(col = "percentage", 
          palette = "Reds",  # Palette da bianco a rosso
          title = "% degli hotel",
          style = "jenks",
          legend.reverse = TRUE,
          alpha = 0.7,
          breaks = c(0, 5, 10, 15, 20, 25, 30)) +  # Puoi modificare questi break
  tm_borders(col = "gray30", lwd = 0.5) +
  tm_layout(panel.labels = c("Hotel 1*", "Hotel 2*", "Hotel 3*", "Hotel 4*", "Hotel 5*"),
            legend.position = c("right", "bottom"),
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            panel.label.size = 1,
            main.title = "Percentuale degli hotel per categoria e provincia",
            main.title.size = 1,
            frame = FALSE)



# più veloce forse
library(dplyr)
library(tidyr)
library(sf)
library(tmap)

# 1. Ottimizzazione del calcolo delle percentuali
hotels_density <- hotels_prov %>%
  filter(title %in% c("hotel 1*", "hotel 2*", "hotel 3*", "hotel 4*", "hotel 5*")) %>%
  group_by(DEN_UTS, title) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(DEN_UTS) %>%
  mutate(total = sum(count), percentage = (count / total) * 100)

# 2. Evitare `st_join()`, se non necessario
prov_density <- prov_shape %>%
  st_join(hotels_density, by = "DEN_UTS") %>%
  mutate(percentage = replace_na(percentage, 0))

# 3. Generazione della mappa con tmap v4
prov_density_map = tm_shape(prov_density) +
  tm_facets(by = "title", ncol = 3) +
  tm_polygons(
    fill = "percentage",  # Cambiato da "col" a "fill"
    fill.scale = tm_scale_intervals(values = c(0, 5, 10, 15, 20, 25, 30)),  # Sostituisce "style = 'jenks'"
    fill.palette = "brewer.reds",  # Aggiornato da "Reds" a "brewer.reds"
    fill.alpha = 0.7,  # Cambiato da "alpha" a "fill.alpha"
    col = "gray30",  # Confini delle province
    lwd = 0.5
  ) +
  tm_legend(title = "% degli hotel", reverse = TRUE) +  # Sostituisce "legend.reverse"
  tm_layout(
    panel.labels = c("Hotel 1*", "Hotel 2*", "Hotel 3*", "Hotel 4*", "Hotel 5*"),
    legend.position = c("right", "bottom"),
    legend.title.size = 0.8,
    legend.text.size = 0.6,
    panel.label.size = 1
  ) +
  tm_title("Percentuale degli hotel per categoria e provincia")


tmap_save(
  tm = prov_density_map,   # Salva l'ultima mappa generata
  filename = "images/prov_density_map.pdf", 
  width = 2000, height = 1500, dpi = 300
)



library(dplyr)
library(tidyr)
library(sf)
library(tmap)

# 1. Calcolo delle percentuali
hotels_density <- hotels_prov %>%
  filter(title %in% c("hotel 1*", "hotel 2*", "hotel 3*", "hotel 4*", "hotel 5*")) %>%
  group_by(DEN_UTS, title) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(DEN_UTS) %>%
  mutate(total = sum(count), 
         percentage = (count / total) * 100)

# 2. Join con le province
prov_density <- prov_shape %>%
  st_join(hotels_density, by = "DEN_UTS") %>%
  mutate(percentage = replace_na(percentage, 0))

# 3. Creazione mappa con sintassi tmap v4
prov_density_map <- tm_shape(prov_density) +
  tm_facets(by = "title", ncol = 3) +
  tm_fill(
    col = "percentage",
    palette = "Reds",
    breaks = c(0, 5, 10, 15, 20, 25, 30),
    title = "% degli hotel"
  ) +
  tm_borders(col = "gray30", lwd = 0.5) +
  tm_layout(
    panel.labels = c("Hotel 1*", "Hotel 2*", "Hotel 3*", "Hotel 4*", "Hotel 5*"),
    legend.position = c("right", "bottom"),
    legend.title.size = 0.8,
    legend.text.size = 0.6,
    panel.label.size = 1,
    main.title = "Percentuale degli hotel per categoria e provincia",
    main.title.size = 1,
    frame = FALSE
  )

# 4. Salvataggio
tmap_save(
  tm = prov_density_map,
  filename = "images/prov_density_map.pdf", 
  width = 10, 
  height = 7.5, 
  dpi = 300
)
