library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(forcats)
library(emoji)
library(sf)
library(dplyr)
library(sf)
library(dbscan)
library(stringdist)

istat_all_hotels <- read_excel("data/istat_all_hotels.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "skip", "numeric"))

df <- read_csv("data/cozycozy-1g1n1m-tc.csv") %>% select(
  uid, 
  title,
  subTitle,
  locationText,
  provider_code = highlightedResults.providerName,
  price = highlightedResults.totalPrice.value,
  offer = highlightedResults.text,
  offer_short = highlightedResults.shortText,
  bedroom_count = highlightedResults.bedRoomCount,
  cityName,
  ratingCount,
  ratingScore,
  coordinates.latitude,
  coordinates.longitude,
  cancellationCategory,
  cancellationPolicy,
  scraped_at
)%>% mutate(
    title = as_factor(title),
    provider_code = as_factor(provider_code),
    offer = str_to_lower(offer),
    offer = as_factor(offer),
    offer_short = as_factor(offer_short),
    bedroom_count = as_factor(bedroom_count),
    cancellationCategory = as_factor(cancellationCategory),
    cancellationPolicy = as_factor(cancellationPolicy),
    breakfast = emoji_detect(offer),
  ) 
  
allowed_price_star = c("hotel 3*", "	hotel 2*", "	hotel 1*")

hotels <- df |>
  filter(scraped_at=="2024-12-10") |>
  filter(!is.na(subTitle)) |>
  filter(str_detect(title, "hotel")) %>% 
  filter(
    !price == 1000000.00,
    !price > 25000, # visual inspection, c'è lo scalino
    !(price > 3000 & title %in% allowed_price_star),
    !(price >5000 & str_detect(offer, "suite"))
  ) %>% 
  # rimuovi gli hotel che non sono hotel perchè non hanno stelle (non ho idee su come fare)
  filter(str_detect(title, "hotel ")) %>%  # Opinabile
  filter(title != "hotel 3.5*") %>% 
  select(-uid, -locationText)
  # per come è fatto il dataset ci sono dei doppioni le cui cause sono
  # 1) errore su una delle colonne (cambiato indirizzo e ripostato l'offerta) -> questo caso va corretto
  # 2) prezzo diverso -> questo caso va mediato


# ✅ 1. Conversione in sf
hotels_sf <- st_as_sf(hotels, 
                      coords = c("coordinates.longitude", "coordinates.latitude"), 
                      crs = 4326) %>%
  st_transform(32632)  # UTM per calcoli più precisi

# ✅ 2. Clusterizzazione spaziale (identifica hotel vicini entro 50m)
threshold <- 50  # Distanza massima in metri
coords <- st_coordinates(hotels_sf)

clusters <- dbscan::dbscan(coords, eps = threshold, minPts = 1)$cluster
hotels_sf$cluster <- clusters  # Assegna cluster agli hotel

# ✅ 3. Classificazione iniziale dei duplicati
hotels_sf <- hotels_sf %>%
  group_by(cluster, title, subTitle) %>%
  mutate(duplicate_category = case_when(
    n() > 1 & n_distinct(price) > 1 & n_distinct(offer) == 1 ~ "stesso hotel, stesso tipo di offerta, prezzi diversi",
    n() > 1 & n_distinct(offer) > 1 ~ "stesso hotel, offerte diverse",
    n() > 1 ~ "duplicati esatti",
    TRUE ~ "unico"
  )) %>%
  ungroup()

# ✅ 4. Fuzzy Matching solo tra titoli nello stesso cluster
find_similar_hotels <- function(df, threshold = 0.2) {
  df <- df %>%
    group_by(cluster) %>%
    mutate(
      min_similarity = map_dbl(title, function(x) {
        other_titles <- title[title != x]  # Prende solo titoli diversi
        if (length(other_titles) == 0) return(1)  # Se non ci sono altri titoli nel cluster, restituisce 1
        min(stringdist(x, other_titles, method = "jw"))  # Distanza minima rispetto agli altri
      })
    ) %>%
    mutate(duplicate_category = case_when(
      min_similarity < threshold & duplicate_category == "unico" ~ "stesso hotel, nomi leggermente diversi",
      TRUE ~ duplicate_category
    )) %>%
    ungroup()
  
  return(df)
}

hotels_sf <- find_similar_hotels(hotels_sf)

# ✅ 5. Risoluzione duplicati con stesso tipo di offerta (scegli prezzo più basso)
# TODO: sentire tiziana
hotels_cleaned <- hotels_sf %>%
  filter(duplicate_category %in% c("stesso hotel, stesso tipo di offerta, prezzi diversi", "stesso hotel, nomi leggermente diversi")) %>%
  group_by(cluster, title, subTitle, offer) %>%
  filter(price == min(price)) %>%
  arrange(desc(ratingScore)) %>%  # Ordina per rating più alto
  slice(1) %>%  # Tiene solo il primo
  ungroup()

# ✅ 6. Mantieni offerte diverse dello stesso hotel
hotels_offers <- hotels_sf %>%
  filter(duplicate_category == "stesso hotel, offerte diverse")

# ✅ 7. Deduplicazione di record completamente identici
hotels_dedup <- hotels_sf %>%
  filter(duplicate_category == "duplicati esatti") %>%
  distinct(cluster, title, subTitle, offer, price, .keep_all = TRUE)  

# ✅ 8. Mantieni i record unici
hotels_final <- hotels_sf %>%
  filter(duplicate_category == "unico")

# ✅ 9. Ultima deduplicazione: elimina righe perfettamente identiche
hotels_final <- bind_rows(hotels_cleaned, hotels_offers, hotels_dedup, hotels_final) %>%
  distinct(cluster, title, subTitle, offer, price, .keep_all = TRUE)  # Rimuove eventuali duplicati rimanenti

# ✅ 10. Visualizzazione finale
hotels_final %>% View("finale")

write_csv(hotels_final, "data/hotels_raw_final.csv")
