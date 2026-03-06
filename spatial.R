library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(dplyr)
library(sf)
library(rgeoda)
library(purrr)
library(RColorBrewer)
library(ggrepel)

set.seed(42)  # Per riproducibilità

# Applica jitter solo agli hotel con "stesso hotel, offerte diverse"
hotels_jittered <- hotels_final %>%
  filter(duplicate_category == "stesso hotel, offerte diverse") %>%
  st_jitter(amount = 0.0001)  # Piccolo jitter (~10m)

# Mantieni inalterate le altre osservazioni
hotels_no_jitter <- hotels_final %>%
  filter(duplicate_category != "stesso hotel, offerte diverse")

# Ricombina i dataset
hotels_final <- bind_rows(hotels_no_jitter, hotels_jittered)

# Ci sono 7 duplicati difficili da gesire li droppo 
# duplicati <- duplicated(st_coordinates(hotels_final))
# hotels_duplicated <- hotels_final[duplicati, ] %>% View("duplicati")

hotels_final <- hotels_final[!duplicati, ] 

# Calcola matrice di vicinanza con k=5
coords_jittered <- st_coordinates(hotels_final)

# Moran scatterplot degli hotel
hotels_clean <- hotels_final %>%
  filter(!is.na(price)) %>%
  st_transform(4326)  # assicuriamoci che sia in WGS84

# Creiamo la matrice dei pesi spaziali per i punti
# coords <- st_coordinates(coords_jittered)
knn <- knearneigh(coords_jittered, k=5)
nb <- knn2nb(knn)
lw <- nb2listw(nb, style="W")

# Standardizziamo i prezzi
z.prices <- scale(hotels_clean$price)

# Calcoliamo il lag spaziale
lag.prices <- lag.listw(lw, z.prices)

# Calcoliamo il Moran's I globale
moran_i <- moran.test(hotels_clean$price, lw)

# Creiamo un dataframe per ggplot
moran_df <- data.frame(
  z_price = z.prices,
  lag_price = lag.prices,
  hotel_title = paste(hotels_clean$subTitle, hotels_clean$title, sep = ", ")
)

# Creiamo il plot
moran_plot = ggplot(moran_df, aes(x = z_price, y = lag_price)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(# title = "Moran Scatterplot - Individual Hotels",
       subtitle = paste("Moran's I =", round(moran_i$estimate[1], 3),
                        "\np-value =", round(moran_i$p.value, 3)),
       x = "Standardized Price",
       y = "Spatially Lagged Standardized Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text_repel(
    data = subset(moran_df, abs(z_price) > 3 | abs(lag_price) > 3),
    aes(label = hotel_title),
    size = 2.5,
    max.overlaps = 10
  )


# Save the plot as an image
ggsave("images/moran_plot.pdf", plot = moran_plot, width = 10, height = 6, dpi = 300)





# Moran scatterplot per province ----
prov_shape = st_read("data/ProvCM01012024_g_WGS84.shp")

# Spatial join con province per LISA analysis
hotels_prov <- st_join(prov_shape, hotels_final)

# Calcola prezzi medi per provincia e per stelle
# TODO: Qui imputazione alla luigi
prices_by_prov_stars <- hotels_prov %>%
  group_by(DEN_UTS, title) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(mean)) # rimuovi hotels senza stelle

prices_by_prov <- hotels_prov %>%
  group_by(DEN_UTS) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  filter(!is.na(mean_price)) 

z.prices <- scale(prices_by_prov$mean_price)

nb <- poly2nb(prices_by_prov, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
li <- localmoran(prices_by_prov$mean_price, lw)

lag.prices <- lag.listw(lw, z.prices)

moran_i <- moran.test(prices_by_prov$mean_price, lw)

# Creiamo un dataframe per ggplot
moran_df <- data.frame(
  z_price = z.prices,
  lag_price = lag.prices,
  province = prices_by_prov$DEN_UTS
)

# Creiamo il plot
ggplot(moran_df, aes(x = z_price, y = lag_price)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Moran Scatterplot - Hotel Prices",
       subtitle = paste("Moran's I =", round(moran_i$estimate[1], 3),
                        "\np-value =", round(moran_i$p.value, 3)),
       x = "Standardized Price",
       y = "Spatially Lagged Standardized Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text_repel(
    data = subset(moran_df, abs(z_price) > 2 | abs(lag_price) > 2),
    aes(label = province),
    size = 3,
    max.overlaps = 15
  )


# LISA plot ----
match_palette <- function(patterns, classifications, colors){
  classes_present <- base::unique(patterns)
  mat <- matrix(c(classifications,colors), ncol = 2)
  logi <- classifications %in% classes_present
  pre_col <- matrix(mat[logi], ncol = 2)
  pal <- pre_col[,2]
  return(pal)
}

lisa_map <- function(df, lisa, alpha = .05) {
  clusters <- lisa_clusters(lisa,cutoff = alpha)
  labels <- lisa_labels(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]
  
  pal <- match_palette(lisa_patterns,labels,colors)
  labels <- labels[labels %in% lisa_patterns]
  
  df["lisa_clusters"] <- clusters
  tm_shape(df) +
    tm_fill("lisa_clusters",labels = labels, palette = pal,style = "cat")+
    tm_layout(
      frame = FALSE,
      legend.title.size = 0.8,
      legend.text.size = 0.6,
      title.position = c("center", "top"),
      legend.position = c("left", "bottom"),
      )
}

significance_map <- function(df, lisa, permutations = 999, alpha = .05) {
  pvalue <- lisa_pvalues(lisa)
  target_p <- 1 / (1 + permutations)
  potential_brks <- c(.00001, .0001, .001, .01)
  brks <- potential_brks[which(potential_brks > target_p & potential_brks < alpha)]
  brks2 <- c(target_p, brks, alpha)
  labels <- c(as.character(brks2), "Not Significant")
  brks3 <- c(0, brks2, 1)
  
  cuts <- cut(pvalue, breaks = brks3,labels = labels)
  df["sig"] <- cuts
  
  pal <- rev(brewer.pal(length(labels), "Greens"))
  pal[length(pal)] <- "#D3D3D3"
  
  tm_shape(df) +
    tm_fill("sig", palette = pal)
}


w <- queen_weights(prices_by_prov, order = 1, include_lower_order = FALSE)
lisa <- local_moran(w, prices_by_prov['mean_price'], permutations = 999)
lisa = lisa_map(prices_by_prov, lisa)

tmap_save(
  tm = lisa,   # Salva l'ultima mappa generata
  filename = "images/lisa_map.pdf", 
  width = 2000, height = 1500, dpi = 300
)

significance_map(prices_by_prov, lisa) 






