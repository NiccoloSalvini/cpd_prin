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
library(factoextra)
library(rvest)
library(Rtsne)
library(gt)

prov_shape = st_read("data/ProvCM01012024_g_WGS84.shp")

# Spatial join con province per LISA analysis
hotels_prov <- st_join(prov_shape, hotels_final)


# TODO load hotel_final


hotels_final <- hotels_final %>%
  mutate(title = fct_relevel(title, "hotel 1*", "hotel 2*", "hotel 3*", "hotel 4*", "hotel 5*"))


# Distribuzione per stelle degli hotel ----
hotels_final %>%
  ggplot(aes(x = title)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribuzione Hotel per Categoria",
       x = "Categoria",
       y = "Numero di Hotel")

# Distribuzione per piattaforma ----
hotels_final %>%
  ggplot(aes(x = provider_code)) +
  geom_bar() +
  theme_minimal() +
  coord_flip() +
  labs(title = "Distribuzione Hotel per Piattaforma")



library(ggplot2)

# Boxplot of hotel prices by category (log scale for better readability)
ggplot(hotels_final, aes(x = title, y = price)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "red", outlier.alpha = 0.5) +  # Highlight outliers
  theme_minimal(base_size = 14) +  # Improve readability for publication
  scale_y_log10() +  # Log scale to handle price distribution
  labs(
    # title = "Hotel Price Distribution by Category",
    # subtitle = "Log-transformed price scale to highlight variations",
    x = NULL,
    y = "Log scale Price"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(face = "bold", size = 16)
  )

# Save the plot
ggsave("images/hotel_price_distribution.pdf", width = 10, height = 6, dpi = 300)


# Densità dei prezzi per principali città ----
library(ggplot2)
library(dplyr)

# Filter hotels in major cities and plot price distribution
p <- hotels_final %>%
  filter(cityName %in% c("Torino", "Milano", "Napoli")) %>%
  ggplot(aes(x = price, fill = cityName)) +
  geom_density(alpha = 0.5) +  # Semi-transparent for overlapping densities
  facet_wrap(~title) +
  scale_x_log10() +  # Log scale for better price distribution visualization
  theme_minimal(base_size = 14) +  # Improve readability
  scale_fill_manual(values = c("Torino" = "#E52b50", "Napoli" = "#377EB8", 
                              "Milano" = "#4DAF4A"))+
  labs(
    # title = "Hotel Price Distribution in Major Italian Cities",
    # subtitle = "Log-transformed price scale to highlight variations",
    x = "Price (euro) ",
    y = NULL,
    fill = "City"
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )

# Save the plot
ggsave("images/hotel_price_distribution_cities.pdf", plot = p, width = 10, height = 6, dpi = 300)


  # Scatter plot prezzo vs rating
# TODO:
ggplot(hotels_final, aes(x = ratingScore, y = price, color = title)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Relazione tra Prezzo e Rating",
       x = "Rating Score",
       y = "Prezzo (log scale)")


# summary table ----
# Create the summary table
summary_table <- hotels_final %>%
  group_by(title) %>%
  summarise(
    n_hotels = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_rating = mean(ratingScore, na.rm = TRUE),
    perc_breakfast = mean(breakfast, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Format and display the table with gt
summary_table %>%
  st_drop_geometry() %>% 
  gt() %>%
  tab_header(
    title = md("**Summary Statistics of Hotel Categories**"),
    subtitle = md("*Average price, ratings, and breakfast availability per category*")
  ) %>%
  fmt_number(
    columns = c(mean_price, median_price, mean_rating),
    decimals = 2
  ) %>%
  fmt_number(
    columns = perc_breakfast,
    decimals = 1
  ) %>%
  cols_label(
    title = "Hotel Category",
    n_hotels = "Number of Hotels",
    mean_price = "Avg. Price (€)",
    median_price = "Median Price (€)",
    mean_rating = "Avg. Rating",
    perc_breakfast = "% Breakfast Included"
  )


##########################################
##########################################
##########################################
##########################################
##########################################
##########################################



# grafici pià sofisticati
# GAM smoothing per relazione prezzo-rating con intervalli di confidenza ----
library(mgcv)

p2 <- ggplot(hotels_final, aes(x = ratingScore, y = price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = "red", fill = "red", alpha = 0.2) +
  facet_wrap(~title, scales = "free_x") +
  theme_minimal() +
  labs(# title = "Non-linear Price-Rating Relationship by Hotel Category",
       x = "Rating Score", y = "Price (euro)") +
  scale_y_continuous(trans = "log10")

ggsave("images/gam_price_rating.pdf", p2, width = 12, height = 8, dpi = 300)



# Cluster Analysis con visualizzazione: ---- 

# Preparazione dati per clustering
cluster_data <- hotels_final %>%
  st_drop_geometry() %>% 
  select(price, ratingScore, ratingCount) %>% 
  scale() %>%
  na.omit()

# K-means clustering
set.seed(123)
km <- kmeans(cluster_data, centers = 5)

# Visualizzazione
p3 <- fviz_cluster(km, data = cluster_data,
                   geom = "point",
                   ellipse.type = "convex",
                   palette = "Set2",
                   ggtheme = theme_minimal()) +
  labs(title = "K-means Clustering of Hotels",
       subtitle = "Based on Price, Rating Score and Review Count")

ggsave("images/cluster_analysis.png", p3, width = 10, height = 8, dpi = 300)




# t-SNE ---
mtc = read_html(x = "https://it.wikipedia.org/wiki/Turismo_in_Italia") %>% 
  html_table() %>% 
  .[[3]] %>% 
  pull(Comune)

hotels_italia = hotels_final %>% 
  st_drop_geometry() %>% 
  filter(cityName%in% mtc) %>% 
  select(cityName, title, price) %>%
  group_by(cityName, title) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

set.seed(123)

tsne_results <- Rtsne(scale(hotels_italia$avg_price), perplexity =  3, theta = 0.2, check_duplicates = FALSE)

# Aggiungiamo le coordinate t-SNE al dataset
hotels_italia$tsne_x <- tsne_results$Y[,1]
hotels_italia$tsne_y <- tsne_results$Y[,2]

# Create the scatter plot with ggrepel for better labeling
price_similarity <- ggplot(hotels_italia, aes(x = tsne_x, y = tsne_y, color = title)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text_repel(aes(label = cityName), size = 4, max.overlaps = 15) + 
  theme_minimal() +
  labs(
    #title = "Hotel Price Similarity in Italy",
    #subtitle = "t-SNE visualization of hotel price clusters in major tourist cities",
    x = "Dimension 1 (t-SNE)", 
    y = "Dimension 2 (t-SNE)",
    color = "Hotel Category"
  )

# Save the plot as an image
ggsave("images/hotel_price_similarity.pdf", plot = p, width = 10, height = 6, dpi = 300)






