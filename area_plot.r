pacman::p_load(tidyverse,sf)
# weekday <- read_csv("C:/Users/idshk/OneDrive/idos_shit/celular1819_v1/v_1/AvgDayHourlyTrips201819_1270_weekday_v1.csv")
st_layers("layers.gpkg")
nafot <- st_read("layers.gpkg",layer = "nafot") %>% mutate(are = st_area(geom))
mehozot <- st_read("layers.gpkg",layer = "mehozot") %>% st_zm() %>% mutate(are = st_area(geom))
cell_33 <- st_read("layers.gpkg",layer = "cell_33")%>% mutate(are = st_area(geom))
cell_15 <- st_read("layers.gpkg",layer = "cell_15")%>% mutate(are = st_area(geom))
minhal_hatichnun <- st_read("layers.gpkg",layer = "minhal_hatichnun")%>% mutate(are = st_area(geom))
layers <- list(nafot= nafot, mehozot= mehozot, cell_33= cell_33, cell_15= cell_15, minhal_hatichnun= minhal_hatichnun)
percent_area_matrix <- function(layer1, layer2) {
  # Ensure both layers use the same CRS
  if (sf::st_crs(layer1) != sf::st_crs(layer2)) {
    stop("CRS of both layers must match.")
  }
  
  # Calculate area of each feature in layer1
  area1 <- sf::st_area(layer1)
  
  # Intersect features
  inter <- sf::st_intersection(
    layer1 %>% dplyr::mutate(row_id = dplyr::row_number()),
    layer2 %>% dplyr::mutate(col_id = dplyr::row_number())
  )
  
  # Calculate area of intersections
  inter$inter_area <- sf::st_area(inter)
  
  # Prepare empty matrix
  mat <- matrix(0, nrow = nrow(layer1), ncol = nrow(layer2))
  rownames(mat) <- seq_len(nrow(layer1))
  colnames(mat) <- seq_len(nrow(layer2))
  
  # Fill matrix with percent areas
  for (i in seq_len(nrow(inter))) {
    r <- inter$row_id[i]
    c <- inter$col_id[i]
    mat[r, c] <- mat[r, c] + as.numeric(inter$inter_area[i]) / as.numeric(area1[r]) * 100
  }
  
  # Ensure each row sums to 100 (or 0 if no overlap)
  mat
}
res1 <- imap(layers, function(layer1, name1) {
    imap(layers, function(layer2, name2) {
        percent_area_matrix(layer1, layer2)
    })
})
# Flatten res1 into a data.frame with layer names, feature indices, and percent values
res1_df <- purrr::imap_dfr(res1, function(layer1_list, name1) {
    purrr::imap_dfr(layer1_list, function(mat, name2) {
        if (length(mat) == 0) return(NULL)
        df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
        df <- df %>%
            mutate(
                layer1 = name1,
                layer2 = name2,
                feature1 = as.integer(as.character(Var1)),
                feature2 = as.integer(as.character(Var2)),
                percent = as.numeric(Freq)
            ) %>%
            select(layer1, layer2, feature1, feature2, percent)
        df
    })
})
res1_df %>% filter(percent > 10,percent < 85 ) %>% 
  ggplot(aes(x= feature1, y = feature2, fill = percent)) +
  geom_raster() +
#   scale_fill_viridis_c() +
  labs(x = "Layer 2 Feature", y = "Layer 1 Feature", fill = "Percent Area") +
  facet_grid(layer2~ layer1 )
ggplot() +geom_sf(data = minhal_hatichnun[1:10,]) 
