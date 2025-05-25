# Load required packages
pacman::p_load(
  hdf5r, tidyverse, hdf5r.Extra, fs, sf, mapview, tidygraph, igraph
)

# Set base directory and list all .omx files
base_dir <- "C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/"
omx_paths <- dir_ls(
  path = base_dir,
  recurse = TRUE,
  type = "file",
  glob = "*.omx"
)
file.h5 <- H5File$new(omx_paths[58], mode = "r")

# Read OD matrices and ensure non-negative values
q0 <- file.h5[["data/total_demand"]]$read() %>% pmax(0)
q1 <- file.h5[["data/total_demand0"]]$read() %>% pmax(0)
q2 <- file.h5[["data/total_demand1"]]$read() %>% pmax(0)
q3 <- file.h5[["data/total_demand2"]]$read() %>% pmax(0)

# Read shapefile for zones
shp <- st_read(
  "C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/resources/geo/NewTazArzi_848.shp",
  crs = 4326
) %>% arrange(TAZ)

# Function: Summarize flows by clusters
cluster_flows_summary <- function(od_matrix, k = 2, method = "average", epsilon = 1e-6) {
  # Compute symmetric distance matrix
  dist_matrix <- 1 / (od_matrix + epsilon)
  dist_matrix <- (dist_matrix + t(dist_matrix)) / 2
  dist_object <- as.dist(dist_matrix)
  
  # Hierarchical clustering
  hc <- hclust(dist_object, method = method)
  clusters <- cutree(hc, k)
  
  # Zone-cluster mapping
  zone_names <- 1:nrow(od_matrix)
  cluster_df <- data.frame(zone = zone_names, cluster = clusters)
  
  # Within-cluster flows
  within_cluster_sums <- sapply(1:k, function(cl) {
    members <- cluster_df$zone[cluster_df$cluster == cl]
    sum(od_matrix[members, members])
  })
  names(within_cluster_sums) <- paste0("Cluster_", 1:k)
  
  # Between-cluster flows
  between_cluster_flows <- matrix(0, nrow = k, ncol = k,
                                  dimnames = list(paste0("C", 1:k), paste0("C", 1:k)))
  for (i in 1:k) {
    for (j in 1:k) {
      zones_i <- cluster_df$zone[cluster_df$cluster == i]
      zones_j <- cluster_df$zone[cluster_df$cluster == j]
      between_cluster_flows[i, j] <- sum(od_matrix[zones_i, zones_j])
    }
  }
  
  list(
    clusters = clusters,
    within_cluster_sums = within_cluster_sums,
    between_cluster_flows = between_cluster_flows,
    hc = hc
  )
}

# Function: Calculate directed modularity for a flow matrix
calculate_modularity_directed <- function(flow_matrix) {
  stopifnot(nrow(flow_matrix) == ncol(flow_matrix))
  m <- sum(flow_matrix)
  out_degrees <- rowSums(flow_matrix)
  in_degrees <- colSums(flow_matrix)
  
  Q <- 0
  for (i in seq_len(nrow(flow_matrix))) {
    for (j in seq_len(ncol(flow_matrix))) {
      if (i == j) {
        expected <- (out_degrees[i] * in_degrees[j]) / m
        Q <- Q + (flow_matrix[i, j] - expected)
      }
    }
  }
  Q / m
}

# Explore modularity for different cluster counts
modularity_results <- map_df(2:100, ~{
  result <- cluster_flows_summary(q0, .x)
  modularity <- calculate_modularity_directed(result$between_cluster_flows)
  data.frame(
    n = .x,
    mod1 = modularity,
    idx = modularity / .x
  ) %>%
    mutate(l = mod1 - lag(mod1))
})

# Plot modularity vs. number of clusters
modularity_results %>%
  ggplot() +
  geom_line(aes(x = n, y = mod1), color = "black") +
  geom_line(aes(x = n, y = idx), color = "red")

# Example: Cluster summary for k = 11
result_11 <- cluster_flows_summary(q0, 11)
modularity_11 <- calculate_modularity_directed(result_11$between_cluster_flows)

# Map clusters and within-cluster sums
shp %>%
  mutate(clus = result_11$clusters) %>%
  group_by(clus) %>%
  summarise() %>%
  mutate(within_cluster_sums = result_11$within_cluster_sums) %>%
  mapview(zcol = "within_cluster_sums")

# Louvain clustering using igraph/tidygraph
res1 <- q0 %>%
  as_tibble() %>%
  rownames_to_column("from") %>%
  gather(to, weight, -from) %>%
  mutate(to = str_remove(to, "V")) %>%
  filter(from != to) %>%
  as_tbl_graph(directed = FALSE)

# Repeat Louvain clustering with different seeds and compute modularity
louvain_modularity <- map_dbl(1:100, ~{
  set.seed(.x)
  res2 <- res1 %>%
    mutate(grp = group_louvain(weights = weight)) %>%
    pull(grp)
  modularity(res1, res2, res1 %E>% as_tibble() %>% pull(weight))
}) %>% as_tibble()

# Single Louvain clustering and mapping
set.seed(1)
res2 <- res1 %>%
  mutate(grp = group_louvain(weights = weight)) %>%
  pull(grp)
modularity_single <- modularity(res1, res2, res1 %E>% as_tibble() %>% pull(weight))

shp %>%
  mutate(clus = res2) %>%
  group_by(clus) %>%
  summarise() %>%
  mapview()