pacman::p_load(hdf5r,tidyverse,hdf5r.Extra,fs,sf,mapview,tidygraph,igraph)
# file.h5 <- H5File$new("C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/2020/Matrices/AM/NL_SUM.omx", mode = "r")
# file.h5$group_info_by_name("data")
# q <- file.h5$open("lookup")
# file.h5[["data/BusPR"]]$read()
# df <- data.frame(Origins = file.h5[["lookup"]][["Origins"]]$read(),
#                  Destinations  = file.h5[["lookup"]][["Destinations"]]$read(),
#                  ID = file.h5[["lookup"]][["ID"]]$read())
# file.h5[["data"]][["Truck"]]$read()
# h5ReadDataset(file.h5[["data"]][["Truck"]])
# arr <- hdf5r.Extra::h5Read(file.h5,"data") 
# array(c(1:16),c(2,2,2,2),list(letters[1:2],letters[3:4],letters[5:6],letters[7:8]))



# set your base directory
base_dir <- "C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/"

# list all .omx files recursively
omx_paths <- dir_ls(
  path    = base_dir,
  recurse = TRUE,
  type    = "file",
  glob    = "*.omx"
) 
file.h5 <- H5File$new(omx_paths[58], mode = "r")

q0 <- file.h5[["data/total_demand"]]$read() %>% pmax(0) 
q1 <- file.h5[["data/total_demand0"]]$read() %>% pmax(0) 
q2 <- file.h5[["data/total_demand1"]]$read() %>% pmax(0) 
q3 <- file.h5[["data/total_demand2"]]$read() %>% pmax(0) 
# 
# 
# # Convert to distance matrix (inverse of flows)
# # Add small epsilon to avoid divide-by-zero
# epsilon <- 1e-6
# dist_matrix <- 1 / (q0 + epsilon)
# 
# # Optional: make it symmetric (if flows aren't)
# dist_matrix <- (dist_matrix + t(dist_matrix)) / 2
# 
# # Convert to dist object for clustering
# dist_object <- as.dist(dist_matrix)
# 
# # Perform hierarchical clustering
# hc <- hclust(dist_object, method = "average")  # or "ward.D", "complete", etc.
# cutree(hc,10)
# # Plot dendrogram
# plot(hc, main = "Hierarchical Clustering of OD Matrix", xlab = "", sub = "")
# k = 30
shp <- st_read("C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/resources/geo/NewTazArzi_848.shp",crs = 4326) %>% 
  arrange(TAZ) 
# within_cluster_sums <- sapply(1:k, function(cl) {
#   members <- which(shp$clus7==cl) 
#   
#   sub_matrix <- q0[members, members]
#   sum(sub_matrix)
# })
# 
# shp %>% 
#   group_by(clus7) %>% 
#   summarise() %>% 
#   mutate(within_cluster_sums = within_cluster_sums) %>% 
#   filter(within_cluster_sums > 1000) %>% 
#   mapview(zcol = "within_cluster_sums")


cluster_flows_summary <- function(od_matrix, k = 2, method = "average", epsilon = 1e-6) {
  # Step 1: Compute distance matrix
  dist_matrix <- 1 / (od_matrix + epsilon)
  dist_matrix <- (dist_matrix + t(dist_matrix)) / 2  # Ensure symmetry
  dist_object <- as.dist(dist_matrix)
  
  # Step 2: Hierarchical clustering
  hc <- hclust(dist_object, method = method)
  clusters <- cutree(hc, k)
  
  # Step 3: Create data frame of zone-cluster mapping
  zone_names <- 1:nrow(od_matrix)
  cluster_df <- data.frame(
    zone = zone_names,
    cluster = clusters
  )
  
  # Step 4: Within-cluster flows
  within_cluster_sums <- sapply(1:k, function(cl) {
    members <- cluster_df$zone[cluster_df$cluster == cl]
    sum(od_matrix[members, members])
  })
  names(within_cluster_sums) <- paste0("Cluster_", 1:k)
  
  # Step 5: Between-cluster flows (k x k matrix)
  between_cluster_flows <- matrix(0, nrow = k, ncol = k,
                                  dimnames = list(paste0("C", 1:k), paste0("C", 1:k)))
  
  for (i in 1:k) {
    for (j in 1:k) {
      zones_i <- cluster_df$zone[cluster_df$cluster == i]
      zones_j <- cluster_df$zone[cluster_df$cluster == j]
      between_cluster_flows[i, j] <- sum(od_matrix[zones_i, zones_j])
    }
  }
  
  # Step 6: Return results
  return(list(
    clusters = clusters,
    within_cluster_sums = within_cluster_sums,
    between_cluster_flows = between_cluster_flows,
    hc = hc
  ))
}
result
calculate_modularity_directed <- function(flow_matrix) {
  stopifnot(nrow(flow_matrix) == ncol(flow_matrix))
  m            <- sum(flow_matrix)
  out_degrees  <- rowSums(flow_matrix)
  in_degrees   <- colSums(flow_matrix)
  
  Q <- 0
  for (i in seq_len(nrow(flow_matrix))) {
    for (j in seq_len(ncol(flow_matrix))) {
      # only count intra‐cluster terms (i == j)
      if (i == j) {
        expected <- (out_degrees[i] * in_degrees[j]) / m
        Q <- Q + (flow_matrix[i, j] - expected)
      }
    }
  }
  Q / m
}
# From previous cluster_flows_summary
map_df(2:100,~{
  result <- cluster_flows_summary(q0,.x)
  modularity <- calculate_modularity_directed(result$between_cluster_flows)
  data.frame(n = .x,mod1 = modularity, idx = modularity/.x) %>% 
    mutate(l = mod1 - lag(mod1))
}) %>% ggplot() +
  geom_line(mapping = aes(x = n, y = mod1), color = "black") + 
  geom_line(mapping = aes(x = n, y = idx), color = "red") 

result <- cluster_flows_summary(q0,11)
calculate_modularity_directed(result$between_cluster_flows)
shp %>% 
  mutate(clus = result$clusters) %>% 
  group_by(clus) %>% 
  summarise() %>% 
  mutate(within_cluster_sums = result$within_cluster_sums) %>% 
  mapview(zcol = "within_cluster_sums")
res1 <- q0 %>% 
  as_tibble() %>% 
  rownames_to_column("from") %>% 
  gather(to, weight,-from) %>% 
  mutate(to = str_remove(to,"V")) %>% 
  filter(from != to) %>% 
  as_tbl_graph(directed = F) 
map_dbl(1:100,~{
  set.seed(.x)
  print(.x)
  res2 <- res1 %>% 
    mutate(grp = group_louvain(weights = weight)) %>% 
    pull(grp)
  modularity(res1,res2,res1 %E>% as_tibble() %>% pull(weight))
}
) %>% as_tibble() 
set.seed(1)
res1 <- q0 %>% 
  as_tibble() %>% 
  rownames_to_column("from") %>% 
  gather(to, weight,-from) %>% 
  mutate(to = str_remove(to,"V")) %>% 
  as_tbl_graph(directed = F) 
res2 <- res1 %>% 
  mutate(grp = group_louvain(weights = weight)) %>% 
  pull(grp)
modularity(res1,res2,res1 %E>% as_tibble() %>% pull(weight))
shp %>% 
  mutate(clus = res2) %>% 
  group_by(clus) %>% 
  summarise() %>% 
  mapview()
