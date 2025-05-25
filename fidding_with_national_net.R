pacman::p_load(tidyverse,tidygraph,sf,mapview,janitor,sfnetworks)
path <- "C:/Users/idshk/OneDrive/idos_shit/National_model_v0.33_open_data/2020/Network/base_network.in"
step1 <- read_lines(path) %>% 
  as_tibble() %>% 
  mutate(fi = str_sub(value,1,1)) %>% 
  slice(-1) %>% 
  mutate(q = cumsum(fi == "t")) 
nodes <- step1 %>%
  filter(q == 1,fi != "t") %>% 
  select(value) %>% 
  separate(value,as.character(1:8) ,"\\s+") %>% 
  row_to_names(1)
nds <- st_as_sf(nodes,coords = c("X-coord","Y-coord"),crs = 4326) 
edges <- step1 %>%
  filter(q == 2,fi != "t") %>% 
  select(value) %>% 
  separate(value,as.character(1:11) ,"\\s+") %>% 
  row_to_names(1) %>% 
  rename(from = From,to = To)

net <- sfnetwork(nds,edges,node_key = "Label",directed = F) %E>% 
  filter(Lane != "9.000000",str_detect(Modes,"c")) %>% 
  convert(to_largest_component) 
set.seed(1)
net %>% 
  mutate(weight = 1/as.numeric(Length))%N>% 
  mutate(grp = group_louvain(weights = weight,resolution = 0.1)) %>% 
  as_tibble() %>% 
  ggplot(aes(color = as.factor(grp))) + 
  geom_sf()

