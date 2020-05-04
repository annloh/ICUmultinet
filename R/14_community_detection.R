source("R/08_import_icu_network.R")

# Community detection -----------------------------------------------------

# Community detection for overall median network --------------------------

adj_mat_igraph <- convert2igraph(median_median_graph_multi_list)
set.seed(42)
median_communities <- cluster_walktrap(adj_mat_igraph)


# Community detection for each poit estimate of the multiverse ana --------



multiverse_communities <- icu_multiverse_adj_mat %>% map(~{set.seed(42)
                                                 tmp <- convert2igraph(.x)
                                                   cluster_walktrap(tmp)$membership
                                                 })

# Prepare Community data for plotting -------------------------------------

trans_multiverse_communities <- transpose(multiverse_communities)
names(trans_multiverse_communities) <- icu_items

community_data <- as_tibble(trans_multiverse_communities) %>%
  as.data.frame() %>%
    cbind(multiverse_id = index,.) %>%
      gather(-multiverse_id, key = "item", value = "community")

community_df <- data_frame(multiverse_id = community_data$multiverse_id,
           item = community_data$item,
           community = unlist(community_data$community))


# Plotting community membership -------------------------------------------


community_plot <- community_df %>%
  ggplot(aes(x = as_factor(item),
             y = fct_rev(factor(multiverse_id, levels = as.character(index))),
             fill = as_factor(community))) +
    geom_tile(alpha=1) +
    scale_fill_manual(values = c("#00aedb", "#a200ff", "#f47835", "#d41243", "#8ec127", "#007228"), #) +
                      labels = c("Community 1", "Community 2", "Community 3", "Community 4", "Community 5", "Community 6"),
                      name = "") +
    labs(y = "Multiverse ID",
         x = "") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12,
                                     angle = 45,
                                     hjust = 1,
                                     face = "bold"),
          axis.text.y = element_text(size = 12,
                                     face = "bold"),
          axis.title.y = element_text(size = 13),
          legend
            )


