source("R/00_dependencies")
source("R/01_helper_functions.R")
source("R/02_import_icu_network.R")

# Network descriptives ----------------------------------------------------

edge_count_icu <- icu_multiverse_adj_mat %>% map2_dfr(index_icu, ~count_edges(adjacency_matrix = .x,
                                            id = .y))

edge_summary_icu <- edge_count_icu %>% summarise(mean_edge = round(mean(edge_total), digits = 2),
                                         mean_neg = round(mean(neg_edge), digits = 2),
                                         sd_edge = round(sd(edge_total), digits = 2),
                                         sd_neg = round(sd(neg_edge), digits = 2),
                                         min_edge = min(edge_total),
                                         max_edge = max(edge_total),
                                         min_neg = min(neg_edge),
                                         max_neg = max(neg_edge))


# Save for import in manuscript -------------------------------------------

save(edge_summary_icu,
     file = "analysis/manuscript/network_descriptives_icu.RData")


# Network Connectivity ----------------------------------------------------

# Compute the average and standard deviation of the weights in the network
# NetworkToolbox::conn(adjMatrix)
