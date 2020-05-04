source("R/00_dependencies.R")
source("R/02_utils.R")
source("R/08_import_icu_networks")


# Compute bridge expected influence ---------------------------------------


bridge_list <- graph_multi_list %>% map(~{
          tmp <- bridge(network = as.matrix(.x),
                 communities = lpe_groups)[["Bridge Expected Influence (1-step)"]]
          names(tmp) <- icu_items
          return(tmp)})


bridge_list[[1]] %>% glimpse
bridge_list_df <- bridge_list %>% tibble()

bridge_df <- bridge_list_df %>% unnest_longer(col = 1)
names(bridge_df) <- c("value","id")
bridge_ei_df <- tibble(bridge_df,
                       multiverse_id = rep(rep(index, each = 24), 100),
                       type = "Bridge Expected Influence")



# Compute stabilizing index -----------------------------------------------

compute_stabilizing_index(adj_matrix = median_median_graph_multi_list,
                          communities = lpe)


stabilizing_index_boot <- graph_list_multi %>%
  map(~{map(.x, ~compute_stabilizing_index(.x,communities = lpe))
        })

stabilizing_index_df <-tibble(multiverse_id = index, stabilizing_index = stabilizing_index_boot) %>%
  unnest_auto(col =2) %>% unnest_wider(col=2) %>%
    gather(-multiverse_id, key = "id", value = "value") %>%
      mutate(type = "Stabilizing Index")



# Combined data frame -----------------------------------------------------



meso_plot_df <- bind_rows(stabilizing_index_df %>% select(multiverse_id, id, type, value),
                          bridge_ei_df %>% select(multiverse_id, id, type, value))




# Cleanup -----------------------------------------------------------------

rm(list = "stabilizing_index_df", "stabilizing_index_boot", "bridge_ei_df",
   "bridge_df", "bridge_list_df", "bridge_list")


