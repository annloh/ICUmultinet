library(tidyverse)
library(patchwork)
library(forcats)

source("R/02_utils.R")
source("R/06_multiverse_definition.R")
source("R/08_import_icu_networks.R")



# ICU network plotting function  -----------------------------------------

plot_network <- function(adjmatrix, communities = NULL, seed){
  set.seed(seed)
  qgraph(input = adjmatrix,
                  layout = "spring",
                  palette = "colorblind",
                  theme = "colorblind",
                  groups = communities,
                  legend = TRUE,
                  borders = TRUE)
}


# Plotting the median network (for main body of thesis) -------------------

median_net_plot <- plot_network(adjmatrix = median_median_graph_multi_list,
                                communities = lpe_groups,
                                seed = 42)

median_cov_net_plot <- plot_network(adjmatrix = median_median_adj_mat_boot,
                                seed = 42)

# Plotting each individual network (for appendix) -------------------------

multiverse_plot_list <- icu_multiverse_adj_mat %>% map(~plot_network(adjmatrix = .x,
                                                                    communities = lpe_groups,
                                                                    seed = 42))


# Arranging and saving plots ----------------------------------------------

multiverse_plot_list %>% map2(index, ~{

pdf(paste0("analysis/figures/multiverse_nets_",.y,".pdf"), width = 8, height = 5.75)

plot(.x)

dev.off()
})
