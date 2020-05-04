source("R/00_dependencies.R")
source("R/02_utils.R")
source("R/01_helper_functions.R")

# Import ICU networks  ----------------------------------------------------


# create character vector with pathnames
index_icu <- c(1:16, 18:20, 23:26) #this is created manually based on the availible files

path_names <- map_chr(index, ~{paste0("tmp/bootnet_", .x, ".rds")})

# Dataframe with edges and node statistics (sample point estimates)
icu_multiverse_data <- path_names %>%
                        purrr::map_dfr(~{tmp <- readRDS(.x)
                                  data.frame(multiverse_id = tmp$multiverse_id,
                                             tmp$sampleTable,
                                             stringsAsFactors = F)})

# List of adjacency matrices from sample networks
icu_multiverse_adj_mat <- path_names %>%
                               purrr::map(~{tmp <- readRDS(.x)
                                        tmp$sample$graph})


# Dataframe with bootstrapped edges and node statistics
icu_multiverse_boot_data <- path_names %>%
                               purrr::map_dfr(~{tmp <- readRDS(.x)
                                        data.frame(multiverse_id = tmp$multiverse_id,
                                                   tmp$bootTable,
                                                   stringsAsFactors = F)})

# List of Adjacency matrices from bootstrapped samples

graph_list_multi <- path_names %>%
                               purrr::map(~{tmp <- readRDS(.x)
                                        map(tmp$boots, ~{.$graph})})


# Computing median Network (median of medians) ----------------------------

# Flatten the list
graph_multi_list <- unlist(graph_list_multi, recursive = FALSE)


# compute the median of medians

median_graph_multi_list <- graph_list_multi %>%
  map(compute_median_matrix)

median_median_graph_multi_list <- median_graph_multi_list %>% compute_median_matrix()


rename_matrix(matrix = median_median_graph_multi_list,
              matrix_names = icu_names)




# Median of point estimates (sample networks) -----------------------------

icu_sample_median_graph <- compute_median_matrix(icu_multiverse_adj_mat)

rename_matrix(matrix = icu_sample_median_graph,
              matrix_names = icu_names)

