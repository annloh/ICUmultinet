#source("R/00_dependencies.R")

# Helper functions --------------------------------------------------------

# Function to compute the median matrix from a list of matrices
compute_median_matrix <- function(matrix_list){
                                  Y <- do.call(cbind, matrix_list)
                                  Y <- array(Y, dim = c(dim(matrix_list[[1]]), length(matrix_list)))
                                  apply(Y, c(1, 2), median, na.rm = TRUE)}

# Function to reapply row and column names lost in the process

rename_matrix <- function(matrix, matrix_names){
                          rownames(matrix) <- matrix_names
                          colnames(matrix) <- matrix_names}

# Stabilizing index (Blanken et al 2018) ----------------------------------

compute_stabilizing_index <- function(adj_matrix, communities){
  tmp <- 1:dim(adj_matrix)[[1]] %>% map_dbl(~{sum(adj_matrix[.x,which(communities==communities[.x])])
                                              })
  names(tmp) <- icu_items
  return(tmp)
}

# Function for counting pos. and neg. edges in an estimated network -------

count_edges <- function(adjacency_matrix, id){

                max <-  dim(adjacency_matrix)[[1]]*(dim(adjacency_matrix)[[1]]+1)/2
                edge_total <- sum(adjacency_matrix[upper.tri(adjacency_matrix, diag=F)]!=0)
                neg_edge <- sum(adjacency_matrix[upper.tri(adjacency_matrix, diag=F)]<0)
                data.frame(mutiverse_id = id,
                           edge_total = edge_total,
                           neg_edge = neg_edge) %>%
                  mutate(percent_total = round(edge_total/max*100),
                         percent_neg = round(neg_edge/max*100))}


