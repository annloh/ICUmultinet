# Network analysis --------------------------------------------------------
library(tidyverse)
library(bootnet)
library(networktools)
library(igraph)
library(purrr)

source("R/06_multiverse_definition.R")

# Read imputed data -------------------------------------------------------

dat_imp <- readRDS("analysis/data/dat_imp.RSD")

icu_items <- c("ICU01", "ICU02", "ICU03", "ICU04", "ICU05",
  "ICU06", "ICU07", "ICU08", "ICU09", "ICU10", "ICU11", "ICU12",
  "ICU13", "ICU14", "ICU15", "ICU16", "ICU17", "ICU18", "ICU19",
  "ICU20", "ICU21", "ICU22", "ICU23", "ICU24")

# Item overlap ------------------------------------------------------------

networktools::goldbricker(dat_imp %>% select(icu_items),
            p = 0.05,
            method = "hittner2003",
            threshold = 0.25,
            corMin = 0.5,
            progressbar = TRUE)


# check whether data is appropriate for network analysis
networktools::assumptionCheck(data = dat_imp %>% select(icu_items),
                              type= "network")

# Equal node variance violation: variance of the following nodes differs more than 20% from grand mean:
# ICU04 ICU05 ICU06 ICU11 ICU21 ICU22
#
# Grand mean of variances =  0.7647441
# Node variances
#     ICU01     ICU02     ICU03     ICU04     ICU05     ICU06     ICU07     ICU08     ICU09     ICU10     ICU11     ICU12     ICU13     ICU14     ICU15     ICU16     ICU17     ICU18
# 0.7760111 0.7655236 0.7912579 0.4017037 1.0575879 1.0762403 0.6262801 0.7862260 0.7371817 0.9033305 0.5062168 0.7888540 0.7755068 0.7668339 0.6565931 0.7298871 0.7476953 0.8781645
#     ICU19     ICU20     ICU21     ICU22     ICU23     ICU24
# 0.7907216 0.7381044 0.5611523 1.0229544 0.7113365 0.7584945



# Subscale definition -----------------------------------------------------

# ICU Subscales corresponding to LPE specifier


lpe <- c("unemotional", "callous", "careless", "uncaring",
         "callous", "unemotional", "careless", "uncaring",
         "callous", "unemotional", "careless", "uncaring",
         "callous", "unemotional", "careless", "callous",
         "uncaring", "callous", "unemotional", "careless",
         "uncaring", "unemotional", "careless", "uncaring")



# Network estimation ------------------------------------------------------


parameters$huge_params %>%
    pmap(function(...) {
            args <- list(...)
            args$data <- dat_imp %>% select(all_of(icu_items))
            multiverse_id <- args$multiverse_id
            args$multiverse_id <- NULL

            if(args$default == "mlm") {args$type <- rep("c", 24)}
            if(is.na(args$tuning)){args$tuning <- NULL}
           set.seed(42)
           net <- do.call(bootnet::estimateNetwork, args) %>%
           bootnet::bootnet(nBoots = 100,
            statistics = c("edge",
                           "strength",
                            "outStrength",
                            "inStrength",
                            "expectedInfluence",
                            "outExpectedInfluence",
                            "InExpectedInfluence",
                            "bridgeStrength",
                            "bridgeCloseness",
                            "bridgeBetweenness",
                            "rspbc"),
            type = "case",
            verbose = FALSE,
            communities = lpe)

           out <- c(net, multiverse_id = multiverse_id)
           saveRDS(out, paste0("tmp/bootnet_", multiverse_id, ".rds"))
           return(out)
          })




# Community detection ----------------------------------------------------

#icu_net_estimate was result from (estimateNetwork)

#bridge stuff from networktools
icu_bridge <- bridge(network= icu_net_estimate$graph)
icu_bridge$communities
cbind(seq(1:24), icu_bridge$communities, lpe)


# walktrap community detection from the igraph package
i_graph <- igraph::graph_from_adjacency_matrix(icu_net_estimate$graph,
                                               mode = "undirected",
                                               weighted = TRUE)

icu_walktrap_communities <- igraph::cluster_walktrap(i_graph) %>%
                              igraph::membership()



icu_spinglas_communities <- igraph::cluster_spinglass(i_graph) %>%
                              igraph::membership()
