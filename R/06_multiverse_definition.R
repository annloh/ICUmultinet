# Multiverse Definition ---------------------------------------------------


# EBICglasso based scenarios ----------------------------------------------

EBICglasso_params <- expand.grid(default = "EBICglasso",
                                 tuning = c(0, 0.25, 0.5),
                                  corMethod = c("cor_auto", "npn", "spearman"),
                                  missing = "pairwise",
                                  sampleSize = "maximum",
                                  nonPositiveDefinite = "stop",
                                  stringsAsFactors = F)

EBICglasso_params <- cbind(multiverse_id = 1:nrow(EBICglasso_params),
                           EBICglasso_params)



# pcor based scenarios ----------------------------------------------------

pcor_params <- expand.grid(default = "pcor",
                           corMethod = c("cor_auto", "npn", "spearman"),
                           missing = "pairwise",
                           nonPositiveDefinite = "stop",
                           stringsAsFactors = F)

pcor_params <- cbind(multiverse_id = (nrow(EBICglasso_params)+1):(nrow(EBICglasso_params)+nrow(pcor_params)),
                     pcor_params)


# huge based scenarios ----------------------------------------------------


# huge uses glasso algorithm,  npn, does not use polychoric

huge_params <- expand.grid(default = "huge",
                           tuning = c(0, 0.25, 0.5),
                           criterion = c("ebic", "ric", "stars"),
                           npn = c(TRUE, FALSE),
                           missing = "listwise",
                           stringsAsFactors = F)

# Filter out unwanted combinations
huge_params <- huge_params %>% filter(!(criterion %in% c("ric", "stars")& tuning>0)) %>%
  mutate(tuning = ifelse(criterion %in% c("ric", "stars"), NA, tuning))

huge_params <- cbind(multiverse_id = (max(pcor_params$multiverse_id)+1):(nrow(EBICglasso_params)+nrow(pcor_params)+nrow(huge_params)),
                     huge_params)


# mgm based scenarios -----------------------------------------------------

mgm_params <- expand.grid(default = "mgm",
                          tuning = c(0, 0.25, 0.5),
                          criterion = c("EBIC", "CV"),
                          rule = "OR",
                          missing = "listwise",
                          stringsAsFactors = F)

mgm_params <- mgm_params %>% filter(!(criterion == "CV" & tuning>0)) %>%
  mutate(tuning = ifelse(criterion == "CV", NA, tuning))

mgm_params <- cbind(multiverse_id = (max(huge_params$multiverse_id)+1):(max(huge_params$multiverse_id)+nrow(mgm_params)),
                    mgm_params)


# Combining scenarios -----------------------------------------------------

parameters <- list(EBICglasso_params = EBICglasso_params,
                pcor_params = pcor_params,
                huge_params = huge_params,
                mgm_params = mgm_params)

# remove individual objects after combining them to a list
rm(EBICglasso_params, pcor_params, huge_params, mgm_params)

# Multiverse grouping variables -------------------------------------------

grouping_vars <- data.frame(
  multiverse_id <- 1:26,
  estimator <- c(rep("ggm", 22), rep("mgm", 4)),
  regularized <- c(rep(TRUE, 9), rep(FALSE, 3), rep(TRUE, 14)),
  ebic <- c(rep(TRUE, 9), rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 2),
            rep(TRUE, 3), rep(FALSE, 2), rep(TRUE, 3), rep(FALSE, 1)),
  cor_method <- c(rep("poly", 3), rep("npn", 3), rep("spearman",3),
                  c("poly", "npn", "spearman"), rep("npn", 5), rep("spearman", 5), rep("mgm", 4)),
  stringsAsFactors = F)

names(grouping_vars) <- c("multiverse_id", "estimator", "regularized", "ebic", "cor_method")

