source("R/03_import_and cleaning.R")
source("R/02_utils.R")

# Missing data imputation -------------------------------------------------

# Missingness visualization -----------------------------------------------

itemwise_missing <- icu_pre_imp %>% select(all_of(relevant_variables))

# adding rownnumbers
itemwise_missing$id <- seq.int(nrow(itemwise_missing))

# creating a missingness indicator for every cell
itemwise_missing %<>%
   gather(-c(age, id, lfdnr, gender), key = "key", value = "val") %>%
      mutate(isna = is.na(val))

# factor label
key_labels <- c("Vandalism", "Shoplifting", "Vehicle Theft", "Larceny", "Burglary",
"Battery", "Battery mult. perp.", "Armed Battery", "Robbery", "Extortion",
"Sexual Assault", "ICU01", "ICU02", "ICU03", "ICU04", "ICU05",
"ICU06", "ICU07", "ICU08", "ICU09", "ICU10", "ICU11", "ICU12",
"ICU13", "ICU14", "ICU15", "ICU16", "ICU17", "ICU18", "ICU19",
"ICU20", "ICU21", "ICU22", "ICU23", "ICU24", "Battery Student",
"Tease Student", "Vandalize", "Extort", "Exclude", "Ignore",
"Ridicule Teacher", "Mean Teacher", "Battery Teacher")

# raster plot of missingness for every cell

row.plot <- itemwise_missing %>%
  ggplot(aes(fct_rev(factor(key, levels = unique(key), labels = key_labels)),
             id,
             fill = isna)) +
    geom_raster(alpha=1) +
    scale_fill_manual(values = c("#cccccc", "black"), #) +
         labels = c("Present", "Missing"),
         name = "") +
 #    scale_x_discrete(limits = levels) +
    labs(x = "",
           y = "Row Number") +
    coord_flip() +
    theme_classic()



# Missingness descriptives ------------------------------------------------

missingness <- itemwise_missing %>%
  group_by(key) %>%
  summarise(total = sum(isna),
            percent = round(sum(isna)/nrow(icu_pre_imp)*100, digits = 3))

missingness %>% summarize(mean = mean(percent),
                          sd = sd(percent))
# Imputation --------------------------------------------------------------

imputation_set <- c("age", "gender", "vandalism", "shoplifting", "vehicle_theft",
                    "larceny", "burglary","battery", "battery_mult_perp",
                    "armed_battery", "robbery", "extortion", "sexual_assault",
                    "battery_student","tease_student", "vandalize", "extort",
                    "exclude", "ignore", "ridicule_teacher", "mean_teacher",
                    "battery_teacher", "ICU01", "ICU02", "ICU03", "ICU04", "ICU05",
                    "ICU06", "ICU07", "ICU08", "ICU09", "ICU10", "ICU11", "ICU12",
                    "ICU13", "ICU14", "ICU15", "ICU16", "ICU17", "ICU18", "ICU19",
                    "ICU20", "ICU21", "ICU22", "ICU23", "ICU24")



# Defining predictor matrix -----------------------------------------------

# Default in mice is that every variable predicts all others
# Predictor matrix is defined such that only age, gender and ICU items are used for imputation

predMatrix <- matrix(rep(1, length(imputation_set)^2),
                     ncol=length(imputation_set))
predMatrix[c(3:22), c(3:22)] <- 0
diag(predMatrix) <- 0

set.seed(427)
icu_imputation<- mice::mice(icu_pre_imp[imputation_set],
                            m = 1,
                            method = "pmm",
                            predictorMatrix = predMatrix)

data_imp <- mice::complete(icu_imputation)


# Save imputed data set ---------------------------------------------------


saveRDS(data_imp,"analysis/data/data_imp.RSD")

