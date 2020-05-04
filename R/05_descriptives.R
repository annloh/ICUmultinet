# Descriptives ------------------------------------------------------------
source("R/utils.R")


# Sample size -------------------------------------------------------------

sample_size <- nrow(icu_inverted)

# Demographics ------------------------------------------------------------


# Gender ------------------------------------------------------------------

n_boys <- table(icu_inverted$gender)["1"]
prop_boys <- table(icu_inverted$gender) %>% prop.table() %>% .["1"]*100
prop_boys <- prop_boys %>% round(digits = 2)

n_girls <- table(icu_inverted$gender)["2"]
prop_girls <- table(icu_inverted$gender) %>% prop.table() %>% .["1"]*100
prop_girls <- prop_girls %>% round(digits = 2)

# Age ---------------------------------------------------------------------

age_mean <- mean(icu_inverted$age, na.rm = TRUE) %>% round(digits = 2)
age_sd <- sd(icu_inverted$age, na.rm = TRUE) %>% round(digits = 2)

age_min <- min(icu_inverted$age, na.rm = TRUE)
age_max <- max(icu_inverted$age, na.rm = TRUE)


# Save for import in manuscript -------------------------------------------
save(sample_size, n_boys, prop_boys, n_girls, prop_girls,
     age_mean, age_sd, age_min, age_max,
     file = "analysis/manuscript/demographics.RData")

# Item analyses -----------------------------------------------------------

data_imp <- readRDS("analysis/data/data_imp.RSD")
summary(imp_data)

# ICU item analysis -------------------------------------------------------

# raw_alpha = Cronbach's alpha
# raw.r = item total correlations
# r.drop = corrected item total correlation

icu_item_analysis <- icu_inverted %>% select(all_of(icu_items)) %>% alpha()
icu_item_analysis$item.stats

# Table for manuscript


# Scale and Item Intercorrelations ----------------------------------------

descriptives <- describe(ICU_data)




descriptives <- psych::describe(data)

# item analysis
# raw_alpha = Cronbach's alpha
# raw.r = item total correlations
# r.drop = corrected item total correlation

################################################################################
# DESCRIPTIVES ICU
################################################################################

# Descriptives ICU --------------------------------------------------------

# ICU (original item coding)
# 1 = "stimmt gar nicht"
# 2 = "stimmt etwas"
# 3 = "stimmt meistens"
# 4 = "stimmt genau"


# ICU Descriptives
descriptives_icu <- psych::describe(icu)

# Item analysis
icu_item_analysis <- icu_recoded %>% psych::alpha()
icu_item_analysis$item.stats

item_characteristics_table <- data.frame(item = descriptives_icu$vars,
                                         content = icu_item_content,
                                         mean = round(descriptives_icu$mean, digits = 2),
                                         sd = round(descriptives_icu$sd, digits = 2),
                                         skew = round(descriptives_icu$skew, digits = 2),
                                         kurtosis = round(descriptives_icu$kurtosis, digits = 2),
                                         se = round(descriptives_icu$se, digits = 2),
                                         r_it = round(icu_item_analysis$item.stats$r.drop, digits = 2)
                                         )

knitr::kable(
  item_characteristics_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  longtable = TRUE,
  col.names = c("Item", "Item Content", "M", "SD", "Skew", "Kurtosis", "SE", "$r_it$"),
  align = c("l", "l", "c", "c", "c", "c", "c", "c"),
  caption = "Item characteristics for ICU items") %>%
    row_spec(row = 0, align = "c") %>%
      kable_styling(full_width = TRUE) %>%
        footnote(
          general_title = "Note.",
          general = "(-) indicates inversely scored items.",
          threeparttable = TRUE,
          footnote_as_chunk = TRUE)


# Correlation analysis ----------------------------------------------------

# Computation od subscales ------------------------------------------------


scales_df <- data_imp %>%
                mutate(uncaring = ICU04 + ICU08+ ICU12+ ICU17+ ICU21 +ICU24,
                       unemotional = ICU01 + ICU06 + ICU10 + ICU14 + ICU19 +ICU22,
                       callous = ICU02 + ICU05 +ICU09 + ICU13 + ICU16 + ICU18,
                       careless = ICU03 + ICU07 + ICU11 + ICU15 + ICU20 + ICU23,
                       ICU_total = ICU01 +ICU02 + ICU03 + ICU04 + ICU05 + ICU06 +
                         ICU07 + ICU08 + ICU09 + ICU10 + ICU11 + ICU12 +ICU13
                         + ICU14 + ICU15 + ICU16 + ICU17 + ICU18 + ICU19 + ICU20 +
                         ICU21 + ICU22 + ICU23 + ICU24) %>%
                        select(-all_of(icu_items)) %>% select(-age,-gender)

scales_pre_df <- icu_pre_imp %>% select(all_of(c("battery_student", "tease_student",
"vandalize", "extort", "exclude", "ignore", "ridicule_teacher",
"mean_teacher", "battery_teacher", "vehicle_theft",
"larceny", "burglary", "battery", "battery_mult_perp", "armed_battery",
"robbery", "extortion", "sexual_assault",
"CADRI01", "CADRI02", "CADRI03", "CADRI04", "CADRI05",
"CADRI06", "CADRI07", "CADRI08", "CADRI09", "CADRI10", "ICU01",
"ICU02", "ICU03", "ICU04", "ICU05", "ICU06", "ICU07", "ICU08",
"ICU09", "ICU10", "ICU11", "ICU12", "ICU13", "ICU14", "ICU15",
"ICU16", "ICU17", "ICU18", "ICU19", "ICU20", "ICU21", "ICU22",
"ICU23", "ICU24"))) %>%
                mutate(CADRI01 = CADRI01-1,
                       CADRI02 = CADRI02-1,
                       CADRI03 = CADRI03-1,
                       CADRI04 = CADRI04-1,
                       CADRI05 = CADRI05-1,
                       CADRI06 = CADRI06-1,
                       CADRI07 = CADRI07-1,
                       CADRI08 = CADRI08-1,
                       CADRI09 = CADRI09-1,
                       CADRI10 = CADRI10-1,
                       uncaring = ICU04 + ICU08+ ICU12+ ICU17+ ICU21 +ICU24,
                       unemotional = ICU01 + ICU06 + ICU10 + ICU14 + ICU19 +ICU22,
                       callous = ICU02 + ICU05 +ICU09 + ICU13 + ICU16 + ICU18,
                       careless = ICU03 + ICU07 + ICU11 + ICU15 + ICU20 + ICU23,
                       ICU_total = ICU01 +ICU02 + ICU03 + ICU04 + ICU05 + ICU06 +
                         ICU07 + ICU08 + ICU09 + ICU10 + ICU11 + ICU12 +ICU13
                         + ICU14 + ICU15 + ICU16 + ICU17 + ICU18 + ICU19 + ICU20 +
                         ICU21 + ICU22 + ICU23 + ICU24) %>%
                        select(-all_of(icu_items))


View(scales_df)

cor(scales_df) %>% round(digits = 2)
cor(scales_df, method = "spearman") %>% round(digits = 2)

mixedCor(data = scales_df, ncat = 5)
cor_table <- mixedCor(data = scales_pre_df, ncat = 5) %>% round(digits = 2)
cor_table$rho %>% round(digits =2)


# Descriptives for all items ----------------------------------------------

descriptives_data <- psych::describe(scales_df)

descriptives_data_PRE <- psych::describe(scales_pre_df)



# Compiling descriptives table and printing as latex output ---------------


descriptives_data_PRE %>% select(-c(vars, trimmed, mad))

install.packages("xtable")
library(xtable)

latex <- descriptives_data_PRE %>% select(-c(vars, trimmed, mad)) %>% xtable()
print(latex, booktabs = TRUE, tabular.envirionment = "longtable")



# Compiling Correlation table and printig as latex code -------------------


cor_table_df <- cor_table$rho %>% round(digits =2) %>% as.data.frame(rownames = "Item",
                                                                     optional = TRUE)

latex_cor <- cor_table_df %>% select(uncaring, unemotional, callous, careless) %>% xtable()
print(latex_cor, booktabs = TRUE, tabular.envirionment = "longtable")

?as.data.frame
