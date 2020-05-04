source('R/00_dependencies.R')

# Data import -------------------------------------------------------------

ICU_data <- haven::read_sav("analysis/data/raw_data/ICU_selected_raw.sav") %>%
  haven::zap_labels() %>%
    haven::zap_label()


# Data import -------------------------------------------------------------

ICU_data_missings <- haven::read_sav("analysis/data/raw_data/FSozU.preIMP.sav") %>%
  haven::zap_labels() %>%
    haven::zap_label()

ICU_missings <- ICU_data_missings %>%
  select(lfdnr,  geschl, trait01, trait02, trait03, trait04, trait05, trait06,
         trait07,trait08, trait09, trait10, trait11, trait12, trait13, trait14,
         trait15, trait16, trait17, trait18, trait19, trait20, trait21,trait22,
         trait23, trait24) %>%
    rowwise() %>%
      mutate(n_missing = sum (is.na(c(trait01, trait02, trait03, trait04,
                                      trait05, trait06, trait07, trait08,
                                      trait09, trait10, trait11, trait12,
                                      trait13, trait14,trait15, trait16,
                                      trait17, trait18, trait19, trait20,
                                      trait21, trait22, trait23, trait24))))


# Filter cases with all missing ICU values OR missing gender

ICU_missings_filtered <-  ICU_missings %>%
  filter(n_missing != 24 & !is.na(geschl))

# ICU subset --------------------------------------------------------------

icu_items <- c("trait01", "trait02", "trait03", "trait04", "trait05",
               "trait06", "trait07", "trait08", "trait09", "trait10",
               "trait11", "trait12", "trait13",
               "trait14", "trait15", "trait16",
               "trait17", "trait18", "trait19","trait20", "trait21", "trait22",
                "trait23", "trait24")


ICU_data[icu_items] <-  ICU_missings_filtered[icu_items]


# Item renaming -------------------------------------------------------

icu_renamed<- ICU_data %>%
  rename_at(vars(starts_with("trait")),
            ~(str_replace(., "trait", "ICU"))) %>%
    rename_at(vars(starts_with("datei")),
              ~(str_replace(., "datei", "CADRI"))) %>%
      rename(age = alter, gender = geschl)



# Recoding the dbs items -------------------------------------------------

icu_recode_na <- icu_renamed %>% mutate(dbs05lj = if_else(dbs05 == 1, 1, dbs05lj),
                                        dbs01lj = if_else(dbs01 == 1, 1, dbs05lj),
                                        dbs15lj = if_else(dbs15 == 1, 1, dbs15lj),
                                        dbs07lj = if_else(dbs07 == 1, 1, dbs07lj),
                                        dbs25lj = if_else(dbs25 == 1, 1, dbs25lj),
                                        dbs20lj = if_else(dbs20 == 1, 1, dbs20lj),
                                        dbs02lj = if_else(dbs02 == 1, 1, dbs02lj),
                                        dbs31lj = if_else(dbs31 == 1, 1, dbs31lj),
                                        dbs32lj = if_else(dbs32 == 1, 1, dbs32lj),
                                        dbs10lj = if_else(dbs10 == 1, 1, dbs10lj),
                                        dbs29lj = if_else(dbs29 == 1, 1, dbs29lj),
                                        dbs27lj = if_else(dbs27 == 1, 1, dbs27lj)
                                        )


# ICU Item recoding -----------------------------------------------------------

# recoding to correct scale

icu_recoded <- icu_recode_na %>% mutate_at(vars(starts_with("ICU")),
                    ~(recode(., `4`=3, `3`=2, `2`=1, `1`=0, .default = NA_real_)))

icu_recoded2 <- icu_recoded %>% mutate_at(vars(starts_with("terrw")),
                    ~(recode(., `6`=5, `5`=4, `4`=3, `3`=2, `2`=1,`1`=0, .default = NA_real_)))


# recoding inverted items

icu_inverted <- icu_recoded2 %>% mutate_at(c("ICU01", "ICU03", "ICU05",
                                                 "ICU08", "ICU13",
                                                 "ICU14", "ICU15", "ICU16",
                                                 "ICU17", "ICU19", "ICU23",
                                                 "ICU24"),
                    ~(recode(., `0`=3, `1`=2, `2`=1, `3`=0, .default = NA_real_)))


# Recoding the bullying items ---------------------------------------------

icu_rename_bully <- icu_inverted %>% rename(battery_student = terrw01,
                                       tease_student = terrw03,
                                       vandalize = terrw04,
                                       extort = terrw06,
                                       exclude = terrw12,
                                       ignore = terrw13,
                                       ridicule_teacher = terrw14,
                                       mean_teacher = terrw15,
                                       battery_teacher = terrw16)


icu_pre_imp <- icu_rename_bully %>% rename(vandalism = dbs05lj,
                                       shoplifting = dbs01lj,
                                       vehicle_theft = dbs07lj,
                                       larceny = dbs25lj,
                                       burglary = dbs20lj,
                                       battery = dbs02lj,
                                       battery_mult_perp = dbs31lj,
                                       armed_battery = dbs32lj,
                                       robbery = dbs10lj,
                                       extortion = dbs29lj,
                                       sexual_assault = dbs27lj) %>%
  mutate_at(c("vandalism", "shoplifting", "vehicle_theft", "larceny", "burglary",
              "battery", "battery_mult_perp", "armed_battery", "robbery", "extortion",
              "sexual_assault"),
            ~(recode(., `2`=1, `1`=0, .default = NA_real_)))


# Save unimputed data -----------------------------------------------------

saveRDS(icu_pre_imp, 'icu_pre_imp.RDS')






