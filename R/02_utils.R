#source("R/00_dependencies.R")

# Defining commonly used objects ------------------------------------------

# Item sets ---------------------------------------------------------------


# Drop filter variables ---------------------------------------------------

relevant_variables <- c("lfdnr", "age", "gender",
"vandalism", "shoplifting", "vehicle_theft", "larceny", "burglary","battery",
"battery_mult_perp", "armed_battery", "robbery", "extortion", "sexual_assault",
"ICU01", "ICU02", "ICU03", "ICU04","ICU05", "ICU06", "ICU07", "ICU08", "ICU09",
"ICU10", "ICU11", "ICU12", "ICU13", "ICU14", "ICU15", "ICU16", "ICU17", "ICU18",
"ICU19", "ICU20", "ICU21", "ICU22", "ICU23", "ICU24", "battery_student",
"tease_student", "vandalize", "extort", "exclude", "ignore", 'ridicule_teacher',
"mean_teacher", "battery_teacher")

# ICU Item wording ------------------------------------------------------------


icu_item_content <- c("I express my feelings openly (-)", #1
                      "What I think is right and wrong is different from what other people think", #2
                      "I care about how well I do at school or work (-)", #3
                      "I do not care who I hurt to get what I want", #4
                      "I feel bad or guilty when I do something wrong (-)", #5
                      "I do not show my emotions to others", #6
                      "I do not care about being on time", #7
                      "I am concerned about the feelings of others (-)", #8
                      "I do not care if I get into trouble", #9
                      "I do not let my feelings control me", #10
                      "I do not care about doing things well", #11
                      "I seem very cold and uncaring to others", #12
                      "I easily admit to being wrong (-)", #13
                      "It is easy for others to tell how I am feeling (-)", #14
                      "I always try my best (-)", #15
                      "I apologize ('say I am sorry') to persons I hurt (-)", #16
                      "I try not to hurt others' feelings (-)", #17
                      "I do not feel remorseful when I do something wrong", #18
                      "I am very expressive and emotional (-)", #19
                      "I do not like to put the time into doing things well", #20
                      "The feelings of others are unimportant to me", #21
                      "I hide my feelings from others", #22
                      "I work hard on everyting I do (-)", #23
                      "I do things to make others feel good (-)") #24


# ICU Item selection -------------------------------------------------------

icu_items <- c("ICU01", "ICU02", "ICU03", "ICU04", "ICU05",
  "ICU06", "ICU07", "ICU08", "ICU09", "ICU10", "ICU11", "ICU12",
  "ICU13", "ICU14", "ICU15", "ICU16", "ICU17", "ICU18", "ICU19",
  "ICU20", "ICU21", "ICU22", "ICU23", "ICU24")

icu_names <- c("ICU01", "ICU02", "ICU03", "ICU04", "ICU05", "ICU06", "ICU07",
              "ICU08", "ICU09", "ICU10", "ICU11", "ICU12", "ICU13", "ICU14",
              "ICU15", "ICU16", "ICU17", "ICU18", "ICU19", "ICU20", "ICU21",
              "ICU22", "ICU23", "ICU24")

# ICU Subscales corresponding to LPE specifier ----------------------------

lpe <- c("unemotional", "callous", "careless", "uncaring",
         "callous", "unemotional", "careless", "uncaring",
         "callous", "unemotional", "careless", "uncaring",
         "callous", "unemotional", "careless", "callous",
         "uncaring", "callous", "unemotional", "careless",
         "uncaring", "unemotional", "careless", "uncaring")


lpe_groups <- list(uncaring = c(4,8,12,17,21,24),
                   unemotional = c(1,6,10,14,19,22),
                   callous = c(2,5,9,13,16,18),
                   careless = c(3,7,11,15,20,23))

lpe_df <- data.frame(id = icu_names,
                     factor = lpe,
                     stringsAsFactors = FALSE)

# Anti social behaviour variables -----------------------------------------

anti_social_vars <- c("vandalism", "shoplifting", "vehicle_theft",
                  "larceny", "burglary", "battery", "battery_mult_perp",
                  "armed_battery", "robbery", "extortion", "sexual_assault")


# Covariate Network node names --------------------------------------------

cov_net_names <- c("battery_student", "tease_student", "vandalize", "extort",
           "exclude", "ignore", "mean_comp_teacher", "battery_teacher", "uncaring", "unemotional",
           "callous", "careless")

