# Dependencies ------------------------------------------------------------


# Required packages -------------------------------------------------------


package_list <- list("foreign",
                     "haven",
                     "tidyverse",
                     "mice",
                     "psych",
                     "psychTools",
                     "kableExtra",
					 "xtable",
                     "bootnet",
                     "networktools",
					 "NetworkToolbox",
                     "igraph",
                     "ggplot2",
                     "patchwork",
                     "RColorBrewer")


# Conditional installer ---------------------------------------------------

conditional_install_and_load <- function(package_name){
  if(! package_name %in% rownames(installed.packages())){
    install.packages(package_name)
  }

  library(package_name, character.only = TRUE)
}

lapply(package_list, conditional_install_and_load)

# library(tidyverse)
# library(mice)
# library(psych)
# library(psychTools)
# library(papaja)
# library(knitr)
# library(kableExtra)
# library(patchwork)
