# dependencies of the Ediacaran climate framework R project
# running this script (e.g. through RStudio) will install the packages needed 
# for this Rproj
to_install <- c(
  "magrittr",
  "dplyr", 
  "tibble", 
  "tidyr", 
  "forcats", 
  "ggplot2",
  "ggpubr",
  "ggnewscale",
  "ggh4x",
  "sf",
  "ggpattern",
  "patchwork",
  "viridis",
  "openxlsx2", 
  "deeptime",
  "divDyn"
  )
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}
