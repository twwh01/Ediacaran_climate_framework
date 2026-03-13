# calculating faunal compositional similarity
# clear the decks ----
rm(list=ls())


# user parameters ----
## set directory paths ----
dir_data_in <- file.path("data", "raw")
dir_data_out <- file.path("data", "processed")
dir_plots <- file.path("figures")

## specify files ----
infile_palaeo_name <- "WH01_SuppData_2_paleontology.xlsx"
infile_palaeo <- file.path(dir_data_in, infile_palaeo_name)

outfile_palaeo_name <- "WH01_SuppData_2_palaeontology_tables.xlsx"
outfile_palaeo <- file.path(dir_data_out, outfile_palaeo_name)


# load packages ----
library(magrittr)
library(dplyr)
library(openxlsx2)


# load data ----
data_palaeo_wb <- openxlsx2::wb_load(file = infile_palaeo)
data_palaeo_in <- openxlsx2::wb_to_df(
  file = data_palaeo_wb,
  sheet = "palaeo_data",
  na.strings = "NA"
)
data_palaeo_in <- data_palaeo_in[colSums(!is.na(data_palaeo_in)) > 0]


# select data ----
## all data ----
### taxon (species) ----
data_palaeo <- data_palaeo_in %>%
  # remove assemblages
  # remove Cambrian data for main plots
  dplyr::filter(Rank1_supraphylum != "assemblage",
                associated_assemblage != "Cambrian", 
                Fossil_type != "ichnofossil"
  ) %>%
  # make unified age model 
  dplyr::mutate(
    plot_age = dplyr::case_when(
      !is.na(Age_mean_TWWH) ~ Age_mean_TWWH, 
      !is.na(Age_mean_Matthews2020) ~ Age_mean_Matthews2020, 
      !is.na(Age_mean_BowyerK) ~ Age_mean_BowyerK, 
      .default = NA
    ), 
    plot_age_max = dplyr::case_when(
      !is.na(Age_max_TWWH) ~ Age_max_TWWH, 
      !is.na(Age_max_Matthews2020) ~ Age_max_Matthews2020, 
      !is.na(Age_max_BowyerK) ~ Age_max_BowyerK, 
      .default = NA
    ), 
    plot_age_min = dplyr::case_when(
      !is.na(Age_min_TWWH) ~ Age_min_TWWH, 
      !is.na(Age_min_Matthews2020) ~ Age_min_Matthews2020, 
      !is.na(Age_min_BowyerK) ~ Age_min_BowyerK, 
      .default = NA
    ), 
    
    Rank5_genus = fct_reorder(
      as.factor(Rank5_genus),
      plot_age,
      .fun = max,
      .desc = TRUE,
      .na_rm = TRUE
    )
    
  ) %>% 
  dplyr::filter(!is.na(plot_age))


### genera ----
data_palaeo_genera <- data_palaeo %>%
  dplyr::filter(!is.na(Rank5_genus))

genera_avalon_565 <- data_palaeo_genera %>% dplyr::filter(plot_age >= 565) %>% pull(Rank5_genus) %>% unique()
genera_whitesea_565 <- data_palaeo_genera %>% dplyr::filter(plot_age < 565 & plot_age >= 550) %>% pull(Rank5_genus) %>% unique()
genera_avalon_560 <- data_palaeo_genera %>% dplyr::filter(plot_age >= 560) %>% pull(Rank5_genus) %>% unique()
genera_whitesea_560 <- data_palaeo_genera %>% dplyr::filter(plot_age < 560 & plot_age >= 550) %>% pull(Rank5_genus) %>% unique()
genera_nama <- data_palaeo_genera %>% dplyr::filter(plot_age < 550 & plot_age >= 538.8) %>% pull(Rank5_genus) %>% unique()

leih_meih_565 <- list(genera_avalon_565, genera_whitesea_565, genera_nama)
leih_meih_560 <- list(genera_avalon_560, genera_whitesea_560, genera_nama)


# Jaccard similarity function ----
jaccard_similarity <- function(x, y) {
  intersection = length(intersect(x, y))
  union = length(x) + length(y) - intersection
  return(
    intersection / union
  )
}

# Sorensen coefficient ----
sorensen_similarity <- function(x, y) {
  intersection <- length(intersect(x, y))
  return(
    (2 * intersection)/(length(x) + length(y))
  )
}

# Simpson coefficient ----
simpson_similarity <- function(x, y) {
  intersection <- length(intersect(x, y))
  n_min <- min(length(x), length(y))
  return(
    intersection / n_min
  )
}

# calculate similarities ----
data_table_s2 <- tibble::tibble(
  "LEIH-MEIH age" = c(
    "565 Ma", 
    "565 Ma", 
    "565 Ma", 
    "560 Ma", 
    "560 Ma", 
    "560 Ma"
  ), 
  "Assemblages" = c(
    "Avalon-White Sea", 
    "White Sea-Nama", 
    "Avalon-Nama",
    "Avalon-White Sea", 
    "White Sea-Nama", 
    "Avalon-Nama"
  ), 
  "Jaccard index" = round(
    c(
      jaccard_similarity(genera_avalon_565, genera_whitesea_565),
      jaccard_similarity(genera_whitesea_565, genera_nama),
      jaccard_similarity(genera_avalon_565, genera_nama),
      jaccard_similarity(genera_avalon_560, genera_whitesea_560),
      jaccard_similarity(genera_whitesea_560, genera_nama),
      jaccard_similarity(genera_avalon_560, genera_nama)
    ),
    digits = 3
  ), 
  "Sorensen index" = round(
    c(
      sorensen_similarity(genera_avalon_565, genera_whitesea_565),
      sorensen_similarity(genera_whitesea_565, genera_nama),
      sorensen_similarity(genera_avalon_565, genera_nama),
      sorensen_similarity(genera_avalon_560, genera_whitesea_560),
      sorensen_similarity(genera_whitesea_560, genera_nama),
      sorensen_similarity(genera_avalon_560, genera_nama)
    ), 
    digits = 3
  ),
  "Simpson index" = round(
    c(
      simpson_similarity(genera_avalon_565, genera_whitesea_565),
      simpson_similarity(genera_whitesea_565, genera_nama),
      simpson_similarity(genera_avalon_565, genera_nama),
      simpson_similarity(genera_avalon_560, genera_whitesea_560),
      simpson_similarity(genera_whitesea_560, genera_nama),
      simpson_similarity(genera_avalon_560, genera_nama)
    ), 
    digits = 3
  )
)


# save data ----
data_palaeo_wb$add_worksheet("Table_S2")
data_palaeo_wb$add_data(sheet = "Table_S2", x = data_table_s2)
openxlsx2::wb_save(data_palaeo_wb, file = outfile_palaeo)


# END ----
