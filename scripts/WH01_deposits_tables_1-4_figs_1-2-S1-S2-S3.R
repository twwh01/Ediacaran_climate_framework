# script to produce plots used in the manuscript Wong Hearing et al "Ediacaran
# coupling of climate and biosphere" submitted to GSA Bulletin

# clear the decks ----
rm(list = ls())


# user parameters ----
plot_age_lims <- c(650, 510) # in Ma for reverse age axis
shade_value <- 0.3
plot_show <- FALSE
plot_save <- TRUE

## set directory paths ----
dir_data_in <- file.path("data", "raw")
dir_data_out <- file.path("data", "processed")
dir_plots <- file.path("figures")

## specify file paths ----
infile_deposits_name <- "WH01_SuppData_1_deposits.xlsx"
infile_deposits <- file.path(dir_data_in, infile_deposits_name)

outfile_deposits_name <- "WH01_SuppData_1_deposits_tables.xlsx"
outfile_deposits <- file.path(dir_data_out, outfile_deposits_name)


# load packages ----
library(openxlsx2) # for opening data file
library(ggplot2) # for plotting
library(deeptime) # for plotting with GTS
library(patchwork) # for clean and easy panel plots
library(ggpubr) # for ggarrange when patchwork doesn't work so well
library(sf) # used in ggpattern
library(ggpattern) # for patterning the compilation plot
library(ggh4x) # for complex faceting
library(ggnewscale) # for complex scales
library(viridis)
library(dplyr)
library(magrittr)
library(tidyr)
library(forcats)


# load functions & palettes & themes ----
source(file = file.path("utils", "literature_glaciations_table.R")) # literature compilation of glacial intervals
source(file = file.path("utils", "ecesd_glaciations_table.R")) # ecesd glacial intervals
source(file = file.path("utils", "ecesd_deeptime_dat.R"))
source(file = file.path("utils", "custom_themes.R"))


# load data ----
## deposits dates ----
data_deposits_wb <- openxlsx2::wb_load(file = infile_deposits)

data_deposits_dates <- openxlsx2::wb_to_df(file = data_deposits_wb,
                                           sheet = "Table_S2_csl_age_constraints",
                                           na.strings = "NA")
data_deposits_dates <- data_deposits_dates[colSums(!is.na(data_deposits_dates)) > 0]

## scores data ----
data_deposits_scores <- openxlsx2::wb_to_df(file = data_deposits_wb,
                                            sheet = "Table_S1_csl_deposits",
                                            na.strings = "NA")
data_deposits_scores <- data_deposits_scores[colSums(!is.na(data_deposits_scores)) > 0]


# sort data ----
## combine dates & scores and sort by oldest age ----
data_deposits <- data_deposits_dates %>%
  dplyr::mutate(
    constraint_position = ifelse(constraint_position == "contemporaneous", "cont.", constraint_position)
  ) %>%
  dplyr::mutate(
    # order constraint types for plotting
    constraint_date_type = ordered(constraint_date_type, levels = c("minimum", "intrusion", "deposition", "eruption", "maximum")),
    date_constraint_type = paste0(constraint_type, ": ", constraint_date_type)
  ) %>%
  # remove NA values from constraint position
  # (i.e. remove dates with uncertainty regarding their stratigraphic position
  # with respect to the deposit)
  dplyr::filter(!is.na(constraint_position)) %>%
  # add likely interval from the Deposits data
  dplyr::left_join(
    ., dplyr::select(.data = data_deposits_scores, deposit_name, likely_interval, deposit_score_WH01, craton),
    by = join_by(deposit_name)
  ) %>%
  dplyr::filter(!is.na(deposit_score_WH01)) %>%
  # make likely_interval an ordered factor
  dplyr::mutate(
    likely_interval = ordered(likely_interval, levels = c(NA, "uncertain", "Cryogenian", "Ediacaran", "MEIH", "LEGH", "LEIH", "TEGH", "Cambrian")),
    # add an alpha (shade) column
    alf = ifelse(deposit_score_WH01 < 3, shade_value, 1.0)
  )

## calculate min/max age constraints ----
data_ages_minmax <- data_deposits %>%
  dplyr::filter(!is.na(constraint_age_ma)) %>%
  dplyr::select(c(
    deposit_name, deposit_score_WH01, likely_interval,
    craton, alf, reference1,
    constraint_type, constraint_position, constraint_date_type, constraint_age_ma, constraint_age_ma_unc
  )) %>%
  dplyr::group_by(
    deposit_name, deposit_score_WH01, likely_interval,
    craton, alf,
    constraint_position, constraint_date_type
  ) %>%
  dplyr::mutate(
    constraint_age_ma_unc = ifelse(
      !is.na(constraint_age_ma - constraint_age_ma_unc),
      constraint_age_ma - constraint_age_ma_unc,
      constraint_age_ma
    ),
    Age_Ma_max_unc = ifelse(
      !is.na(constraint_age_ma + constraint_age_ma_unc),
      constraint_age_ma + constraint_age_ma_unc,
      constraint_age_ma
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(deposit_name, deposit_score_WH01, likely_interval, craton, alf) %>%
  dplyr::summarise(
    # max likely age
    plot_date_max = ifelse(
      !is.infinite(min(constraint_age_ma[constraint_position == "below"], na.rm = TRUE)),
      min(constraint_age_ma[constraint_position == "below"], na.rm = TRUE),
      ifelse(
        !is.infinite(min(constraint_age_ma[constraint_position == "cont."], na.rm = TRUE)),
        min(constraint_age_ma[constraint_position == "cont."], na.rm = TRUE),
        as.numeric(NA)
      )
    ),
    # max age accounting for measurement uncertainties
    plot_date_max_unc = ifelse(
      !is.infinite(min(constraint_age_ma_unc[constraint_position == "below"], na.rm = TRUE)),
      min(constraint_age_ma_unc[constraint_position == "below"], na.rm = TRUE),
      ifelse(
        !is.infinite(min(constraint_age_ma_unc[constraint_position == "cont."], na.rm = TRUE)),
        min(constraint_age_ma_unc[constraint_position == "cont."], na.rm = TRUE),
        1000
      )
    ),
    # min likely age
    plot_date_min = ifelse(
      !is.infinite(max(constraint_age_ma[constraint_position == "above" &
        constraint_date_type != "maximum"], na.rm = TRUE)),
      max(constraint_age_ma[constraint_position == "above" &
        constraint_date_type != "maximum"], na.rm = TRUE),
      ifelse(
        !is.infinite(min(constraint_age_ma[constraint_position == "cont." &
          constraint_date_type != "maximum"], na.rm = TRUE)),
        min(constraint_age_ma[constraint_position == "cont." &
          constraint_date_type != "maximum"], na.rm = TRUE),
        as.numeric(NA)
      )
    ),
    # min age accounting for measurement uncertainties
    plot_date_min_unc = ifelse(
      !is.infinite(max(constraint_age_ma_unc[constraint_position == "above"], na.rm = TRUE)),
      max(constraint_age_ma_unc[constraint_position == "above"], na.rm = TRUE),
      0
    ),
    plot_date_range_unc = plot_date_max_unc - plot_date_min_unc,
    plot_date_range = plot_date_max - plot_date_min
  )

## order deposits by score and age constraint ----
data_deposits <- data_deposits %>%
  dplyr::mutate(
    deposit_name = ordered(
      deposit_name,
      levels = unique(data_ages_minmax$deposit_name[
        order(-data_ages_minmax$alf,
          -data_ages_minmax$deposit_score_WH01,
          data_ages_minmax$plot_date_max,
          decreasing = TRUE
        )
      ])
    )
  )

data_scores_figS2 <- data_deposits %>%
  dplyr::distinct(deposit_name, deposit_score_WH01, alf, likely_interval)


# tables 1 and 3: deposits by compilation and score ----
## sort data for tables ----
data_tables_1_4 <- data_deposits_scores %>%
  dplyr::select(
    deposit_name,
    deposit_score_WH01, deposit_score_Tindal2023,
    retallack_2022_included, retallack_2022_glaciation,
    wang_2023_included, wang_2023_glaciation,
    niu_2024_included, niu_2024_glaciation,
    youbi_2020_included
  ) %>%
  pivot_longer(
    cols = starts_with(c("retallack_2022", "wang_2023", "niu_2024", "youbi_2020")),
    names_to = c("compilation", ".value"),
    names_pattern = "(.*\\d{4})_(.*)"
  ) %>%
  # remove rows where the deposit is not included in the compilation
  dplyr::filter(
    included == "Yes"
  ) %>%
  dplyr::select(-included) %>%
  dplyr::mutate(
    compilation = case_when(
      compilation == "retallack_2022" ~ "Retallack (2022)",
      compilation == "wang_2023" ~ "Wang et al. (2023a,b)",
      compilation == "niu_2024" ~ "Niu et al. (2024)",
      compilation == "youbi_2020" ~ "Youbi et al. (2020)",
      .default = NA
    )
  )

## table 1: number of deposits by glaciation ----
# make table
data_table_1 <- data_tables_1_4 %>%
  dplyr::filter(compilation != "Youbi et al. (2020)") %>%
  dplyr::group_by(compilation) %>%
  dplyr::summarise(
    `Unique deposits` = n_distinct(deposit_name, na.rm = TRUE),
    `Gaskiers^1` = n_distinct(deposit_name[glaciation == "Gaskiers"], na.rm = TRUE),
    `Fauquier^2` = n_distinct(deposit_name[glaciation == "Fauquier"], na.rm = TRUE),
    `Bou Azzer^3` = n_distinct(deposit_name[glaciation == "Bou Azzer"], na.rm = TRUE),
    `Hankalchough^4` = n_distinct(deposit_name[glaciation == "Hankalchough"], na.rm = TRUE),
    `GEG^5` = n_distinct(deposit_name[glaciation == "GEG"], na.rm = TRUE),
    `Other/Uncertain` = n_distinct(deposit_name[glaciation == "uncertain" | is.na(glaciation)], na.rm = FALSE)
  ) %>%
  dplyr::mutate(
    compilation = ordered(
      compilation,
      levels = c(
        "Retallack (2022)",
        "Wang et al. (2023a,b)",
        "Niu et al. (2024)"
      )
    )
  ) %>%
  dplyr::arrange(compilation)

# save table to workbook
data_deposits_wb$add_worksheet("Table_1")
data_deposits_wb$add_data(sheet = "Table_1", x = data_table_1)

## table 3: deposits by compilation star rating ----
# make table
data_table_4 <- data_tables_1_4 %>%
  dplyr::group_by(compilation) %>%
  dplyr::summarise(
    Five = n_distinct(deposit_name[deposit_score_WH01 == 5], na.rm = TRUE),
    Four = n_distinct(deposit_name[deposit_score_WH01 == 4], na.rm = TRUE),
    Three = n_distinct(deposit_name[deposit_score_WH01 == 3], na.rm = TRUE),
    Two = n_distinct(deposit_name[deposit_score_WH01 == 2], na.rm = TRUE),
    One = n_distinct(deposit_name[deposit_score_WH01 == 1], na.rm = TRUE),
    Zero = n_distinct(deposit_name[deposit_score_WH01 == 0], na.rm = TRUE),
    Five_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 5], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 5], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    ),
    Four_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 4], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 4], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    ),
    Three_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 3], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 3], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    ),
    Two_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 2], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 2], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    ),
    One_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 1], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 1], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    ),
    Zero_pc = paste0(
      n_distinct(deposit_name[deposit_score_WH01 == 0], na.rm = TRUE),
      " (", round(100 * (n_distinct(deposit_name[deposit_score_WH01 == 0], na.rm = TRUE) / n_distinct(deposit_name, na.rm = TRUE)), digits = 0), " %)"
    )
  ) %>%
  # add Tindal data
  rbind(
    c(
      "Tindal (2023)",
      3, 62, 84, 41, 16, 18,
      paste0("3 (", round(100 * (3 / 224), digits = 0), " %)"),
      paste0("62 (", round(100 * (62 / 224), digits = 0), " %)"),
      paste0("84 (", round(100 * (84 / 224), digits = 0), " %)"),
      paste0("41 (", round(100 * (41 / 224), digits = 0), " %)"),
      paste0("16 (", round(100 * (16 / 224), digits = 0), " %)"),
      paste0("18 (", round(100 * (18 / 224), digits = 0), " %)")
    )
  ) %>%
  dplyr::mutate(
    compilation = ordered(
      compilation,
      levels = c("Youbi et al. (2020)", "Retallack (2022)", "Tindal (2023)", "Wang et al. (2023a,b)", "Niu et al. (2024)")
    )
  ) %>%
  dplyr::arrange(compilation)

# save table to workbook
data_deposits_wb$add_worksheet("Table_4")
data_deposits_wb$add_data(sheet = "Table_4", x = data_table_4)

## save workbook ----
openxlsx2::wb_save(data_deposits_wb, file = outfile_deposits)


# figures 1 and S1: compare compilations ----
## sort literature compilations data ----
### reduce and make long-form ----
data_compilations <- data_deposits_scores %>%
  dplyr::mutate(
    tindal_2023_glaciation = NA,
    this_study_2025_included = ifelse(!is.na(deposit_score_WH01), "Yes", "No"),
    this_study_2025_glaciation = likely_interval
  ) %>%
  dplyr::select(
    deposit_name, deposit_score_WH01, deposit_score_Tindal2023, likely_interval,
    this_study_2025_included, this_study_2025_glaciation,
    tindal_2023_included, tindal_2023_glaciation, 
    youbi_2020_included, youbi_2020_glaciation,
    retallack_2022_included, retallack_2022_glaciation,
    wang_2023_included, wang_2023_glaciation,
    niu_2024_included, niu_2024_glaciation,
  ) %>%
  dplyr::rename(original_score = deposit_score_Tindal2023) %>%
  pivot_longer(
    cols = starts_with(c("youbi_2020", "retallack_2022", "tindal_2023", "wang_2023", "niu_2024", "this_study_2025")),
    names_to = c("compilation", ".value"),
    names_pattern = "(.*\\d{4})_(.*)"
  ) %>%
  # remove rows where the deposit is not included in the compilation
  dplyr::filter(included == "Yes") %>%
  dplyr::mutate(compilation = ordered(compilation, levels = c("youbi_2020", "retallack_2022", "tindal_2023", "wang_2023", "niu_2024", "this_study_2025")))
### format for plotting ----
data_compilations_plot <- data_compilations %>%
  # remove Tindal and Youbi from the compilation
  dplyr::filter(compilation != "tindal_2023" &
    compilation != "youbi_2020") %>%
  dplyr::mutate(
    glaciation = case_when(
      glaciation == "Ediacaran" ~ "uncertain",
      glaciation == "Cryogenian" ~ "not Ediacaran",
      glaciation == "Cambrian" ~ "not Ediacaran",
      .default = glaciation
    ),
    compilation = case_when(
      compilation == "retallack_2022" ~ "Retallack 2022",
      compilation == "wang_2023" ~ "Wang et al 2023a,b",
      compilation == "niu_2024" ~ "Niu et al 2024",
      compilation == "this_study_2025" ~ "this study"
    ),
    compilation = ordered(compilation, levels = c(
      "Retallack 2022",
      "Wang et al 2023a,b",
      "Niu et al 2024",
      "this study"
    ))
  ) %>%
  # remove deposits that are "not Ediacaran" & only represented in the WH01 dataset
  dplyr::group_by(deposit_name) %>%
  dplyr::filter(any(glaciation != "not Ediacaran" & n() > 1)) %>%
  dplyr::ungroup()


## plot Figure S1 compilation glaciation age ranges ----
plot_figS1_compilation_glaciations <- literature_glaciations %>%
  ggplot(aes(
    y = Compilation,
    xmin = Age_min,
    xmax = Age_max,
    colour = glaciation_name
  )) +
  geom_linerange(
    linewidth = 10,
    stat = "identity",
    position = position_dodge(width = 0.25)
  ) +
  scale_x_reverse(limits = c(600, 538.8)) +
  scale_y_discrete(limits = levels(literature_glaciations$Compilation)) +
  scale_colour_viridis_d(
    option = "turbo",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(x = "Age (Ma)", y = "Compilation", colour = "Inferred\nglaciation") +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", size = 28 / .pt),
    axis.text = element_text(face = "plain", size = 24 / .pt),
    legend.title = element_text(face = "bold", size = 28 / .pt),
    legend.text = element_text(face = "plain", size = 24 / .pt),
    legend.position = "bottom",
    legend.justification.bottom = "right",
    legend.key.spacing = unit(0.5, "pt")
  ) +
  guides(color = guide_legend(ncol = 3))

if (isTRUE(plot_show)) {
  print(plot_figS1_compilation_glaciations)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(file = file.path(dir_plots, "fig_S1_compilation_glaciation_ranges.png"),
      width = 158.7,
      height = 150,
      units = "mm",
      bg = "white",
      res = 1200)
  print(plot_figS1_compilation_glaciations)
  dev.off()
  
  # pdf
  pdf(file = file.path(dir_plots, "fig_S1_compilation_glaciation_ranges.pdf"),
      width = 6.25, # in inches
      height = 5.91,
      bg = "white")
  print(plot_figS1_compilation_glaciations)
  dev.off()
}

## plot Figure 1 deposits by compilation score and glaciation ----
plot_fig1_compilation_deposits <- data_compilations_plot %>%
  dplyr::mutate(glaciation = dplyr::case_when(glaciation == "All" ~ "uncertain", .default = glaciation),
                glaciation = ordered(glaciation,
                                     levels = c("TEGH",
                                                "LEIH",
                                                "MEIH",
                                                "Hankalchough",
                                                "Bou Azzer",
                                                "Fauquier",
                                                "Gaskiers",
                                                "GEG",
                                                "uncertain",
                                                "not Ediacaran")),
                likely_interval = fct_rev(ordered(likely_interval,
                                                  levels = c(NA, 
                                                             "Cambrian", 
                                                             "Cryogenian", 
                                                             "uncertain",
                                                             "Ediacaran",
                                                             "MEIH",
                                                             "LEIH",
                                                             "TEGH"))),
                pattern_spacing = case_when(deposit_score_WH01 > 2 ~ ">2 star",
                                            .default = "≤2 star")) %>%
  dplyr::group_by(likely_interval) %>%
  dplyr::arrange(likely_interval, desc(deposit_score_WH01), glaciation, .by_group = TRUE) %>%
  dplyr::mutate(deposit_name = forcats::as_factor(deposit_name)) %>%
  ggplot(aes(x = deposit_name,
             y = compilation,
             fill = glaciation,
             pattern_spacing = pattern_spacing)) +
  geom_tile_pattern(colour = "grey25",
                    pattern_size = 0.1,
                    pattern = "stripe",
                    pattern_angle = 45,
                    pattern_fill = "white",
                    pattern_colour = "white") +
  scale_x_discrete(position = "top", limits = rev) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_pattern_spacing_discrete(breaks = c(">2 star", "≤2 star"), range = c(5, 0.01)) +
  scale_fill_viridis_d(option = "turbo", na.value = "grey50") +
  labs(x = "Deposit name",
       y = "Compilation",
       fill = "Interval",
       pattern_spacing = "Star rating") +
  theme_graph +
  theme(plot.margin = margin(1, 20, 1, 1, "pt"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 28 / .pt),
        axis.text.y = element_text(size = 24 / .pt),
        axis.text.x = element_text(size = 14 / .pt, angle = 60, hjust = 0, vjust = 0),
        legend.title = element_text(size = 24 / .pt),
        legend.text = element_text(size = 20 / .pt),
        legend.box = "vertical",
        legend.position = "bottom",
        legend.justification.bottom = "right",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(override.aes = list(pattern_spacing = 5)),
         pattern_spacing = guide_legend(nrow = 1))

if (isTRUE(plot_show)) {
  print(plot_fig1_compilation_deposits)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(file = file.path(dir_plots, "fig_1_compilation_deposits_score.png"),
      width = 165.1,
      height = 140,
      units = "mm",
      bg = "white",
      res = 1200)
  print(plot_fig1_compilation_deposits)
  dev.off()
  
  # pdf 
  pdf(file = file.path(dir_plots, "fig_1_compilation_deposits_score.pdf"),
      width = 6.5,
      height = 5.51,
      bg = "white")
  print(plot_fig1_compilation_deposits)
  dev.off()
}


# figure 2 deposit correlation and scores ----
## sort data ----
data_deposits_fig2 <- data_deposits %>%
  dplyr::ungroup() %>%
  dplyr::select(deposit_name,
                deposit_score_WH01,
                likely_interval,
                alf,
                constraint_wrt_csl,
                constraint_position,
                constraint_type,
                constraint_date_type,
                constraint_date_system,
                constraint_age_ma,
                constraint_age_ma_unc) %>%
  dplyr::filter(
    # remove entries with no age constraint (shouldn't be any in the dataset but just to be sure)
    !is.na(constraint_age_ma),
    # remove dates beyond plot age limits
    (constraint_age_ma < max(plot_age_lims, na.rm = TRUE) &
      constraint_age_ma > min(plot_age_lims, na.rm = TRUE)),
    # remove any entries that are likely Cryogenian
    likely_interval != "Cryogenian",
    # remove any entries with Rb-Sr whole-rock dates as these are not considered reliable here
    (is.na(constraint_date_system) | constraint_date_system != "Rb-Sr"),
    !is.na(constraint_wrt_csl)
  ) %>%
  dplyr::mutate(
    # merge "Cambrian" with "uncertain" deposits
    likely_interval = ordered(case_when(likely_interval == "Cambrian" ~ "uncertain",
                                        .default = likely_interval),
                              levels = c("uncertain", "Ediacaran", "MEIH", "LEGH", "LEIH", "TEGH")),
    # add a new variable for colour in the plots reflecting the dating method
    plot_colour_date_type = ordered(case_when(constraint_type == "Radiometric date" ~ paste0("Radiometric: ", constraint_date_system),
                                              # constraint_type == "CIE" ~ "Shuram CIE",
                                              .default = constraint_type),
                                    levels = c("Radiometric: U-Pb", "Radiometric: Re-Os", "Radiometric: Pb-Pb", "Radiometric: Ar-Ar",
                                               "Biostratigraphy", "Correlation",
                                               # "Shuram CIE"
                                               "CIE")),
    plot_shape_constraint = ordered(constraint_wrt_csl,
                                    levels = c("maximum", "deposition", "minimum")))

# recalculate min/max age constraints
data_ages_minmax_fig2 <- data_deposits_fig2 %>%
  # calculate each age with analytical uncertainty where relevant
  dplyr::mutate(age_ma_min_unc = ifelse(!is.na(constraint_age_ma - constraint_age_ma_unc),
                                        constraint_age_ma - constraint_age_ma_unc,
                                        constraint_age_ma),
                age_ma_max_unc = ifelse(!is.na(constraint_age_ma + constraint_age_ma_unc),
                                        constraint_age_ma + constraint_age_ma_unc,
                                        constraint_age_ma)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(deposit_name,
                  deposit_score_WH01,
                  likely_interval,
                  alf) %>%
  dplyr::summarise(plot_date_max = dplyr::case_when(
    !is.infinite(min(constraint_age_ma[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE)) ~ min(constraint_age_ma[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE),
    .default = 1000),
    plot_date_max_unc = dplyr::case_when(
      !is.infinite(min(age_ma_max_unc[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE)) ~ min(age_ma_max_unc[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE),
      .default = 1000),
    plot_date_min = dplyr::case_when(
      !is.infinite(max(constraint_age_ma[plot_shape_constraint == "minimum"], na.rm = TRUE)) ~ max(constraint_age_ma[plot_shape_constraint == "minimum"], na.rm = TRUE),
      .default = 0),
    plot_date_min_unc = dplyr::case_when(
      !is.infinite(max(age_ma_min_unc[plot_shape_constraint == "minimum"], na.rm = TRUE)) ~ max(age_ma_min_unc[plot_shape_constraint == "minimum"], na.rm = TRUE),
      .default = 0),
    .groups = "keep")

## order deposits by maximum age constraint ----
data_deposits_fig2 <- data_deposits_fig2 %>%
  dplyr::mutate(deposit_name = ordered(
    deposit_name,
    levels = unique(data_ages_minmax$deposit_name[order(-data_ages_minmax$alf,
                                                        -data_ages_minmax$deposit_score_WH01,
                                                        data_ages_minmax$plot_date_max,
                                                        decreasing = TRUE)])))

data_scores_fig2 <- data_deposits_fig2 %>%
  dplyr::distinct(deposit_name, deposit_score_WH01, alf, likely_interval)

### plot figure 2 scores ----
plot_fig2_deposit_scores <- ggplot(data = data_scores_fig2,
                                   aes(x = deposit_score_WH01, y = deposit_name)) +
  geom_col(aes(alpha = alf),
           fill = "steelblue") +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "grey50") +
  scale_alpha_identity(guide = "none") +
  scale_y_discrete(name = "Candidate glacial deposits", expand = expansion(add = 0.45)) +
  scale_x_reverse(name = "Star\nrating",
                  limits = c(5, 0),
                  breaks = seq(from = 5, to = 0, by = -1),
                  expand = expansion(add = c(0.1, 0.1))) +
  facet_grid(fct_rev(likely_interval) ~ .,
             scales = "free_y",
             space = "free_y",
             drop = TRUE) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(
          angle = 0,
          vjust = 0.3,
          hjust = 1,
          size = 6),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0.5), "cm"))

if (isTRUE(plot_show)) {
  print(plot_fig2_deposit_scores)
}

### plot figure 2 dates ----
plot_fig2_deposit_dates <- ggplot() +
  # annotate with icehouse intervals
  geom_rect(data = ecesd_glaciations,
            aes(xmin = Age_min,
                xmax = Age_max,
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.3,
            fill = "steelblue",
            colour = "steelblue") +
  # annotate with E-C boundary
  geom_vline(xintercept = c(538.8, 635),
             linetype = "dashed",
             colour = "grey50") +
  # add solid line between min and max age ranges
  geom_linerange(data = data_ages_minmax_fig2,
                 aes(y = deposit_name,
                     xmin = plot_date_min_unc,
                     xmax = plot_date_max_unc,
                     alpha = alf),
                 position = "identity",
                 linetype = "solid",
                 linewidth = 1,
                 colour = "black") +
  # add error bars for geom_point
  # geom_errorbarh(
  #   data = data_deposits_fig2,
  #   aes(
  #     # x = constraint_age_ma,
  #     y = deposit_name,
  #     xmin = constraint_age_ma - constraint_age_ma_unc,
  #     xmax = constraint_age_ma + constraint_age_ma_unc,
  #     colour = plot_colour_date_type,
  #     alpha = alf,
  #     group = deposit_name
  #   ),
  #   position = position_dodge(width = 1),
  #   linewidth = 0.5
  # ) +
  geom_linerange(data = data_deposits_fig2,
                 aes(x = constraint_age_ma,
                     y = deposit_name,
                     xmin = constraint_age_ma - constraint_age_ma_unc,
                     xmax = constraint_age_ma + constraint_age_ma_unc,
                     colour = plot_colour_date_type,
                     alpha = alf,
                     group = deposit_name),
                 # position = position_dodge2(width = 0.75),
                 linewidth = 0.5) +
  # add individual dates as points
  geom_point(data = data_deposits_fig2,
             aes(x = constraint_age_ma,
                 y = deposit_name,
                 shape = plot_shape_constraint,
                 colour = plot_colour_date_type,
                 alpha = alf,
                 group = deposit_name),
             # position = position_dodge2(width = 0.75),
             size = 3,
             stroke = 1,) +
  # add an empty geom_col to align with score columns when panel plotting
  geom_col(data = data_deposits_fig2, aes(x = 0, y = deposit_name)) +
  scale_shape_manual(values = c("maximum" = -9658, # 24,
                                "deposition" = 18,
                                "minimum" = -9668 # 25
                                ),     
                     na.value = NA,
                     breaks = c("maximum", "deposition", "minimum", NA)) +
  scale_colour_viridis_d(option = "turbo",
                         begin = 0.1,
                         end = 0.9,
                         direction = -1,
                         na.value = "grey25") +
  scale_alpha_identity(guide = "none") +
  # add scale_x expansion to allow alignment with geom_col below
  scale_y_discrete(expand = expansion(add = 2)) +
  scale_x_reverse(
    breaks = seq(from = min(plot_age_lims - 10), to = max(plot_age_lims), by = 20),
    expand = expansion(add = 2)
  ) +
  labs(x = "Age (Ma)",
       y = "Candidate glacial deposits",
       shape = "Date position",
       colour = "Date type") +
  coord_geo(pos = "top",
            height = unit(1, "line"),
            dat = ECESD_dt_MS1F1,
            xlim = plot_age_lims) +
  facet_grid(fct_rev(likely_interval) ~ .,
             scales = "free_y",
             space = "free_y",
             drop = TRUE) +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(nrow = 3, order = 2) # byrow = TRUE
         ) +
  theme_bw() +
  # theme edited to work with panel plot
  # remove x-axis details and sort plot margins
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length.y = unit(0, "cm"),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.y = element_text(size = 6, angle = 0, hjust = 0, colour = "black", face = "bold"),
        plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.box.spacing = unit(1, "pt"),
        legend.position = "bottom",
        legend.justification.bottom = "left",
        legend.spacing.x = unit(0, "pt"),
        legend.spacing.y = unit(0, "pt"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6, margin = margin(0, 0, 0, 0)),
        legend.margin = margin(0, 0, 0, 0, unit = "pt"))

# have a check
if (isTRUE(plot_show)) {
  print(plot_fig2_deposit_dates)
}

### combine figure 2 as panel plot ----
plot_fig2_panel <- ggarrange(plot_fig2_deposit_scores,
                             plot_fig2_deposit_dates,
                             ncol = 2,
                             align = "h",
                             widths = c(0.5, 1))

if (isTRUE(plot_show)) {
  plot_fig2_panel
}

if (isTRUE(plot_save)) {
  # hi-res png 
  png(file = file.path(dir_plots, "fig_2_deposits_dates_scores.png"),
      width = 159,
      height = 210,
      units = "mm",
      bg = "white",
      res = 1200)
  print(plot_fig2_panel)
  dev.off()
  
  # pdf
  pdf(file = file.path(dir_plots, "fig_2_deposits_dates_scores.pdf"),
      width = 6.26,
      height = 8.27,
      bg = "white")
  print(plot_fig2_panel)
  dev.off()
}


# figures S2 and S3: deposit correlation and scores ----
## sort data for figures S2 and S3 radiometric date distributions ----
### subset ----
data_for_pdf <- data_deposits_fig2 %>%
  dplyr::left_join(.,
                   unique(data_deposits[c("deposit_name", "craton")]),
                   by = join_by(deposit_name)) %>%
  dplyr::filter(!is.na(constraint_age_ma),
                !is.na(constraint_age_ma_unc),
                constraint_type == "Radiometric date",
                deposit_score_WH01 > 2,
                likely_interval == "MEIH" |
                  likely_interval == "LEIH") %>%
  dplyr::select(deposit_name,
                deposit_score_WH01,
                craton,
                likely_interval,
                constraint_wrt_csl,
                constraint_age_ma,
                constraint_age_ma_unc) %>%
  dplyr::mutate(
    date_id = paste0(deposit_name, "_", constraint_age_ma, "_", constraint_age_ma_unc)
  )

### make pdfs ----
age_range <- seq(from = 500, to = 650, by = 0.1)
data_dates_pdfs <- as.data.frame(age_range, col.names = "age_ma")

for (i in 1:nrow(data_for_pdf)) {
  y <- dnorm(x = age_range, mean = data_for_pdf$constraint_age_ma[i], data_for_pdf$constraint_age_ma_unc[i])
  data_dates_pdfs[paste0(data_for_pdf$date_id[i])] <- y
}

### format for plots ----
data_dates_pdfs_figS2 <- data_dates_pdfs %>%
  tidyr::pivot_longer(.,
                      cols = !age_range,
                      names_to = "date_id",
                      values_to = "date_pdf") %>%
  dplyr::filter(date_pdf > 0) %>%
  dplyr::left_join(.,
                   data_for_pdf,
                   by = join_by(date_id)) %>%
  dplyr::mutate(likely_interval = as.character(likely_interval),
                constraint_wrt_csl = ordered(
                  constraint_wrt_csl,
                  levels = c("maximum", "deposition", "minimum")))

data_intervals <- data.frame(likely_interval = c("MEIH", "LEIH"),
                             xmin = c(579, 550),
                             xmax = c(595, 565),
                             ymin = -Inf,
                             ymax = Inf) %>%
  dplyr::right_join(.,
                    unique(droplevels(data_for_pdf[c("deposit_name", "likely_interval", "craton", "constraint_age_ma")])),
                    by = join_by(likely_interval))


## plot figure S2 date pdfs by deposit ----
plot_figS2_radiometric_constraints_deposit <- ggplot() +
  theme_minimal() +
  theme(axis.title.x = element_text(face = "bold", size = 28 / .pt),
        axis.text.x = element_text(face = "plain", size = 24 / .pt),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour = NA),
        ggh4x.facet.nestline = element_line(colour = "grey25"),
        legend.title = element_text(face = "bold", size = 28 / .pt),
        legend.text = element_text(face = "plain", size = 24 / .pt),
        legend.position = "bottom",
        legend.justification.bottom = "right") +
  scale_x_reverse(limits = plot_age_lims, name = "Age (Ma)") +
  scale_y_continuous() +
  deeptime::coord_geo(height = unit(1, "line"), size = 8 / .pt) +
  facet_nested(ordered(likely_interval, levels = c("LEIH", "MEIH")) +
                 forcats::fct_reorder(ordered(deposit_name), constraint_age_ma, .fun = max) ~ .,
               switch = "y",
               scales = "free_y",
               strip = strip_nested(size = "variable", # as interval names are shorter than deposit names
                                    text_y = list(
                                      element_text(face = "bold", angle = 90, size = 28 / .pt),
                                      element_text(face = "plain", angle = 0, size = 24 / .pt, hjust = 1)),
                                    by_layer_y = TRUE)) +
  geom_rect_pattern(data = data_intervals,
                    aes(xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        pattern_angle = likely_interval),
                    pattern = "stripe",
                    pattern_fill = "white",
                    pattern_colour = NA,
                    pattern_spacing = 0.5,
                    colour = NA,
                    fill = "slategray2") +
  scale_pattern_angle_discrete(name = "Intervals:", breaks = c("MEIH", "LEIH"), range = c(45, 135)) +
  guides(pattern_angle = guide_legend(override.aes = list(pattern_spacing = 0.03))) +
  ggnewscale::new_scale_colour() +
  geom_hline(yintercept = 0, colour = "grey10") +
  geom_area(data = data_dates_pdfs_figS2,
            aes(x = age_range,
                y = date_pdf,
                colour = date_id,
                fill = constraint_wrt_csl),
            alpha = 0.5,
            position = "identity",
            orientation = "x") +
  scale_colour_manual(values = rep("black", nrow(data_dates_pdfs_figS2)), guide = "none") +
  ggnewscale::new_scale_colour() +
  geom_vline(data = data_dates_pdfs_figS2,
             aes(xintercept = constraint_age_ma, colour = constraint_wrt_csl),
             linewidth = 2) +
  scale_colour_viridis_d(option = "turbo",
                         begin = 0,
                         end = 0.9,
                         name = "Constraint:",
                         aesthetics = c("colour", "fill"))

if (isTRUE(plot_show)) {
  print(plot_figS2_radiometric_constraints_deposit)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(file = file.path(dir_plots, "fig_S2_radiometric_constraints_distribution_deposits.png"),
      width = 158.7,
      height = 180,
      units = "mm",
      bg = "white",
      res = 1200)
  print(plot_figS2_radiometric_constraints_deposit)
  dev.off()
  
  # pdf
  pdf(file = file.path(dir_plots, "fig_S2_radiometric_constraints_distribution_deposits.pdf"),
      width = 6.25,
      height = 7.09,
      bg = "white")
  print(plot_figS2_radiometric_constraints_deposit)
  dev.off()
}

## plot figure S3 date pdfs by craton ----
plot_figS3_radiometric_constraints_craton <- ggplot() +
  theme_minimal() +
  theme(axis.title.x = element_text(face = "bold", size = 28 / .pt),
        axis.text.x = element_text(face = "plain", size = 24 / .pt),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour = NA),
        ggh4x.facet.nestline = element_line(colour = "grey25"),
        legend.title = element_text(face = "bold", size = 28 / .pt),
        legend.text = element_text(face = "plain", size = 24 / .pt),
        legend.position = "bottom",
        legend.justification.bottom = "right") +
  scale_x_reverse(limits = plot_age_lims, name = "Age (Ma)") +
  scale_y_continuous() +
  deeptime::coord_geo(height = unit(1, "line"), size = 8 / .pt) +
  facet_nested(ordered(likely_interval, levels = c("LEIH", "MEIH")) +
                 forcats::fct_reorder(ordered(craton), constraint_age_ma, .fun = max) ~ .,
               switch = "y",
               scales = "free_y",
               strip = strip_nested(size = "variable", # as interval names are shorter than deposit names
                                    text_y = list(element_text(face = "bold", angle = 90, size = 28 / .pt),
                                                  element_text(face = "plain", angle = 0, size = 24 / .pt, hjust = 1)),
                                    by_layer_y = TRUE)) +
  geom_rect_pattern(data = data_intervals,
                    aes(xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        pattern_angle = likely_interval),
                    pattern = "stripe",
                    pattern_fill = "white",
                    pattern_colour = NA,
                    pattern_spacing = 0.5,
                    colour = NA,
                    fill = "slategray2") +
  scale_pattern_angle_discrete(name = "Intervals:", breaks = c("MEIH", "LEIH"), range = c(45, 135)) +
  guides(pattern_angle = guide_legend(override.aes = list(pattern_spacing = 0.03))) +
  ggnewscale::new_scale_colour() +
  geom_hline(yintercept = 0, colour = "grey10") +
  geom_area(data = data_dates_pdfs_figS2,
            aes(x = age_range,
                y = date_pdf,
                colour = date_id,
                fill = constraint_wrt_csl),
            alpha = 0.5,
            position = "identity",
            orientation = "x") +
  scale_colour_manual(values = rep("black", nrow(data_dates_pdfs_figS2)), guide = "none") +
  ggnewscale::new_scale_colour() +
  geom_vline(data = data_dates_pdfs_figS2,
             aes(xintercept = constraint_age_ma, colour = constraint_wrt_csl),
             linewidth = 2) +
  scale_colour_viridis_d(option = "turbo",
                         begin = 0,
                         end = 0.9,
                         name = "Constraint:",
                         aesthetics = c("colour", "fill"))

if (isTRUE(plot_show)) {
  print(plot_figS3_radiometric_constraints_craton)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(file = file.path(dir_plots, "fig_S3_radiometric_constraints_distribution_craton.png"),
      width = 158.7,
      height = 150,
      units = "mm",
      bg = "white",
      res = 1200)
  print(plot_figS3_radiometric_constraints_craton)
  dev.off()
  
  # pdf
  pdf(file = file.path(dir_plots, "fig_S3_radiometric_constraints_distribution_craton.pdf"),
      width = 6.25,
      height = 5.91,
      bg = "white")
  print(plot_figS3_radiometric_constraints_craton)
  dev.off()
}


# END ----
