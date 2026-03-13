# script to produce plots used in the manuscript Wong Hearing et al "Ediacaran 
# coupling of climate and biosphere" submitted to GSA Bulletin

# clear the decks ----
rm(list=ls())


# user parameters ----
plot_age_lims <- c(650, 510) # in Ma for reverse age axis
shade_value <- 0.3
plot_show <- TRUE
plot_save <- TRUE

## set directory paths ----
dir_data <- file.path("..", "Data")
dir_plots <- file.path("plots", "WH01_GSABull")

## dates ----
indate <- "20250530"
outdate <- "20250610"

## specify file paths ----
infile_deposits <- file.path(dir_data, paste0(indate, "_WH01_Data_S1_deposits.xlsx"))
infile_palaeo <- file.path(dir_data, paste0(indate, "_WH01_Data_S2_palaeontology.xlsx"))


# load packages ----
library(openxlsx2) # for opening data file
library(ggplot2) # for plotting
library(deeptime) # for plotting with GTS
library(sf) # for handling spatial data
library(rgplates) # for palaeorotations
library(ggforce) # for geom_sina()
library(patchwork) # for clean and easy panel plots
library(ggpubr) # for when patchwork doesn't work so well
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
source(file = "literature_glaciations_table.R") # literature compilation of glacial intervals
source(file = "ecesd_glaciations_table.R") # ecesd glacial intervals
source(file = file.path("functions", "map_for_deposits.R"))


# load palettes and themes ----
## palettes ----
source(file = file.path("functions", "ECESD_deeptime_dat.R"))

## graph themes ----
source(file = file.path("custom_palettes", "custom_themes.R"))


# load data ----
## deposits data ----
data_deposits_wb <- openxlsx2::wb_load(
  file = infile_deposits
  )

data_deposits_dates <- openxlsx2::wb_to_df(
  file = data_deposits_wb,
  sheet = "csl_age_constraints",
  na.strings = "NA"
)
data_deposits_dates <- data_deposits_dates[colSums(!is.na(data_deposits_dates)) > 0]

## scores data ----
data_deposits_scores <- openxlsx2::wb_to_df(
  file = data_deposits_wb,
  sheet = "csl_deposits",
  na.strings = "NA"
)
data_deposits_scores <- data_deposits_scores[colSums(!is.na(data_deposits_scores)) > 0]


# sort data ----
## combine dates & scores and sort by oldest age ----
data_deposits <- data_deposits_dates %>%
  dplyr::mutate(
    constraint_position = ifelse(constraint_position == "contemporaneous",
                                 "cont.",
                                 constraint_position)
  ) %>%
  dplyr::mutate(
    ## order constraint types for plotting
    constraint_date_type = ordered(
      constraint_date_type,
      levels = c("minimum", "intrusion", "deposition", "eruption", "maximum")
    ),
    date_constraint_type = paste0(constraint_type, ": ", constraint_date_type)
  ) %>%
  ## remove NA values from constraint position
  ## (i.e. remove dates with uncertainty regarding their stratigraphic position
  ## with respect to the deposit)
  dplyr::filter(!is.na(constraint_position)) %>%
  ## add likely interval from the Deposits data
  dplyr::left_join(
    .,
    dplyr::select(.data = data_deposits_scores, deposit_name, likely_interval, deposit_score_WH01, craton),
    by = join_by(deposit_name)
  ) %>%
  dplyr::filter(
    !is.na(deposit_score_WH01)
  ) %>%
  ## make likely_interval an ordered factor
  dplyr::mutate(
    likely_interval = ordered(
      likely_interval,
      levels = c(
        NA,
        "uncertain",
        "Cryogenian",
        "Ediacaran",
        "MEIH",
        "LEGH",
        "LEIH",
        "TEGH",
        "Cambrian"
      )
    ),
    # add an alpha (shade) column
    alf = ifelse(deposit_score_WH01 < 3,
                 shade_value,
                 1.0)
  )

## calculate min/max age constraints ----
data_ages_minmax <- data_deposits %>%
  dplyr::filter(!is.na(constraint_age_ma)) %>%
  dplyr::select(
    c(
      deposit_name,
      deposit_score_WH01,
      likely_interval, 
      craton,
      alf,
      reference1,
      constraint_type,
      constraint_position,
      constraint_date_type,
      constraint_age_ma,
      constraint_age_ma_unc
    )
  ) %>%
  group_by(deposit_name,
           deposit_score_WH01,
           likely_interval,
           craton,
           alf,
           constraint_position,
           constraint_date_type) %>%
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
  dplyr::group_by(deposit_name, 
                  deposit_score_WH01,
                  likely_interval, 
                  craton, 
                  alf) %>%
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
              decreasing = TRUE)
      ]
      )
    )
  )

data_scores_figS2 <- data_deposits %>%
  dplyr::distinct(deposit_name, deposit_score_WH01, alf, likely_interval)


# figures 1 and S1: compare compilations ----
## sort literature compilations data ----
### reduce and make long-form ----
data_compilations <- data_deposits_scores %>%
  dplyr::mutate(tindal_2023_glaciation = NA,
                this_study_2025_included = ifelse(!is.na(deposit_score_WH01),
                                             "Yes", 
                                             "No"),
                this_study_2025_glaciation = likely_interval
                ) %>%
  dplyr::select(deposit_name,
                deposit_score_WH01,
                deposit_score_Tindal2023,
                likely_interval,
                this_study_2025_included,
                this_study_2025_glaciation,
                tindal_2023_included,
                tindal_2023_glaciation,
                tindal_2023_evidence,
                youbi_2020_included,
                youbi_2020_glaciation,
                retallack_2022_included,
                retallack_2022_glaciation, 
                wang_2023_included,
                wang_2023_glaciation,
                niu_2024_included, 
                niu_2024_glaciation,
  ) %>%
  dplyr::rename(
    original_score = deposit_score_Tindal2023
  ) %>%
  pivot_longer(
    cols = starts_with(c("youbi_2020", "retallack_2022", "tindal_2023", "wang_2023", "niu_2024", "this_study_2025")),
    names_to = c("compilation", ".value"),
    names_pattern = "(.*\\d{4})_(.*)"
  ) %>%
  # remove rows where the deposit is not included in the compilation
  dplyr::filter(
    included == "Yes"
  ) %>%
  dplyr::mutate(
    compilation = ordered(
      compilation,
      levels = c("youbi_2020", "retallack_2022", "tindal_2023", "wang_2023", "niu_2024", "this_study_2025")
    )
  )

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
    compilation = ordered(compilation, levels = c("Retallack 2022",
                                                  "Wang et al 2023a,b",
                                                  "Niu et al 2024",
                                                  "this study"
                                                  )
                          )
  ) %>%
  # remove deposits that are "not Ediacaran" & only represented in the WH01 dataset
  dplyr::group_by(
    deposit_name
  ) %>%
  dplyr::filter(
    any(glaciation != "not Ediacaran" & n() > 1)
  ) %>%
  dplyr::ungroup()


## plot Figure S1 compilation glaciation age ranges ----
plot_figS1_compilation_glaciations <- literature_glaciations %>%
  ggplot(aes(y = Compilation, 
             xmin = Age_min,
             xmax = Age_max, 
             colour = glaciation_name)
  ) +
  geom_linerange(
    linewidth = 10,
    stat = "identity", 
    position = position_dodge(width = 0.25)
  ) + 
  scale_x_reverse(limits = c(600, 538.8)) +
  scale_y_discrete(limits = levels(literature_glaciations$Compilation)) +
  scale_colour_viridis_d(option = "turbo",
                         guide = guide_legend(reverse = TRUE)) +
  labs(x = "Age (Ma)", y = "Compilation", colour = "Inferred glaciation") + 
  theme_bw() + 
  theme(
    # legend.position = "inside",
    # legend.position.inside = c(0.15, 0.85),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size= 14)
  )

if(isTRUE(plot_show)){
  print(plot_figS1_compilation_glaciations)
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(
      dir_plots,
      paste0(
        outdate,
        "_fig_S1_compilation_glaciation_ranges.png"
      )
    ),
    width = 303.9,
    height = 153,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_figS1_compilation_glaciations)
  dev.off()
}

## plot Figure 1 deposits by compilation score and glaciation ----
library(ggpattern)
plot_fig1_compilation_deposits <- data_compilations_plot %>%
  dplyr::mutate(
    glaciation = case_when(
      glaciation == "All" ~ "uncertain",
      .default = glaciation
    ),
    glaciation = ordered(
    glaciation,
    levels = c(
      "TEGH",
      "LEIH",
      # "LEGH",
      "MEIH",
      "Hankalchough",
      "Bou Azzer",
      "Fauquier",
      "Gaskiers",
      # "All",
      # for Retallack Doushantuo Formation
      "GEG",
      "uncertain",
      "not Ediacaran"
    )
  ),
  likely_interval = fct_rev(ordered(likely_interval, 
                            levels = c(NA, "uncertain", "Cambrian", "Cryogenian", "Ediacaran", "MEIH", "LEIH", "TEGH"))
  ),
  # pattern_density = case_when( # should be able to make this continuous
  #   deposit_score_WH01 > 2 ~ 0.05,
  #   .default = 0.25
    # deposit_score_WH01 == 0 ~ 0.25,
  #   deposit_score_WH01 == 1 ~ 0.2,
  #   deposit_score_WH01 == 2 ~ 0.15,
  #   deposit_score_WH01 == 3 ~ 0.1,
  #   deposit_score_WH01 == 4 ~ 0.05,
  #   deposit_score_WH01 == 5 ~ 0
  # ),
  # pattern_density = ordered(as.character(deposit_score_WH01), levels = c(0, 1, 2, 3, 4, 5)),
  pattern_spacing = case_when(
    deposit_score_WH01 > 2 ~ ">2 star",
    .default = "≤2 star"
  )
  ) %>%
  dplyr::group_by(likely_interval) %>%
  dplyr::arrange(glaciation, desc(deposit_score_WH01), .by_group = TRUE) %>%
  dplyr::mutate(deposit_name = forcats::as_factor(deposit_name)) %>%
  # ggplot(aes(x = deposit_name,
  #            y = compilation,
  #            fill = glaciation,
  #            alpha = deposit_score_WH01
  # )) +
  # geom_tile(colour = "white") +
  ggplot(
    aes(
      x = deposit_name,
      y = compilation,
      fill = glaciation,
      # pattern_density = pattern_density
      pattern_spacing = pattern_spacing
    )
  ) +
  geom_tile_pattern(
    colour = "grey25",
    pattern = "stripe",
    pattern_angle = 45,
    pattern_fill = "white",
    pattern_colour = "white"
  ) +
  scale_x_discrete(position = "top", limits = rev) +
  scale_y_discrete(expand = c(0, 0)) +
  # scale_pattern_fill_viridis_d(option = "turbo", na.value = "grey50") +
  # scale_pattern_density_discrete() +
  scale_pattern_spacing_discrete(
    # limits = c("≤2 star", ">2 star"), 
    breaks = c(">2 star", "≤2 star"), 
    range = c(5, 0.01)
    ) +
  scale_fill_viridis_d(option = "turbo", na.value = "grey50") +
  labs(x = "Deposit name",
       y = "Compilation", 
       # alpha = "Score",
       fill = "Interval",
       # pattern_density = "Glaciogenicity"
       pattern_spacing = "Glaciogenicity"
       ) +
  # guides(alpha = guide_legend(reverse = TRUE)) +
  theme_graph +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 8, angle = 60, hjust = 0, vjust = 0.3),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.box = "horizontal"
  ) +
  guides(pattern_density = guide_legend(keywidth = unit(1, "cm"), keyheight = unit(5, "cm")))

if(isTRUE(plot_show)){
  print(plot_fig1_compilation_deposits)
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(
      dir_plots,
      paste0(
        outdate,
        "_fig_1_compilation_deposits_score.png"
      )
    ),
    width = 303.9,
    height = 153,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig1_compilation_deposits)
  dev.off()
}


# figure 2 deposit correlation and scores ----
## sort data ----
data_deposits_fig2 <- data_deposits %>%
  dplyr::ungroup() %>%
  dplyr::select(
    deposit_name, 
    deposit_score_WH01, 
    likely_interval, 
    alf,
    constraint_wrt_csl,
    constraint_position, 
    constraint_type, 
    constraint_date_type, 
    constraint_date_system, 
    constraint_age_ma,
    constraint_age_ma_unc
  ) %>%
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
    likely_interval = ordered(
      case_when(
        likely_interval == "Cambrian" ~ "uncertain", 
        .default = likely_interval
      ),
      levels = c("uncertain", "Ediacaran", "MEIH", "LEGH", "LEIH", "TEGH")
    ),
    # add a new variable for colour in the plots reflecting the dating method
    plot_colour_date_type = ordered(
      case_when(
        constraint_type == "Radiometric date" ~ paste0("Radiometric: ", constraint_date_system),
        # constraint_type == "CIE" ~ "Shuram CIE", 
        .default = constraint_type
      ),
      levels = c("Radiometric: U-Pb", "Radiometric: Re-Os", "Radiometric: Pb-Pb", "Radiometric: Ar-Ar",
                 "Biostratigraphy", "Correlation",
                 # "Shuram CIE"
                 "CIE"
                )
    ),
    plot_shape_constraint = ordered(
      constraint_wrt_csl,
      levels = c("maximum", "deposition", "minimum")
    )
  )

# recalculate min/max age constraints
data_ages_minmax_fig2 <- data_deposits_fig2 %>%
  # calculate each age with analytical uncertainty where relevant
  dplyr::mutate(
    age_ma_min_unc = ifelse(
      !is.na(constraint_age_ma - constraint_age_ma_unc),
      constraint_age_ma - constraint_age_ma_unc,
      constraint_age_ma
    ),
    age_ma_max_unc = ifelse(
      !is.na(constraint_age_ma + constraint_age_ma_unc),
      constraint_age_ma + constraint_age_ma_unc,
      constraint_age_ma
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    deposit_name, 
    deposit_score_WH01, 
    likely_interval, 
    alf
  ) %>%
  dplyr::summarise(
    plot_date_max = dplyr::case_when(
      !is.infinite(min(constraint_age_ma[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE)) ~ min(constraint_age_ma[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE),
      .default = 1000
    ),
    plot_date_max_unc = dplyr::case_when(
      !is.infinite(min(age_ma_max_unc[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE)) ~ min(age_ma_max_unc[plot_shape_constraint == "deposition" | plot_shape_constraint == "maximum"], na.rm = TRUE),
      .default = 1000
    ),
    plot_date_min = dplyr::case_when(
      !is.infinite(max(constraint_age_ma[plot_shape_constraint == "minimum"], na.rm = TRUE)) ~ max(constraint_age_ma[plot_shape_constraint == "minimum"], na.rm = TRUE),
      .default = 0
    ),
    plot_date_min_unc = dplyr::case_when(
      !is.infinite(max(age_ma_min_unc[plot_shape_constraint == "minimum"], na.rm = TRUE)) ~ max(age_ma_min_unc[plot_shape_constraint == "minimum"], na.rm = TRUE),
      .default = 0
    ),
    .groups = "keep"
  )
    
## order deposits by maximum age constraint ----
data_deposits_fig2 <- data_deposits_fig2 %>%
  dplyr::mutate(
    deposit_name = ordered(
      deposit_name,
      levels = unique(data_ages_minmax$deposit_name[
        order(-data_ages_minmax$alf,
              -data_ages_minmax$deposit_score_WH01, 
              data_ages_minmax$plot_date_max, 
              decreasing = TRUE)
      ]
      )
    )
  ) 

data_scores_fig2 <- data_deposits_fig2 %>%
  dplyr::distinct(deposit_name, deposit_score_WH01, alf, likely_interval)

### plot figure 2 scores ----
plot_fig2_deposit_scores <- ggplot(
  data = data_scores_fig2,
  aes(x = deposit_score_WH01,
      y = deposit_name)) +
  geom_col(aes(alpha = alf),
           fill = "steelblue") +
  geom_vline(xintercept = 2,
             linetype = "dashed",
             colour = "grey50") +
  scale_alpha_identity(
    guide = "none"
  ) +
  scale_y_discrete(name = "Candidate glacial deposits",
                   expand = expansion(add = 0.45)) +
  scale_x_reverse(
    name = "Star\nrating",
    limits = c(5, 0),
    breaks = seq(from = 5, to = 0, by = -1),
    expand = expansion(add = c(0.1, 0.1))
  ) +
  facet_grid(fct_rev(likely_interval) ~ .,
             scales = "free_y",
             space = "free_y",
             drop = TRUE) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(
      angle = 0,
      vjust = 0.3,
      hjust = 1,
      size = 6
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0.5), "cm")
  )
if(isTRUE(plot_show)){
  print(plot_fig2_deposit_scores)
}

### plot figure 2 dates ----
plot_fig2_deposit_dates <- ggplot() +
  # annotate with icehouse intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max, 
      ymin = -Inf,
      ymax = Inf
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  # annotate with E-C boundary
  geom_vline(
    xintercept = c(538.8, 635),
    linetype = "dashed",
    colour = "grey50"
  ) +
  # add solid line between min and max age ranges
  geom_linerange(
    data = data_ages_minmax_fig2,
    aes(y = deposit_name,
        xmin = plot_date_min_unc,
        xmax = plot_date_max_unc,
        alpha = alf),
    position = "identity",
    linetype = "solid",
    linewidth = 1,
    colour = "black"
  ) +
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
  geom_linerange(
    data = data_deposits_fig2,
    aes(
      x = constraint_age_ma,
      y = deposit_name,
      xmin = constraint_age_ma - constraint_age_ma_unc,
      xmax = constraint_age_ma + constraint_age_ma_unc,
      colour = plot_colour_date_type,
      alpha = alf,
      group = deposit_name
    ),
    # position = position_dodge2(width = 0.75),
    linewidth = 0.5
  ) +
  # add individual dates as points
  geom_point(
    data = data_deposits_fig2,
    aes(
      x = constraint_age_ma,
      y = deposit_name,
      shape = plot_shape_constraint,
      colour = plot_colour_date_type,
      alpha = alf,
      group = deposit_name
    ),
    # position = position_dodge2(width = 0.75),
    size = 3,
    stroke = 1,
  ) +
  # add an empty geom_col to align with score columns when panel plotting
  geom_col(
    data = data_deposits_fig2,
    aes(
      x = 0,
      y = deposit_name
    )) +
  scale_shape_manual(
    values = c(
      "maximum" = -9658, #24,
      "deposition" = 18,
      "minimum" = -9668 #25
    ),
    na.value = NA,
    breaks = c( "maximum", "deposition", "minimum", NA)
  ) +
  scale_colour_viridis_d(
    option = "turbo", 
    begin = 0.1,
    end = 0.9,
    direction = -1, 
    na.value = "grey25"
  ) +
  scale_alpha_identity(
    guide = "none"
  ) +
  # add scale_x expansion to allow alignment with geom_col below
  scale_y_discrete(expand = expansion(add = 2)) +
  scale_x_reverse(
    breaks = seq(from = min(plot_age_lims-10), to = max(plot_age_lims), by = 20),
    expand = expansion(add = 2)
  ) +
  labs(
    x = "Age (Ma)",
    y = "Candidate glacial deposits",
    shape = "Date position",
    colour = "Date type"
  ) +
  coord_geo(
    pos = "top", 
    height = unit(1, "line"),
    dat = ECESD_dt_MS1F1, 
    xlim = plot_age_lims
  ) +
  facet_grid(fct_rev(likely_interval) ~ ., 
             scales = "free_y",
             space = "free_y",
             drop = TRUE) +
  guides(
    shape = guide_legend(order = 1),
    colour = guide_legend(nrow = 3, order = 2) # byrow = TRUE
  ) + 
  theme_bw() +
  # theme edited to work with panel plot
  # remove x-axis details and sort plot margins
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    strip.text.y = element_text(size = 6, angle = 0, hjust = 0, colour = "black"),
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
    legend.margin = margin(0, 0, 0, 0, unit = "pt")
  )
# have a check
if(isTRUE(plot_show)){
  print(plot_fig2_deposit_dates)
}

### combine figure 2 as panel plot ----
plot_fig2_panel <- ggarrange(
  plot_fig2_deposit_scores, 
  plot_fig2_deposit_dates,
  ncol = 2, 
  align = "h",
  widths = c(0.5, 1)
)
if(isTRUE(plot_show)){
  plot_fig2_panel
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(dir_plots, paste0(outdate, "_fig_2_deposits_dates_scores_revised.png")),
    width = 159,
    height = 210,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig2_panel)
  dev.off()
}


## fig 2 simplified ----
### plot simplified figure 2 dates ----
plot_fig2_deposit_dates_simple <- ggplot() +
  # annotate with icehouse intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max, 
      ymin = -Inf,
      ymax = Inf
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  # annotate with E-C boundary
  geom_vline(
    xintercept = c(538.8, 635),
    linetype = "dashed",
    colour = "grey50"
  ) +
  # add solid line between min and max age ranges
  geom_linerange(
    data = data_ages_minmax_fig2,
    aes(y = deposit_name,
        xmin = plot_date_min_unc,
        xmax = plot_date_max_unc,
        alpha = alf),
    position = "identity",
    linetype = "solid",
    linewidth = 1,
    colour = "black"
  ) +
  # add an empty geom_col to align with score columns when panel plotting
  geom_col(
    data = data_deposits_fig2,
    aes(
      x = 0,
      y = deposit_name
    )) +
  scale_alpha_identity(
    guide = "none"
  ) +
  # add scale_x expansion to allow alignment with geom_col below
  scale_y_discrete(expand = expansion(add = 2)) +
  scale_x_reverse(
    breaks = seq(from = min(plot_age_lims-10), to = max(plot_age_lims), by = 20),
    expand = expansion(add = 2)
  ) +
  labs(
    x = "Age (Ma)",
    y = "Candidate glacial deposits",
    shape = "Date position",
    colour = "Date type"
  ) +
  coord_geo(
    pos = "top", 
    height = unit(1, "line"),
    dat = ECESD_dt_MS1F1, 
    xlim = plot_age_lims
  ) +
  facet_grid(fct_rev(likely_interval) ~ ., 
             scales = "free_y",
             space = "free_y",
             drop = TRUE) +
  theme_bw() +
  # theme edited to work with panel plot
  # remove x-axis details and sort plot margins
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    strip.text.y = element_text(size = 6, angle = 0, hjust = 0, colour = "black"),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
# have a check
if(isTRUE(plot_show)){
  print(plot_fig2_deposit_dates_simple)
}

### combine figure 2 as panel plot ----
plot_fig2_panel_simple <- ggarrange(
  plot_fig2_deposit_scores, 
  plot_fig2_deposit_dates_simple,
  ncol = 2, 
  align = "h",
  widths = c(0.5, 1)
)
if(isTRUE(plot_show)){
  plot_fig2_panel_simple
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(dir_plots, paste0(outdate, "_fig_2_deposits_dates_scores_revised_simple.png")),
    width = 159,
    height = 210,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig2_panel_simple)
  dev.off()
}


# figure 3 maps ----
## set parameters ----
# list map ages
map_ages_ma <- c(
  "MEIH" = 585, 
  "LEGH" = 570, 
  "LEIH" = 555, 
  "TEGH" = 545
  )

# list rotation models 
rot_models <- c(
  "MERDITH2021",
  "PALEOMAP"
  )

# variable names to hold rotated points
# depends on the listed rotation models
rot_cols_lon <- c(
  "plon_M21",
  "plon_PAL"
)
rot_cols_lat <- c(
  "plat_M21",
  "plat_PAL"
)

size_score <- c(
  "NA" = 0.25,
  "0" = 0.25,
  "1" = 0.5,
  "2" = 1,
  "3" = 2,
  "4" = 4,
  "5" = 8
)

alpha_score <- c(
  "NA" = 1/6,
  "0" = 1/6,
  "1" = 2/6,
  "2" = 3/6,
  "3" = 4/6,
  "4" = 5/6,
  "5" = 1
)

## sort data ----
### trim dataset ----
data_deposits_to_rotate <- data_deposits_scores %>%
  dplyr::select(likely_interval, 
                deposit_name, 
                craton,
                avg_longitude, avg_latitude, 
                deposit_score_WH01
                ) %>%
  dplyr::filter(
    likely_interval %in% c("MEIH", "LEGH", "LEIH", "TEGH")
  ) %>% 
  dplyr::mutate(
    rot_age = case_when(
      likely_interval == "MEIH" ~ 585,
      likely_interval == "LEGH" ~ 570,
      likely_interval == "LEIH" ~ 555,
      likely_interval == "TEGH" ~ 545,
      .default = NA
    )
  )

### rotate data ----
# make dataset to hold rotation information
# remove NA lon and lat values for rotation
# for now focus only on possible MEIH data
data_deposits_rotated <- data_deposits_to_rotate %>%
  dplyr::filter(
    !is.na(avg_longitude),
    !is.na(avg_latitude)
  )

for(i in 1:length(data_deposits_rotated$deposit_name)){
  # get coordinates
  xy <- c(
    data_deposits_rotated$avg_longitude[i],
    data_deposits_rotated$avg_latitude[i]
  )
  rot_age <- data_deposits_rotated$rot_age[i]
  
  for(j in 1:length(rot_models)){
    # print(rot_models[j])
    # rotate for each model
    rotated <- rgplates::reconstruct(
      x = xy,
      age = rot_age,
      model = rot_models[j]
    )
    # append to data.frame
    data_deposits_rotated[i, rot_cols_lon[j]] <- rotated[1,1]
    data_deposits_rotated[i, rot_cols_lat[j]] <- rotated[1,2]
  }
}

### sort rotated data ----
data_deposits_rotated_long <- data_deposits_rotated %>%
  tidyr::pivot_longer(
    cols = starts_with(c("plon_", "plat_")),
    names_sep = "_",
    names_to = c(".value", "rot_model"),
    values_drop_na = TRUE
  ) %>%
  st_as_sf(coords = c("plon",
                      "plat"),
           remove = FALSE,
           crs = 4326) %>%
  dplyr::mutate(
    rot_model = case_when(
      rot_model == "M21" ~ "MERDITH2021",
      rot_model == "PAL" ~ "PALEOMAP"
    ),
    abs_lat_bin = ordered(
      case_when(
        abs(plat) < 30 ~ "low",
        abs(plat) > 60 ~ "high",
        .default = "mid"),
      levels = c("low", "mid", "high")
    )
  )

### save unrotated data ----
data_deposits_unrotated <- data_deposits_rotated %>%
  tidyr::pivot_longer(
    cols = starts_with(c("plon_", "plat_")),
    names_sep = "_",
    names_to = c(".value", "rot_model"),
    values_drop_na = FALSE
  ) %>% 
  dplyr::filter(is.na(plon)) %>%
  dplyr::mutate(
    rot_model = case_when(
      rot_model == "M21" ~ "MERDITH2021",
      rot_model == "PAL" ~ "PALEOMAP"
    )
  )


## make maps ----
### generate base maps ----
# make map edge for plotting
map_edge <- rgplates::mapedge() %>% 
  st_as_sf(crs = 4326)

# make list to hold maps
rot_maps <- list()
for(i in 1:length(rot_models)){
  print(paste0("Getting ", rot_models[i], " maps for ..."))
  
  for(j in 1:length(map_ages_ma)){
    print(paste0("... ", names(map_ages_ma[j]), " at ", map_ages_ma[j], " Ma"))
    
    rot_maps[[rot_models[i]]][[names(map_ages_ma[j])]] <- rgplates::reconstruct(
      x = "coastlines",
      age = map_ages_ma[j],
      model = rot_models[i]
    ) %>% 
      st_as_sf(crs = 4326)
  }
  print(paste0("... finished getting ", rot_models[i], " maps."))
}

### map parameters ----
map_added_theme <- theme(
  plot.subtitle = element_text(size = 18/.pt, hjust = 0.5),
  legend.title = element_text(size = 18/.pt),
  legend.text = element_text(size = 18/.pt),
  axis.title = element_blank()
)
map_crs <- st_crs("+proj=robin")
map_default_crs <- sf::st_crs(4326)

### plot maps ----
plot_maps <- list()
for(i in 1:length(rot_maps)){
  # loop through rotation models
  # get map name
  this_rot_model <- names(rot_maps[i])
  
  print(paste0("Making deposit maps for ", this_rot_model, " ..."))
  
  for(j in 1:length(rot_maps[[i]])){
    # get age interval
    this_interval <- names(map_ages_ma[j])
    this_age <- map_ages_ma[j]
    
    print(paste0("... ", this_interval, " at ", this_age, " Ma"))
    
    # select data
    these_data <- data_deposits_rotated_long %>%
      dplyr::filter(
        rot_model == this_rot_model,
        likely_interval == this_interval
      )
    
    # select map
    this_map <- rot_maps[[this_rot_model]][[this_interval]]
    
    this_map_deposits <- map_for_deposits(
      deposit_data = these_data,
      deposit_data_mapping = aes(size = ordered(deposit_score_WH01, levels = c(0, 1, 2, 3, 4, 5)),
                                 alpha = ordered(deposit_score_WH01, levels = c(0, 1, 2, 3, 4, 5))),
      deposit_data_shape = 23,
      deposit_data_colour = "black",
      deposit_data_fill = "darkorange",
      map_edge = map_edge,
      map_edge_colour = "black",
      map_edge_fill = "steelblue",
      map_edge_alpha = 0.3,
      palaeomap = this_map,
      palaeomap_colour = "grey50",
      palaeomap_fill = "#088080",
      palaeomap_alpha = 0.5,
      this_subtitle = paste0(
        # "(", LETTERS[j], ")   ", 
        this_rot_model, " ", this_interval, ": ", this_age, " Ma"
        ), 
      this_alpha_values = alpha_score,
      this_alpha_lab = "Score",
      this_size_values = size_score,
      this_size_lab = "Score",
      this_colour_values = NULL,
      this_colour_lab = "",
      plot_theme = theme_minimal(),
      added_themes = map_added_theme,
      underlying_annotations = NULL,
      overlying_annotations = list(),
      this_crs = map_crs,
      this_default_crs = map_default_crs
    )
    # print(this_map_deposits)
    plot_maps[[this_rot_model]][[this_interval]] <- this_map_deposits
  }
  
}

### combine maps into figures ----
#### just PALEOMAP ----
plot_fig_panel_maps_PAL <- plot_maps[["PALEOMAP"]][["MEIH"]] +
  plot_maps[["PALEOMAP"]][["LEGH"]] +
  plot_maps[["PALEOMAP"]][["LEIH"]] +
  plot_maps[["PALEOMAP"]][["TEGH"]] + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  tag_prefix = "(", 
                  tag_sep = "", 
                  tag_suffix = ")"
  ) & 
  theme(plot.tag = element_text(size = 18/.pt))
  
if(isTRUE(plot_show)){
  plot_fig_panel_maps_PAL
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(dir_plots, paste0(outdate, "_fig_3_deposits_maps_PALEOMAP.png")),
    width = 159,
    height = 90,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig_panel_maps_PAL)
  dev.off()
}

#### just MERTDITH2021 ----
plot_fig_panel_maps_MER <- plot_maps[["MERDITH2021"]][["MEIH"]] +
  plot_maps[["MERDITH2021"]][["LEGH"]]  +
  plot_maps[["MERDITH2021"]][["LEIH"]] +
  plot_maps[["MERDITH2021"]][["TEGH"]] + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  tag_prefix = "(", 
                  tag_sep = "", 
                  tag_suffix = ")"
  ) & 
  theme(plot.tag = element_text(size = 18/.pt))

if(isTRUE(plot_show)){
  plot_fig_panel_maps_MER
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(dir_plots, paste0(outdate, "_fig_3_deposits_maps_MERDITH2021.png")),
    width = 159,
    height = 90,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig_panel_maps_MER)
  dev.off()
}

#### combined icehouse intervals ----
plot_fig_panel_maps_both <- plot_maps[["MERDITH2021"]][["MEIH"]] +
  plot_maps[["MERDITH2021"]][["LEIH"]]  +
  plot_maps[["PALEOMAP"]][["MEIH"]] +
  plot_maps[["PALEOMAP"]][["LEIH"]] + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  tag_prefix = "(", 
                  tag_sep = "", 
                  tag_suffix = ")"
                  ) & 
  theme(plot.tag = element_text(size = 18/.pt))


if(isTRUE(plot_show)){
  plot_fig_panel_maps_both
}

if(isTRUE(plot_save)) {
  png(
    file = file.path(dir_plots, paste0(outdate, "_fig_3_deposits_maps_both.png")),
    width = 159,
    height = 90,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig_panel_maps_both)
  dev.off()
}


# END ----