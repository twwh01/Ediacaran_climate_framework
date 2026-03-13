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
outdate <- "20250530"

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

data_scores_FigS2 <- data_deposits %>%
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
plot_fig1_compilation_deposits <- data_compilations_plot %>%
  dplyr::mutate(glaciation = ordered(
    glaciation,
    levels = c(
      "TEGH", 
      "LEIH",
      "LEGH",
      "MEIH",
      "Hankalchough",
      "Bou Azzer",
      "Fauquier",
      "Gaskiers",
      "All", # for Retallack Doushantuo Formation
      "GEG",
      "uncertain",
      "not Ediacaran"
    )
  )) %>%
  dplyr::arrange(glaciation, desc(deposit_score_WH01)) %>%
  dplyr::mutate(deposit_name = forcats::as_factor(deposit_name)) %>%
  ggplot(aes(x = deposit_name,
             y = compilation,
             fill = glaciation,
             alpha = deposit_score_WH01
  )) +
  geom_tile(colour = "white") +
  scale_x_discrete(position = "top", limits = rev) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "turbo", na.value = "grey50") +
  labs(x = "Deposit name",
       y = "Compilation", 
       fill = "Interval",
       alpha = "Score") +
  guides(alpha = guide_legend(reverse = TRUE)) +
  theme_graph +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 8, angle = 60, hjust = 0, vjust = 0.3),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.box = "horizontal"
  )

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
  dplyr::filter(
    !is.na(constraint_age_ma) &
      likely_interval != "Cryogenian" &
      (is.na(constraint_date_system) | constraint_date_system != "Rb-Sr")
    )

# recalculate min/max age constraints
data_ages_minmax_fig2 <- data_deposits_fig2 %>%
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
      constraint_date_system,
      constraint_age_ma,
      constraint_age_ma_unc
    )
  ) %>%
  group_by(deposit_name,
           likely_interval,
           craton,
           constraint_position,
           constraint_date_type
           ) %>%
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
  dplyr::group_by(deposit_name, deposit_score_WH01, likely_interval, alf, craton) %>%
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
      !is.infinite(min(age_ma_max_unc[constraint_position == "below"], na.rm = TRUE)),
      min(age_ma_max_unc[constraint_position == "below"], na.rm = TRUE),
      ifelse(
        !is.infinite(min(age_ma_max_unc[constraint_position == "cont."], na.rm = TRUE)),
        min(age_ma_max_unc[constraint_position == "cont."], na.rm = TRUE),
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
      !is.infinite(max(age_ma_min_unc[constraint_position == "above"], na.rm = TRUE)),
      max(age_ma_min_unc[constraint_position == "above"], na.rm = TRUE),
      0
    ), 
    
    plot_date_range = plot_date_max - plot_date_min,
    plot_date_range_unc = plot_date_max_unc - plot_date_min_unc
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


## plot vertical version of figure 2 dates ----
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
  # add error bars for geom_point
  geom_linerange(
    data = data_deposits_fig2,
    aes(
      x = constraint_age_ma,
      y = deposit_name,
      xmin = constraint_age_ma - constraint_age_ma_unc,
      xmax = constraint_age_ma + constraint_age_ma_unc,
      colour = constraint_date_type,
      alpha = alf,
      group = deposit_name
    ),
    position = position_dodge(width = 1),
    linewidth = 0.5
  ) +
  # add individual dates as points
  geom_point(
    data = data_deposits_fig2,
    aes(
      x = constraint_age_ma,
      y = deposit_name,
      shape = constraint_position,
      colour = constraint_date_type,
      alpha = alf,
      group = deposit_name
    ),
    size = 3,
    stroke = 1,
    position = position_dodge(width = 1)
  ) +
  # add dashed line between min and max dates with uncertainty
  geom_linerange(
    data = data_ages_minmax_fig2,
    aes(y = deposit_name,
        xmin = plot_date_min_unc,
        xmax = plot_date_max_unc,
        alpha = alf),
    position = "identity",
    linetype = "dotted",
    linewidth = 1,
    colour = "black"
  ) +
  # add solid line between min and max dates without uncertainty
  geom_linerange(
    data = data_ages_minmax_fig2,
    aes(y = deposit_name,
        xmin = plot_date_min,
        xmax = plot_date_max,
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
  # scales and formatting
  scale_shape_manual(
    values = c(
      "below" = -9658, #24,
      "cont." = 18,
      "above" = -9668 #25
    ),
    na.value = 1,
    breaks = c( "below", "cont.", "above", NA)
  ) +
  scale_colour_viridis_d(
    option = "turbo", 
    end = 0.8,
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
    colour = "Date type",
    shape = "Date position"   
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
    #strip.background = element_rect(fill = "white", colour = "grey50"),
    strip.text.y = element_text(size = 6, angle = 0, hjust = 0, colour = "black"),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.direction = "horizontal", 
    legend.box = "vertical",
    legend.box.spacing = unit(1, "pt"),
    legend.position = "bottom",
    legend.spacing.x = unit(0, "pt"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6,
                               margin = margin(0, 0, 0, 0)),
    legend.spacing.y = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0, unit = "pt")
  )
# have a check
if(isTRUE(plot_show)){
  print(plot_fig2_deposit_dates)
}

### plot Figure 1 vertical scores ----
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

### combine Figure 1 vertical as panel plot ----
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
    file = file.path(dir_plots, paste0(indate, "_fig_2_deposits_dates_scores.png")),
    width = 159,
    height = 210,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig2_panel)
  dev.off()
}


# END ----