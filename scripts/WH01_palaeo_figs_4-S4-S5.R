# Plotting stratigraphic ranges of fossil taxa
# clear the decks ----
rm(list = ls())


# user parameters ----
plot_age_lims <- c(595, 530) # in Ma for reverse age axis
shade_value <- 0.3
plot_show <- FALSE
plot_save <- TRUE

## set directory paths ----
dir_data_in <- file.path("data", "raw")
dir_plots <- file.path("figures")

## specify files ----
infile_palaeo_name <- "WH01_SuppData_2_paleontology.xlsx"
infile_palaeo <- file.path(dir_data_in, infile_palaeo_name)


# load packages ----
library(dplyr)
library(magrittr)
library(tidyr)
library(forcats)
library(ggplot2)
library(openxlsx2)
library(deeptime)


# load functions & palettes & themes ----
source(file = file.path("utils", "ecesd_glaciations_table.R")) # ecesd glacial intervals
source(file = file.path("utils", "ecesd_deeptime_dat.R"))
source(file = file.path("utils", "custom_themes.R"))


# load data ----
data_palaeo_wb <- openxlsx2::wb_load(file = infile_palaeo)

data_palaeo_in <- openxlsx2::wb_to_df(file = data_palaeo_wb,
                                      sheet = "Table_S4_palaeo_data",
                                      na.strings = "NA")
data_palaeo_in <- data_palaeo_in[colSums(!is.na(data_palaeo_in)) > 0]


# select plotting data ----
## all data ----
### taxon (species) ----
data_palaeo_plot <- data_palaeo_in %>%
  # remove assemblages
  # remove Cambrian data for main plots
  dplyr::filter(Rank1_supraphylum != "assemblage",
                associated_assemblage != "Cambrian") %>%
  # make associated_assemblage an ordered factor
  dplyr::mutate(associated_assemblage = ordered(associated_assemblage,
                                                levels = c(NA,
                                                           "ichnofossil",
                                                           "Cambrian",
                                                           "Nama",
                                                           "White Sea",
                                                           "Avalon",
                                                           "Wenghui",
                                                           "Weng'an",
                                                           "Lantian",
                                                           "Ediacaran"))) %>%
  # make taxonomic hierarchy factors ordered from oldest to youngest
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
    
    Taxon = fct_reorder(
      as.factor(Taxon),
      plot_age,
      .fun = max,
      .desc = TRUE,
      .na_rm = TRUE
    ),
    Rank5_genus = fct_reorder(
      as.factor(Rank5_genus),
      plot_age,
      .fun = max,
      .desc = TRUE,
      .na_rm = TRUE
    ),
    Morphogroup = fct_reorder(
      as.factor(Morphogroup),
      plot_age,
      .fun = max,
      .desc = TRUE,
      .na_rm = TRUE
    )
  ) %>% 
  dplyr::filter(!is.na(plot_age))

### genera ----
data_palaeo_genera_plot <- data_palaeo_plot %>%
  dplyr::filter(!is.na(Rank5_genus))

### morphogroup ----
data_palaeo_morpho_plot <- data_palaeo_plot %>%
  dplyr::filter(!is.na(Morphogroup)) 

## no South Australia ----
# remove assemblages and South Australia data
### taxa (species) ----
data_palaeo_noSA_plot <- data_palaeo_plot %>%
  dplyr::filter(Craton != "South Australia")

### genera ----
data_palaeo_genera_noSA_plot <- data_palaeo_noSA_plot %>%
  dplyr::filter(!is.na(Rank5_genus))

### morphogroup ----
data_palaeo_morpho_noSA_plot <- data_palaeo_noSA_plot %>%
  dplyr::filter(!is.na(Morphogroup))

## make plotting labels ----
### labels for taxa-assemblage-water depth ----
taxon_axis_labels_wd_aa <- data_palaeo_plot %>%
  dplyr::select(
    c(
      Taxon,
      associated_assemblage,
      plot_age,
      plot_age_max,
      plot_age_min
    )
  ) %>%
  # only group by variables that are being plotted here
  dplyr::group_by(Taxon,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
  )

### labels for no SA taxa-assemblage-water depth ----
taxon_axis_labels_wd_aa_noSA <- data_palaeo_noSA_plot %>%
  dplyr::select(
    c(
      Taxon,
      Crude_lithofacies,
      Water_depth,
      associated_assemblage,
      plot_age,
      plot_age_min,
      plot_age_max
    )
  ) %>%
  # only group by variables that are being plotted here
  dplyr::group_by(Taxon,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
  )

### labels for genera-assemblage-water depth ----
genus_axis_labels_wd_aa <- data_palaeo_plot %>%
  dplyr::select(
    c(
      Rank5_genus,
      Crude_lithofacies,
      Water_depth,
      associated_assemblage,
      plot_age,
      plot_age_min,
      plot_age_max
    )
  ) %>%
  # only group by variables that are being plotted here
  dplyr::group_by(Rank5_genus,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
  )

### labels for no SA genera-assemblage-water depth ----
genus_axis_labels_wd_aa_noSA <- data_palaeo_noSA_plot %>%
  dplyr::select(
    c(
      Rank5_genus,
      Crude_lithofacies,
      Water_depth,
      associated_assemblage,
      plot_age,
      plot_age_min,
      plot_age_max
    )
  ) %>%
  # only group by variables that are being plotted here
  dplyr::group_by(Rank5_genus,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
  )

### labels for morphogroup-assemblage-water depth ----
# using Morphogroup level for morphogroups
# this includes rangeomorphs, arboreomorphs, cloudinds, etc. as their own groups
morpho_axis_labels_wd_aa <- data_palaeo_plot %>%
  dplyr::select(
    c(
      Morphogroup,
      Crude_lithofacies,
      Water_depth,
      associated_assemblage,
      plot_age,
      plot_age_min,
      plot_age_max
    )
  ) %>%
  dplyr::group_by(Morphogroup,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
  )

### labels for no SA morphogroup-assemblage-water depth ----
# using Morphogroup level for morphogroups
# this includes rangeomorphs, arboreomorphs, cloudinds, etc. as their own groups
morpho_axis_labels_wd_aa_noSA <- data_palaeo_noSA_plot %>%
  dplyr::select(
    c(
      Morphogroup,
      Crude_lithofacies,
      Water_depth,
      associated_assemblage,
      plot_age,
      plot_age_min,
      plot_age_max
    )
  ) %>%
  # only group by variables that are being plotted here
  dplyr::group_by(Morphogroup,
                  associated_assemblage) %>%
  dplyr::summarise(label_max = max(plot_age_max, na.rm = TRUE),
                   label_age = max(plot_age, na.rm = TRUE)
                   )


# make plots ----
## plot dates by assemblage & water depth ----
### plot all taxa ----
# make plot
plot_taxa <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
    data = data_palaeo_plot, 
    aes(
      x = plot_age,
      y = Taxon,
      colour = Water_depth,
      shape = Water_depth
    ),
    size = 2
  ) +
  geom_linerange(data = data_palaeo_plot,
                 aes(
                   y = Taxon,
                   xmin = plot_age_min,
                   xmax = plot_age_max,
                   colour = Water_depth
                 ),
                 linetype = "dotted"
                 ) +
  geom_label(
    data = taxon_axis_labels_wd_aa,
    aes(x = label_age + 1,
        y = Taxon,
        label = Taxon),
    angle = 0,
    hjust = 1,
    vjust = 0.4,
    size = 2, 
    label.size = 0,
    alpha = 0.25
  ) +
  # scale_x_reverse() +
  scale_x_reverse(limits = plot_age_lims,
                  expand = expansion(add = c(0, 0))) +
  scale_colour_manual(
    values = c(
      "deep" = "midnightblue",
      "mid" = "steelblue",
      "shallow" = "orange"
    ),
    na.value = "grey50",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_discrete(na.value = 4,
                       guide = guide_legend(reverse = TRUE)) +
  labs(
    x = "Age (Ma)",
    # y = "Taxon",
    colour = "Water depth",
    shape = "Water depth"
  ) +
  facet_grid(associated_assemblage ~ .,
             scales = "free_y",
             space = "free_y",
             # switch = "y",
             labeller = labeller(associated_assemblage = label_wrap_gen(6))
             ) +
  coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(size = 16, angle = 0, hjust = 0, colour = "black")
        )

if (isTRUE(plot_show)) {
  print(plot_taxa)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_S99_rangesT_taxa_waterdepth_assemblage_glac.png"),
    width = 3000,
    height = 5000,
    units = "px",
    bg = "white",
    res = 450
  )
  print(plot_taxa)
  dev.off()
}

### no South Australia: assemblage & water depth ----
# make this plot
plot_taxa_noSA <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
        data = data_palaeo_noSA_plot, 
        aes(
          x = plot_age,
          y = Taxon,
          colour = Water_depth,
          shape = Water_depth
        ),
        size = 2
      ) +
      geom_linerange(data = data_palaeo_noSA_plot,
                     aes(
                       y = Taxon,
                       xmin = plot_age_min,
                       xmax = plot_age_max,
                       colour = Water_depth
                     ),
                     linetype = "dotted"
      ) +
      geom_label(
        data = taxon_axis_labels_wd_aa_noSA,
        aes(x = label_age + 1,
            y = Taxon,
            label = Taxon),
        angle = 0,
        hjust = 1,
        vjust = 0.4,
        size = 2, 
        label.size = 0,
        alpha = 0.25
      ) +
      # scale_x_reverse() +
      scale_x_reverse(limits = plot_age_lims,
                      expand = expansion(add = c(0,0))) +
      scale_colour_manual(
        values = c(
          "deep" = "midnightblue",
          "mid" = "steelblue",
          "shallow" = "orange"
        ),
        na.value = "grey50",
        guide = guide_legend(reverse = TRUE)
      ) +
      scale_shape_discrete(na.value = 4,
                           guide = guide_legend(reverse = TRUE)) +
      labs(
        x = "Age (Ma)",
        # y = "Taxon",
        colour = "Water depth",
        shape = "Water depth"
      ) +
      facet_grid(associated_assemblage ~ .,
                 scales = "free_y",
                 space = "free_y",
                 # switch = "y",
                 labeller = labeller(associated_assemblage = label_wrap_gen(6))
      ) +
      coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  # theme_graph +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # strip.text.y.right = element_text(angle = 0),
        strip.text.y = element_text(size = 16, angle = 0, hjust = 0, colour = "black")
  )

if (isTRUE(plot_show)) {
  print(plot_taxa_noSA)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_S99_rangesT_taxa_waterdepth_assemblage_glac_noSA.png"),
    width = 3000,
    height = 5000,
    units = "px",
    bg = "white",
    res = 450
  )
  print(plot_taxa_noSA)
  dev.off()
}

## plot Figure 4: genera ----
### assemblage & water depth ----
# make plot
plot_Fig4_genera <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
    data = data_palaeo_genera_plot,
    aes(
      x = plot_age,
      y = Rank5_genus,
      colour = Water_depth,
      shape = Water_depth
    ),
    size = 1
  ) +
  geom_linerange(
    data = data_palaeo_genera_plot,
    aes(
      y = Rank5_genus,
      xmin = plot_age_min,
      xmax = plot_age_max,
      colour = Water_depth
    ),
    linetype = "dotted"
  ) +
  geom_label(
    data = genus_axis_labels_wd_aa,
    aes(x = label_age + 1,
        y = Rank5_genus,
        label = Rank5_genus),
    angle = 0,
    hjust = 1,
    vjust = 0.4,
    size = 5/.pt,
    fontface = "italic", 
    label.size = 0,
    alpha = 0.25
  ) +
  # scale_x_reverse() +
  scale_x_reverse(limits = plot_age_lims, 
                  expand = expansion(add = c(0,0))) +
  scale_colour_manual(
    values = c(
      "deep" = "midnightblue",
      "mid" = "steelblue",
      "shallow" = "orange"
    ),
    na.value = "grey50",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_discrete(na.value = 4,
                       guide = guide_legend(reverse = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Genera / Ichnogenera",
    colour = "Water\ndepth",
    shape = "Water\ndepth"
  ) +
  facet_grid(associated_assemblage ~ ., 
             scales = "free_y",
             space = "free_y") +
  coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.y = element_text(size = 10, angle = -90, hjust = 0.5, colour = "black")
  )

if (isTRUE(plot_show)) {
  print(plot_Fig4_genera)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_4_rangesT_genera_waterdepth_assemblage_glac.png"),
    width = 159,
    height = 245,
    units = "mm",
    bg = "white",
    res = 1200
  )
  print(plot_Fig4_genera)
  dev.off()
  
  # pdf
  pdf(
    file = file.path(dir_plots, "fig_4_rangesT_genera_waterdepth_assemblage_glac.pdf"),
    width = 6.26,
    height = 9.65,
    bg = "white"
  )
  print(plot_Fig4_genera)
  dev.off()
}

## plot Figure S5: genera no South Australia ----
# make this plot
# make plot
plot_FigS5_genera_noSA <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
    data = data_palaeo_genera_noSA_plot,
    aes(
      x = plot_age,
      y = Rank5_genus,
      colour = Water_depth,
      shape = Water_depth
    ),
    size = 1
  ) +
  geom_linerange(
    data = data_palaeo_genera_noSA_plot,
    aes(
      y = Rank5_genus,
      xmin = plot_age_min,
      xmax = plot_age_max,
      colour = Water_depth
    ),
    linetype = "dotted"
  ) +
  geom_label(
    data = genus_axis_labels_wd_aa_noSA,
    aes(x = label_age + 1,
        y = Rank5_genus,
        label = Rank5_genus),
    angle = 0,
    hjust = 1,
    vjust = 0.4,
    size = 5/.pt,
    fontface = "italic", 
    label.size = 0,
    alpha = 0.25
  ) +
  # scale_x_reverse() +
  scale_x_reverse(limits = plot_age_lims, 
                  expand = expansion(add = c(0,0))) +
  scale_colour_manual(
    values = c(
      "deep" = "midnightblue",
      "mid" = "steelblue",
      "shallow" = "orange"
    ),
    na.value = "grey50",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_discrete(na.value = 4,
                       guide = guide_legend(reverse = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Genera / Ichnogenera",
    colour = "Water\ndepth",
    shape = "Water\ndepth"
  ) +
  facet_grid(
    associated_assemblage ~ ., 
    # Morphogroup ~ .,         
    scales = "free_y",
    space = "free_y",
    drop = TRUE) +
  coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.y = element_text(size = 10, angle = -90, hjust = 0.5, colour = "black")
  )

if (isTRUE(plot_show)) {
  print(plot_FigS5_genera_noSA)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_S5_rangesT_genera_noSA_waterdepth_assemblage_glac.png"),
    width = 159,
    height = 245,
    units = "mm",
    bg = "white",
    res = 1200
  )
  print(plot_FigS5_genera_noSA)
  dev.off()
  
  # pdf
  pdf(
    file = file.path(dir_plots, "fig_S5_rangesT_genera_noSA_waterdepth_assemblage_glac.pdf"),
    width = 6.26,
    height = 9.65,
    bg = "white"
  )
  print(plot_FigS5_genera_noSA)
  dev.off()
}

## plot by morphogroup ----
# using Morphogroup level for morphogroups
# this includes rangeomorphs, arboreomorphs, cloudinds, etc. as their own groups
### plot Figure S4: morphogroups ----
# make plot
plot_FigS4_morpho <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
    data = data_palaeo_morpho_plot,
    aes(
      x = plot_age,
      y = Morphogroup,
      colour = Water_depth,
      shape = Water_depth
    ),
    size = 2
  ) +
  geom_linerange(
    data = data_palaeo_morpho_plot,
    aes(
      y = Morphogroup,
      xmin = plot_age_min,
      xmax = plot_age_max,
      colour = Water_depth
    ),
    linetype = "dotted"
  ) +
  geom_label(
    data = morpho_axis_labels_wd_aa,
    aes(x = label_age + 1,
        y = Morphogroup,
        label = Morphogroup),
    angle = 0,
    hjust = 1,
    vjust = 0.4,
    size = 8/.pt,
    label.size = 0,
    alpha = 0.25
  ) +
  # scale_x_reverse() +
  scale_x_reverse(limits = plot_age_lims, 
                  expand = expansion(add = c(0,0))) +
  scale_colour_manual(
    values = c(
      "deep" = "midnightblue",
      "mid" = "steelblue",
      "shallow" = "orange"
    ),
    na.value = "grey50",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_discrete(na.value = 4,
                       guide = guide_legend(reverse = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Morphogroup",
    colour = "Water depth",
    shape = "Water depth"
  ) +
  facet_grid(associated_assemblage ~ ., 
             scales = "free_y",
             space = "free_y") +
  coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  # theme_graph +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(), 
    # strip.text.y.right = element_text(angle = 0),
    strip.text.y = element_text(size = 10, angle = -90, hjust = 0.5, colour = "black")
  )

if (isTRUE(plot_show)) {
  print(plot_FigS4_morpho)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_S4_rangesT_morpho_waterdepth_assemblage_glac.png"),
    width = 159,
    height = 180,
    units = "mm",
    bg = "white",
    res = 1200
  )
  print(plot_FigS4_morpho)
  dev.off()
  
  # pdf
  pdf(
    file = file.path(dir_plots, "fig_S4_rangesT_morpho_waterdepth_assemblage_glac.pdf"),
    width = 6.26,
    height = 7.09,
    bg = "white"
  )
  print(plot_FigS4_morpho)
  dev.off()
}

### plot Figure S7 (UNUSED): morphogroups no South Australia ----
# make this plot
plot_FigS7_morpho_noSA <- ggplot() +
  # annotate with ECESD glacial intervals
  geom_rect(
    data = ecesd_glaciations,
    aes(
      xmin = Age_min,
      xmax = Age_max,
      ymin = -Inf,
      ymax = Inf,
    ),
    alpha = 0.3,
    fill = "steelblue",
    colour = "steelblue"
  ) +
  geom_point(
    data = data_palaeo_morpho_noSA_plot,
    aes(
      x = plot_age,
      y = Morphogroup,
      colour = Water_depth,
      shape = Water_depth
    ),
    size = 2
  ) +
  geom_linerange(
    data = data_palaeo_morpho_noSA_plot,
    aes(
      y = Morphogroup,
      xmin = plot_age_min,
      xmax = plot_age_max,
      colour = Water_depth
    ),
    linetype = "dotted"
  ) +
  geom_label(
    data = morpho_axis_labels_wd_aa_noSA,
    aes(x = label_age + 1,
        y = Morphogroup,
        label = Morphogroup),
    angle = 0,
    hjust = 1,
    vjust = 0.4,
    size = 8/.pt,
    label.size = 0,
    alpha = 0.25
  ) +
  # scale_x_reverse() +
  scale_x_reverse(limits = plot_age_lims, 
                  expand = expansion(add = c(0,0))) +
  scale_colour_manual(
    values = c(
      "deep" = "midnightblue",
      "mid" = "steelblue",
      "shallow" = "orange"
    ),
    na.value = "grey50",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_discrete(na.value = 4,
                       guide = guide_legend(reverse = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Morphogroup",
    colour = "Water\ndepth",
    shape = "Water\ndepth"
  ) +
  facet_grid(associated_assemblage ~ ., 
             scales = "free_y",
             space = "free_y") +
  coord_geo(pos = "top", dat = ECESD_dt_MS1F3, expand = TRUE) +
  # theme_graph +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(), 
    # strip.text.y.right = element_text(angle = 0),
    strip.text.y = element_text(size = 10, angle = -90, hjust = 0.5, colour = "black")
  )

if (isTRUE(plot_show)) {
  print(plot_FigS7_morpho_noSA)
}

if (isTRUE(plot_save)) {
  # hi-res png
  png(
    filename = file.path(dir_plots, "fig_S99_rangesT_morpho_noSA_waterdepth_assemblage_glac.png"),
    width = 159,
    height = 180,
    units = "mm",
    bg = "white",
    res = 1200
  )
  print(plot_FigS7_morpho_noSA)
  dev.off()
}

# END ----

