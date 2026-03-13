# R script for diversity analyses and subsampling
# used to make table S3 and plots for figures 5 & S6
# LN and TWWH

# clear the decks ----
rm(list = ls())


# user parameters ----
plot_age_lims <- c(595, 530) # in Ma for reverse age axis
# shade_value <- 0.3
plot_show <- TRUE
plot_save <- TRUE
nongen <- c(
  "assemblage",
  "dumbell",
  "ostrich feather",
  "string organism"
)

## define time bins ----
start <- 590
end <- 528   # use 534 for alternative binning

age1ma  <- rev(seq(end, start, by = 1))
# age2ma  <- rev(seq(end, start, by = 2))
age5ma  <- rev(seq(end, start, by = 5))
# age10ma <- rev(seq(end, start, by = 10))

## run options ----
runs <- c("all", "noSA")

## set directory paths ----
dir_data_in <- file.path("data", "processed") # run after WH01_palaeo_table_S2.R
dir_data_out <- file.path("data", "processed")
dir_plots <- file.path("figures")

## specify files ----
infile_palaeo_name <- "WH01_SuppData_2_palaeontology_tables.xlsx" # run after WH01_palaeo_table_S2.R
infile_palaeo <- file.path(dir_data_in, infile_palaeo_name)

outfile_palaeo_name <- "WH01_SuppData_2_palaeontology_tables.xlsx"
outfile_palaeo <- file.path(dir_data_out, outfile_palaeo_name)


# load packages ----
library(divDyn)
library(magrittr)
library(dplyr)
library(openxlsx2)
library(deeptime)
library(patchwork)
library(forcats)


# custom themes and functions ----
source(file = file.path("utils", "custom_themes.R"))
source(file = file.path("utils", "ecesd_deeptime_dat.R"))
source(file = file.path("utils", "ecesd_glaciations_table.R"))


# load data ----
data_palaeo_wb <- openxlsx2::wb_load(file = infile_palaeo)
data_palaeo_in <- openxlsx2::wb_to_df(
  file = data_palaeo_wb,
  sheet = "palaeo_data",
  na.strings = "NA"
)
data_palaeo_in <- data_palaeo_in[colSums(!is.na(data_palaeo_in)) > 0]


# sort data ----
data_palaeo <- data_palaeo_in %>%
  # get analysis age
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
    )
  ) %>% 
  dplyr::filter(
    Fossil_type %in% c("biomineralized", "non-mineralized"), # remove assemblages and trace fossils
    !is.na(Rank5_genus), # make sure there is a genus id
    !(Rank5_genus %in% nongen)
  ) %>%
  dplyr::mutate(
    sample = factor(paste(Section_name, Formation, Member, Unit, Site_Surface, sep = "; ")), 
  ) %>%
  dplyr::select(
    palaeo_id, 
    sample,
    Section_name,
    Craton,
    Formation,
    Member,
    Unit,
    Site_Surface,
    plot_age,
    plot_age_max,
    plot_age_min,
    Taxon, 
    Rank5_genus
  ) 


# analysis run ----
for (run in 1:length(runs)) {
  selection <- runs[run]
  
  if (selection == "all") {
    data <- data_palaeo
  } else if (selection == "noSA") {
    data <- data_palaeo[data_palaeo$Craton!="South Australia",]
  } else {
    stop("I don't know how to subset that...") 
  }

  # assign collection numbers
  tmp1 <- levels(factor(data$sample))
  tmp2 <- as.numeric(factor(tmp1))
  tmpdf <- data.frame(sample = tmp1, collection_no = tmp2)
  
  df <- dplyr::left_join(data, tmpdf, by = "sample")
  rm(list = c("tmp1", "tmp2", "tmpdf"))
  
  # 5 Myr bins for paper
  temp <- subset(df, plot_age != "")
  a <- as.numeric(temp$plot_age)
  for (i in 1:(length(age5ma) - 1)) {
    a <- replace(a, a <= age1ma[i] & a > age5ma[i + 1], i)
  }
  
  temp$slc <- a
  temp <- subset(temp, slc != "598.25")
  temp. <- subset(temp, select = c(collection_no, Taxon, slc))
  # remove samples lacking identifiers
  temp. <- temp.[temp.$collection_no != "", ]
  
  ## diversity analysis ----
  edi_div <- divDyn(temp., tax = "Taxon", bin = "slc")
  ow <- subsample(
    temp.,
    iter = 100,
    q = 14,
    tax = "Taxon",
    bin = "slc",
    coll = "collection_no",
    output = "dist",
    type = "oxw",
    xexp = 1
  )
  uw <- subsample(
    temp.,
    iter = 100,
    q = 8,
    tax = "Taxon",
    bin = "slc",
    coll = "collection_no",
    output = "dist",
    keep = NULL,
    type = "oxw",
    xexp = 0
  )
  sqs <- subsample(
    temp.,
    iter = 100,
    q = 0.4,
    tax = "Taxon",
    bin = "slc",
    output = "dist",
    type = "sqs"
  )
  
  ## summary ----
  # div.sqs.1ma <- apply(sqs$divRT, 1, mean)
  # sd.sqs.1ma  <- apply(sqs$divRT, 1, sd)
  
  # oriPC.1ma   <- apply(sqs$oriPC, 1, mean, na.rm = TRUE)
  # sd.pc.1ma   <- apply(sqs$oriPC, 1, sd, na.rm = TRUE)
  
  # oriProp.1ma <- apply(sqs$oriProp, 1, mean, na.rm = TRUE)
  # sd.prop.1ma <- apply(sqs$oriProp, 1, sd, na.rm = TRUE)
  
  # div.1ma <- edi_div$divSIB
  # ori.1ma <- edi_div$oriPC
  
  div.sqs.5ma <- apply(sqs$divRT, 1, mean)
  sd.sqs.5ma  <- apply(sqs$divRT, 1, sd)
  
  div.ow.5ma <- apply(ow$divRT, 1, mean)
  sd.ow.5ma  <- apply(ow$divRT, 1, sd)
  
  div.uw.5ma <- apply(uw$divRT, 1, mean)
  sd.uw.5ma  <- apply(uw$divRT, 1, sd)
  
  oriPC.5ma   <- apply(sqs$oriPC, 1, mean, na.rm = TRUE)
  sd.pc.5ma   <- apply(sqs$oriPC, 1, sd, na.rm = TRUE)
  
  oriProp.5ma <- apply(sqs$oriProp, 1, mean, na.rm = TRUE)
  sd.prop.5ma <- apply(sqs$oriProp, 1, sd, na.rm = TRUE)
  
  div.5ma <- edi_div$divSIB
  ori.5ma <- edi_div$oriPC
  
  # collection and formation counts
  coll <- unique(subset(temp., select = c(collection_no, slc)))
  ncoll <- table(coll$slc)
  temp.. <- unique(subset(temp, select = c(Formation, Taxon, slc)))
  fm <- unique(subset(temp.., select = c(Formation, slc)))
  nfm <- table(fm$slc)
  
  # age bins
  # age.1ma <- numeric(length(age1ma) - 1)
  # for (i in 1:(length(age1ma) - 1)) {
  #   age.1ma[i] <- (age1ma[i] + age1ma[i + 1]) / 2
  # }
  # nc.1ma <- numeric()
  # for (i in 1:62) {
  #   coll. <- unique(subset(coll, slc == i))
  #   nc.1ma[i] <- nrow(coll.)
  # }
  
  age.5ma <- numeric(length(age5ma) - 1)
  for (i in 1:(length(age5ma) - 1)) {
    age.5ma[i] <- (age5ma[i] + age5ma[i + 1]) / 2
  }
  nc.5ma <- numeric()
  for (i in 1:12) {
    coll. <- unique(subset(coll, slc == i))
    nc.5ma[i] <- nrow(coll.)
  }
  nf.5ma <- numeric()
  for (i in 1:12) {
    fm. <- unique(subset(fm, slc == i))
    nf.5ma[i] <- nrow(fm.)
  }
  
  ## make output table ----
  subset_table_s3 <- data.frame(
    age_ma = age.5ma,
    subset = selection, 
    raw_richness_diversity_value = div.5ma,
    raw_collections_diversity_value = nc.5ma,
    raw_formations_diversity_value = nf.5ma,
    subsampled_SQS_diversity_value = div.sqs.5ma,
    subsampled_SQS_diversity_sd = sd.sqs.5ma,
    subsampled_OW_diversity_value = div.ow.5ma,
    subsampled_OW_diversity_sd = sd.ow.5ma,
    subsampled_UW_diversity_value = div.uw.5ma,
    subsampled_UW_diversity_sd = sd.uw.5ma,
    subsampled_Foote_rate_value = oriPC.5ma,
    subsampled_Foote_rate_sd = sd.pc.5ma,
    subsampled_Proportional_rate_value = oriProp.5ma,
    subsampled_Proportional_rate_sd = sd.prop.5ma
  )
  if (run == 1) {
    data_table_s3 <- subset_table_s3 
  } else {
    data_table_s3 <- rbind(data_table_s3, subset_table_s3)
  }
}

data_palaeo_wb$add_worksheet("Table_S3")
data_palaeo_wb$add_data(sheet = "Table_S3", x = data_table_s3)
openxlsx2::wb_save(data_palaeo_wb, file = outfile_palaeo)


# make plots ----
# making four subplots for one panel plot here
# a. richness, formations, collections
# b. subsampled richness
# c. per capita origination rate
# d. Foote's origination rate (SQS)

## sort for plot ----
data_to_plot <- data_table_s3 %>%
  tidyr::pivot_longer(cols = starts_with(c("raw", "subsampled")),
                      names_to = c("type", "variable", "Metric", "stat"),
                      names_sep = "_",
                      values_to = "value"
                      ) %>%
  tidyr::pivot_wider(names_from = "stat", 
                     values_from = "value")


data_all_5myr <- data_to_plot %>% 
  dplyr::filter(subset == "all") %>% 
  dplyr::mutate(bin_width = "5Myr")

data_noSA_5myr <- data_to_plot %>% 
  dplyr::filter(subset == "noSA") %>% 
  dplyr::mutate(bin_width = "5Myr")



## Figure 5 all data ----
### 5a raw counts ----
plot_raw_counts <- ggplot() +
  # add glacial intervals
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
  # add lines for raw data
  geom_line(data = data_all_5myr %>% dplyr::filter(type == "raw"),
            aes(x = age_ma, y = value, linetype = variable),
            colour = "midnightblue", linewidth = 0.75) +
  # add points for raw data
  geom_point(data = data_all_5myr %>% dplyr::filter(type == "raw"),
             aes(x = age_ma, y = value, shape = variable),
             colour = "midnightblue", size = 1) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 5)) +
  scale_shape_manual(values = c("richness" = 21, "formations" = 22, "collections" = 23),
                       guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
  scale_linetype_manual(values = c("richness" = "solid", "formations" = "dotted", "collections" = "dashed"),
                          guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Raw count",
    shape = "Metric",
    linetype = "Metric"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_raw_counts)
}

### 5b subsampled richness ----
plot_ss_richness <- ggplot() +
  # add glacial intervals
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
  # add value lines for subsampled diversity data
  geom_line(
    data = data_all_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, y = value, colour = variable, linetype = variable),
    linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_all_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd, colour = variable),
    linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_all_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, y = value, colour = variable, shape = variable),
    size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 5)) +
  scale_color_viridis_d(option = "mako", begin = 0, end = 0.75, guide = guide_legend(byrow = TRUE)) +
  scale_shape_manual(values = c("SQS" = 21, "OW" = 22, "UW" = 23), guide = guide_legend(byrow = TRUE)) +
  scale_linetype_manual(values = c("SQS" = "solid", "OW" = "dotted", "UW" = "dashed"), 
                        guide = guide_legend(byrow = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Subsampled diversity",
    colour = "Metric",
    shape = "Metric",
    linetype = "Metric"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_ss_richness)
}

### 5c per capita origination rate ----
plot_pc_origination <- ggplot() +
  # add glacial intervals
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
  # add value lines for subsampled diversity data
  geom_line(
    data = data_all_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_all_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd),
    colour = "midnightblue", linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_all_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous() +
  labs(
    x = "Age (Ma)",
    y = "Raw per capita origination rate"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_pc_origination)
}

### 5d Foote's origination rate (SQS) ----
plot_Foote_origination <- ggplot() +
  # add glacial intervals
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
  # add value lines for subsampled diversity data
  geom_line(
    data = data_all_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", linetype = "solid", linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_all_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd),
    colour = "midnightblue", linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_all_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous() +
  labs(
    x = "Age (Ma)",
    y = "Foote's origination rate (SQS; 0.4)"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_Foote_origination)
}

### Figure 5 panel plot ----
plot_fig5_panel <- (plot_raw_counts + labs(tag = "a")) +
    (plot_ss_richness + labs(tag = "b")) +
    (plot_pc_origination + labs(tag = "c")) +
    (plot_Foote_origination + labs(tag = "d"))

if (isTRUE(plot_save)) {
  png(
    filename = file.path(dir_plots, "fig_5_diversity_panel.png"),
    width = 250,
    height = 159,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_fig5_panel)
  dev.off()
}


## Figure S6 without South Australia ----
### S6a raw counts ----
plot_raw_counts_noSA <- ggplot() +
  # add glacial intervals
  geom_rect(data = ecesd_glaciations, 
            aes(xmin = Age_min, xmax = Age_max, ymin = -Inf, ymax = Inf),
            alpha = 0.3, fill = "steelblue", colour = "steelblue"
  ) +
  # add lines for raw data
  geom_line(data = data_noSA_5myr %>% dplyr::filter(type == "raw"),
            aes(x = age_ma, y = value, linetype = variable),
            colour = "midnightblue", linewidth = 0.75) +
  # add points for raw data
  geom_point(data = data_noSA_5myr %>% dplyr::filter(type == "raw"),
             aes(x = age_ma, y = value, shape = variable),
             colour = "midnightblue", size = 1) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 5)) +
  scale_shape_manual(values = c("richness" = 21, "formations" = 22, "collections" = 23),
                     guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
  scale_linetype_manual(values = c("richness" = "solid", "formations" = "dotted", "collections" = "dashed"),
                        guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Raw count",
    shape = "Metric",
    linetype = "Metric"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_raw_counts_noSA)
}

### S6b subsampled richness ----
plot_ss_richness_noSA <- ggplot() +
  # add glacial intervals
  geom_rect(data = ecesd_glaciations, 
            aes(xmin = Age_min, xmax = Age_max, ymin = -Inf, ymax = Inf),
            alpha = 0.3, fill = "steelblue", colour = "steelblue"
  ) +
  # add value lines for subsampled diversity data
  geom_line(
    data = data_noSA_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, y = value, colour = variable, linetype = variable),
    linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_noSA_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd, colour = variable),
    linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_noSA_5myr %>% dplyr::filter(type == "subsampled", Metric == "diversity"),
    aes(x = age_ma, y = value, colour = variable, shape = variable),
    size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 5)) +
  scale_color_viridis_d(option = "mako", begin = 0, end = 0.75, guide = guide_legend(byrow = TRUE)) +
  scale_shape_manual(values = c("SQS" = 21, "OW" = 22, "UW" = 23), guide = guide_legend(byrow = TRUE)) +
  scale_linetype_manual(values = c("SQS" = "solid", "OW" = "dotted", "UW" = "dashed"), 
                        guide = guide_legend(byrow = TRUE)) +
  labs(
    x = "Age (Ma)",
    y = "Subsampled diversity",
    colour = "Metric",
    shape = "Metric",
    linetype = "Metric"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_ss_richness_noSA)
}

### S6c per capita origination rate ----
plot_pc_origination_noSA <- ggplot() +
  # add glacial intervals
  geom_rect(data = ecesd_glaciations,
            aes(xmin = Age_min, xmax = Age_max, ymin = -Inf, ymax = Inf),
            alpha = 0.3, fill = "steelblue", colour = "steelblue"
  ) +
  # add value lines for subsampled diversity data
  geom_line(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd),
    colour = "midnightblue", linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Proportional"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous() +
  labs(
    x = "Age (Ma)",
    y = "Raw per capita origination rate"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_pc_origination_noSA)
}

### S6d Foote's origination rate (SQS) ----
plot_Foote_origination_noSA <- ggplot() +
  # add glacial intervals
  geom_rect(data = ecesd_glaciations,
            aes(xmin = Age_min, xmax = Age_max, ymin = -Inf, ymax = Inf),
            alpha = 0.3, fill = "steelblue", colour = "steelblue"
  ) +
  # add value lines for subsampled diversity data
  geom_line(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", linetype = "solid", linewidth = 0.75
  ) +
  # add error bars for point data
  geom_errorbar(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, ymin = value - sd, ymax = value + sd),
    colour = "midnightblue", linetype = "solid", linewidth = 0.5, width = 0.4
  ) +
  # add points for raw data
  geom_point(
    data = data_noSA_5myr %>% dplyr::filter(variable == "Foote"),
    aes(x = age_ma, y = value),
    colour = "midnightblue", size = 1
  ) +
  # add formatting
  scale_x_reverse(limits = plot_age_lims, expand = c(0, 0)) +
  scale_y_continuous() +
  labs(
    x = "Age (Ma)",
    y = "Foote's origination rate (SQS; 0.4)"
  ) +
  coord_geo(pos = "bottom", dat = ECESD_dt_MS1F3, expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        legend.spacing.y = unit(1, "mm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)
  )

if(isTRUE(plot_show)){
  print(plot_Foote_origination_noSA)
}

### Figure S6 panel plot ----
plot_figS6_panel <- (plot_raw_counts_noSA + labs(tag = "a")) +
  (plot_ss_richness_noSA + labs(tag = "b")) +
  (plot_pc_origination_noSA + labs(tag = "c")) +
  (plot_Foote_origination_noSA + labs(tag = "d"))

if (isTRUE(plot_save)) {
  png(
    filename = file.path(dir_plots, "fig_S6_diversity_panel_noSA.png"),
    width = 250,
    height = 159,
    units = "mm",
    bg = "white",
    res = 450
  )
  print(plot_figS6_panel)
  dev.off()
}

# END ----

