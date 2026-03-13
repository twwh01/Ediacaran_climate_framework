# custom geological interval data.frames for using with deeptime::coord_geo()
# author: TWWH
# where available dates are from the International Chronostratigraphic Chart v2024-12

# custom functions ----
source(file = file.path("utils", "lookup_vector.R"))

# ICC intervals ----
## Eons ----
ECESD_eons <- c("Proterozoic", "Phanerozoic")

## Eras ----
ECESD_eras <- c("Neoproterozoic",
                "Palaeozoic")

## Periods ----
ECESD_periods <-  c("Cryogenian",
                    "Ediacaran",
                    "Cambrian")

## Epochs ----
ECESD_epochs <- c(
  "Cryogenian",
  "Ediacaran",
  "Terreneuvian",
  "Epoch 2",
  "Miaolingian",
  "Furongian"
)

## Ages ----
ECESD_ages <- c(
  "Cryogenian",
  "Ediacaran",
  "Fortunian",
  "Age 2",
  "Age 3",
  "Age 4",
  "Wuliuan",
  "Drumian",
  "Guzhangian",
  "Paibian",
  "Jiangshanian",
  "Age 10"
)

## MS1F1 ----
ECESD_MS1F1 <- c(
  "Cr",
  "Ed",
  "Cm"
)

## MS1F3 ----
ECESD_MS1F3 <- c(
  "Ed",
  "Cm"
)

## ICC abbreviations ----
ECESD_abbr <- c(
  # eons
  "Proterozoic" = "PRO",
  "Phanerozoic" = "PHA",
  # eras
  "Neoproterozoic" = "Neo",
  "Palaeozoic" = "Pal",
  # periods
  "Cryogenian" = "Cr",
  "Ediacaran" = "Ed",
  "Cambrian" = "Cm",
  # epochs
  "Terreneuvian" = "Ter",
  "Epoch 2" = "Ep 2",
  "Miaolingian" = "Mia.",
  "Furongian" = "Fur",
  # ages
  "Fortunian" = "For",
  "Age 2" = "Ag2",
  "Age 3" = "Ag3",
  "Age 4" = "Ag4",
  "Wuliuan" = "Wul",
  "Drumian" = "Dru",
  "Guzhangian" = "Guz",
  "Paibian" = "Pai",
  "Jiangshanian" = "Jia",
  "Age 10" = "Ag10",
  # MS1F1
  "Cr" = "Cr",
  "Ed" = "Ed",
  "Cm" = "Cm"
)

## Dates: ICC v2024-12 ----
# early and late Ediacaran informally defined by end of Gaskiers glaciation
ECESD_age_max <- c(
  # eons
  "Proterozoic" = 2500,
  "Phanerozoic" = 538.8,
  # eras
  "Neoproterozoic" = 1000,
  "Palaeozoic" = 538.8,
  # periods
  "Cryogenian" = 720,
  "Ediacaran" = 635,
  "Cambrian" = 538.8,
  # epochs
  "Terreneuvian" = 538.8,
  "Epoch 2" = 521,
  "Miaolingian" = 506.5,
  "Furongian" = 497,
  # ages
  "Fortunian" = 538.8,
  "Age 2" = 529,
  "Age 3" = 521,
  "Age 4" = 514,
  "Wuliuan" = 506.5,
  "Drumian" = 504.5,
  "Guzhangian" = 500.5,
  "Paibian" = 497,
  "Jiangshanian" = 494,
  "Age 10" = 489.5,
  # MS1F1
  "Cr" = 650,
  "Ed" = 635,
  "Cm" = 538.8
)

ECESD_age_min <- c(
  # eons
  "Proterozoic" = 538.8,
  "Phanerozoic" = 0,
  # eras
  "Neoproterozoic" = 538.8,
  "Palaeozoic" = 251.902,
  # periods
  "Cryogenian" = 635,
  "Ediacaran" = 538.8,
  "Cambrian" = 485.4,
  # epochs
  "Terreneuvian" = 521,
  "Epoch 2" = 506.5,
  "Miaolingian" = 497,
  "Furongian" = 485.4,
  # ages
  "Fortunian" = 529,
  "Age 2" = 521,
  "Age 3" = 514,
  "Age 4" = 506.5,
  "Wuliuan" = 504.5,
  "Drumian" = 500.5,
  "Guzhangian" = 497,
  "Paibian" = 494,
  "Jiangshanian" = 489.5,
  "Age 10" = 485.4,
  # MS1F1
  "Cr" = 635,
  "Ed" = 538.8,
  "Cm" = 510 # a useful mid point for the figure
)

## Colours ----
ECESD_colours <- c(
  # eons
  "Proterozoic" = "#F73563",
  "Phanerozoic" = "#9AD9DD",
  # eras
  "Neoproterozoic" = "#FEB342",
  "Palaeozoic" = "#99C08D",
  # periods
  "Cryogenian" = "#FECC5C",
  "Ediacaran" = "#FED96A",
  "Cambrian" = "#7FA056",
  # epochs
  "Terreneuvian" = "#8CB06C",
  "Epoch 2" = "#99C078",
  "Miaolingian" = "#A6CF86",
  "Furongian" = "#B3E095",
  # ages
  "Fortunian" = "#99B575",
  "Age 2" = "#A6BA80",
  "Age 3" = "#A6C583",
  "Age 4" = "#B3CA8E",
  "Wuliuan" = "#B3D492",
  "Drumian" = "#BFD99D",
  "Guzhangian" = "#CCDFAA",
  "Paibian" = "#CCEBAE",
  "Jiangshanian" = "#D9F0BB",
  "Age 10" = "#E6F5C9",
  # MS1F1
  "Cr" = "#FECC5C",
  "Ed" = "#FED96A",
  "Cm" = "#7FA056"
)


# data.frames for deeptime ----
## age level ----
ECESD_dt_ages <- data.frame(name = ECESD_ages)
ECESD_dt_ages$max_age <- lookup_vector(vec = ECESD_age_max,
                                       value = ECESD_dt_ages$name)
ECESD_dt_ages$min_age <- lookup_vector(vec = ECESD_age_min,
                                       value = as.character(ECESD_dt_ages$name))
ECESD_dt_ages$abbr <- lookup_vector(vec = ECESD_abbr,
                                    value = as.character(ECESD_dt_ages$name))
ECESD_dt_ages$color <- lookup_vector(vec = ECESD_colours,
                                     value = as.character(ECESD_dt_ages$name))

## epoch level ----
ECESD_dt_epochs <- data.frame(name = ECESD_epochs)
ECESD_dt_epochs$max_age <- lookup_vector(vec = ECESD_age_max,
                                         value = as.character(ECESD_dt_epochs$name))
ECESD_dt_epochs$min_age <- lookup_vector(vec = ECESD_age_min,
                                         value = as.character(ECESD_dt_epochs$name))
ECESD_dt_epochs$abbr <- lookup_vector(vec = ECESD_abbr,
                                      value = as.character(ECESD_dt_epochs$name))
ECESD_dt_epochs$color <- lookup_vector(vec = ECESD_colours,
                                       value = as.character(ECESD_dt_epochs$name))

## period level ----
ECESD_dt_periods <- data.frame(name = ECESD_periods)
ECESD_dt_periods$max_age <- lookup_vector(vec = ECESD_age_max,
                                          value = as.character(ECESD_dt_periods$name))
ECESD_dt_periods$min_age <- lookup_vector(vec = ECESD_age_min,
                                          value = as.character(ECESD_dt_periods$name))
ECESD_dt_periods$abbr <- lookup_vector(vec = ECESD_abbr,
                                       value = as.character(ECESD_dt_periods$name))
ECESD_dt_periods$color <- lookup_vector(vec = ECESD_colours,
                                        value = as.character(ECESD_dt_periods$name))

## era level ----
ECESD_dt_eras <- data.frame(name = ECESD_eras)
ECESD_dt_eras$max_age <- lookup_vector(vec = ECESD_age_max,
                                       value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$min_age <- lookup_vector(vec = ECESD_age_min,
                                       value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$abbr <- lookup_vector(vec = ECESD_abbr,
                                    value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$color <- lookup_vector(vec = ECESD_colours,
                                     value = as.character(ECESD_dt_eras$name))

## eon level ----
ECESD_dt_eras <- data.frame(name = ECESD_eons)
ECESD_dt_eras$max_age <- lookup_vector(vec = ECESD_age_max,
                                       value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$min_age <- lookup_vector(vec = ECESD_age_min,
                                       value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$abbr <- lookup_vector(vec = ECESD_abbr,
                                    value = as.character(ECESD_dt_eras$name))
ECESD_dt_eras$color <- lookup_vector(vec = ECESD_colours,
                                     value = as.character(ECESD_dt_eras$name))

## ECESD_MS1_Fig1 ----
ECESD_dt_MS1F1 <- data.frame(name = ECESD_MS1F1)
ECESD_dt_MS1F1$max_age <- lookup_vector(vec = ECESD_age_max,
                                       value = as.character(ECESD_dt_MS1F1$name))
ECESD_dt_MS1F1$min_age <- lookup_vector(vec = ECESD_age_min,
                                       value = as.character(ECESD_dt_MS1F1$name))
ECESD_dt_MS1F1$abbr <- lookup_vector(vec = ECESD_abbr,
                                    value = as.character(ECESD_dt_MS1F1$name))
ECESD_dt_MS1F1$color <- lookup_vector(vec = ECESD_colours,
                                     value = as.character(ECESD_dt_MS1F1$name))

## ECESD_MS1_Fig3 ----
ECESD_dt_MS1F3 <- data.frame(name = ECESD_MS1F3)
ECESD_dt_MS1F3$max_age <- c(595, 538.8)
ECESD_dt_MS1F3$min_age <- c(538.8, 530)
ECESD_dt_MS1F3$abbr <- c("Ed", "Cm")
ECESD_dt_MS1F3$color <- lookup_vector(vec = ECESD_colours,
                                      value = as.character(ECESD_dt_MS1F3$name))
