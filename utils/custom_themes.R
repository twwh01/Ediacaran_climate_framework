# Custom ggplot themes and colours for the ECESD project

# load ggplot2 ----
library(ggplot2)


# graphs theme ----
theme_graph <- theme_minimal() + 
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank())


# maps theme ----
theme_map <- theme_minimal() + 
  theme(axis.title = element_text(size = 20),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank())


# standard plot colours and fills ----
## alpha for glacial intervals
alpha_glac <- 0.3
## alpha for boxplot boxes
alpha_box <- 0.3
## alpha for points
alpha_pts <- 0.75
## linewidths
size_lines <- 1
## point siex
size_points <- 1
## text size
size_text <- 1
## fill glacials boxes
fill_glac <- "steelblue"
## colour for points if not coloured by time interval
col_points <- "#008080"
## colour for smoothed lines
col_smooth <- "grey25"
## colour for boxplot lines
col_box <- "grey25"
## Ghent blue
col_Gblue <- "#1E64C8"
## Ghent gold
col_Ggold <- "#FFD200"
## Ghent teal
col_Gteal <- "#2D8CA8"
## time slab fill
fill_TS <- "#FFD200"