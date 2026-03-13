map_for_deposits <- function(deposit_data,
                             deposit_data_mapping,
                             deposit_data_shape = 23,
                             deposit_data_colour = "black",
                             deposit_data_fill = "orange",
                             
                             map_edge = map_edge,
                             map_edge_colour = "black",
                             map_edge_fill = "steelblue",
                             map_edge_alpha = 0.3,
                             
                             
                             palaeomap = palaeomap,
                             palaeomap_colour = "grey50",
                             palaeomap_fill = "#088080",
                             palaeomap_alpha = 0.5,
                             
                             this_title = waiver(),
                             this_subtitle = waiver(),
                             
                             this_size_values = waiver(),
                             this_size_lab = waiver(),
                             
                             this_colour_values = waiver(),
                             this_colour_lab = waiver(),
                             
                             this_alpha_values = waiver(),
                             this_alpha_lab = waiver(),
                             
                             plot_theme = theme_minimal(),
                             added_themes = waiver(),
                             
                             # must be a list of ggproto objects
                             underlying_annotations = list(),
                             # must be a list of ggproto objects
                             overlying_annotations = list(),
                             
                             this_crs = st_crs("+proj=robin"),
                             this_default_crs = sf::st_crs(4326)) {
                             
  this_map <-
    # initialise plot object
    ggplot() +
    
    # add scales
    scale_alpha_manual(values = this_alpha_values, drop = FALSE, name = this_alpha_lab) +
    scale_size_manual(values = this_size_values, drop = FALSE, name = this_size_lab) +
    # scale_colour_manual(values = this_colour_values, name = this_colour_lab) +
    
    # add labels
    labs(
      title = this_title,
      subtitle = this_subtitle
    ) +
    
    # add map edge
    geom_sf(
      data = map_edge,
      colour = map_edge_colour,
      fill = map_edge_fill,
      alpha = map_edge_alpha
    ) +
    
    # add map
    geom_sf(
      data = palaeomap,
      colour = palaeomap_colour,
      fill = palaeomap_fill,
      alpha = palaeomap_alpha
    ) +
    
    # add underlying annotations
    underlying_annotations + 
    
    # add deposit data
    geom_sf(
      data = deposit_data,
      mapping = deposit_data_mapping, 
      shape = deposit_data_shape,
      colour = deposit_data_colour,
      fill = deposit_data_fill,
      show.legend = TRUE
    ) +
    
    # add overlying annotations
    overlying_annotations +
    
    # add themes
    plot_theme + 
    added_themes +
    
    # add coordinate system
    coord_sf(
      xlim = c(-180, 180),
      ylim = c(-90, 90),
      crs = this_crs,
      default_crs = this_default_crs
    )
  
  return(this_map)
}