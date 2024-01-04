# function for output plot

# (but see https://stackoverflow.com/questions/63722900/ggplot-shape-not-matching-legend 
# which says you should be transforming/melting data!)

output.plot <- function(data, mylocation, year.text, caption.text, display.ratio) {
  
  # create ggplot 
  ggplot(data) +
    # scale_y_continuous(labels = scales::comma) + 
    
    # trains - volume
    geom_line(aes(x = fin_year_comp, 
                  y = train.volume / display.ratio, ## data transform for appearance
                  colour ="train"),
              linewidth = 2) +
    geom_point(aes(x = fin_year_comp, 
                   y = train.volume / display.ratio, ## data transform for appearance
                   colour = "train"),
               size = 4) +
    
    # trams - volume with capacity adjustment
    geom_line(aes(x = fin_year_comp,  
                  y = train.volume.capadj / display.ratio, ## data transform for appearance
                  colour = "train.cap"),
              linewidth = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = train.volume.capadj / display.ratio, ## data transform for appearance
                   colour = "train.cap"),
               size = 2) +
    
    # trams - volume
    geom_line(aes(x = fin_year_comp, 
                  y = tram.volume / display.ratio, ## data transform for appearance
                  colour ="tram"),
              linewidth = 2) +
    geom_point(aes(x = fin_year_comp, 
                   y = tram.volume / display.ratio, ## data transform for appearance
                   colour = "tram"),
               size = 4) +
    
    # trams - volume with capacity adjustment
    geom_line(aes(x = fin_year_comp,  
                  y = tram.volume.capadj / display.ratio, ## data transform for appearance
                  colour = "tram.cap"),
              linewidth = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = tram.volume.capadj / display.ratio, ## data transform for appearance
                   colour = "tram.cap"),
               size = 2) +
    
    # buses - volume
    geom_line(aes(x = fin_year_comp, 
                  y = bus.volume / display.ratio, ## data transform for appearance
                  colour ="bus"),
              linewidth = 2) +
    geom_point(aes(x = fin_year_comp, 
                   y = bus.volume / display.ratio, ## data transform for appearance
                   colour = "bus"),
               size = 4) +
    
    # buses - volume with capacity adjustment
    geom_line(aes(x = fin_year_comp,  
                  y = bus.volume.capadj / display.ratio, ## data transform for appearance
                  colour = "bus.cap"),
              linewidth = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = bus.volume.capadj / display.ratio, ## data transform for appearance
                   colour = "bus.cap"),
               size = 2) +
    
    # apartments
    geom_line(aes(x = fin_year_comp,  
                  y = cum_hi_dens_dwel,
                  colour = "apt"),
              linewidth = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = cum_hi_dens_dwel,
                   colour = "apt"),
               size = 2) +
    
    scale_x_continuous(breaks = seq(min(data$fin_year_comp), 
                                    max(data$fin_year_comp),
                                    by = 2),
                       labels = transform.fin.yr(seq(min(data$fin_year_comp), 
                                                     max(data$fin_year_comp),
                                                     by = 2))) +
    
    scale_y_continuous(name = "Cumulative number of apartments",
                       # # next 2 lines are to have y-axis start at 0
                       # limits = c(0, NA),  # lowest figure 0 for y axis
                       # expand = expansion(mult = c(0, 0.05)), # white space at top & 
                       # # bottom - zero for bottom, 5% (default) for top
                       labels = scales::comma,
                       sec.axis = sec_axis(trans = ~.*display.ratio,  # transformation is reverse of above
                                           name = "Annual bus and tram services",
                                           labels = scales::comma)) +
    # See https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot, but doesn't work
    scale_colour_manual("", 
                        labels = c("Apartments (LHS)",
                                   "Annual bus services (RHS)",
                                   "Annual bus services, adjusted for capacity (RHS)",
                                   "Annual train services (RHS)",
                                   "Annual train services, adjusted for capacity (RHS)",
                                   "Annual tram services (RHS)",
                                   "Annual tram services, adjusted for capacity (RHS)"
                        ),
                        values = c("apt" = "black",
                                   "train" = brewer.pal(n = 6, name = 'Paired')[1],
                                   "train.cap" = brewer.pal(n = 6, name = 'Paired')[2],
                                   "tram" = brewer.pal(n = 6, name = 'Paired')[3],
                                   "tram.cap" = brewer.pal(n = 6, name = 'Paired')[4],
                                   "bus" = brewer.pal(n = 6, name = 'Paired')[5],
                                   "bus.cap" = brewer.pal(n = 6, name = 'Paired')[6])) +
    
    # guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(colour = guide_legend(nrow = 3, bycol = TRUE)) +
    
    labs(x = "Financial year of completion",
         # y = "Cumulative number of apartments and other high density dwellings",
         title = "Annual change in apartment numbers and bus, tram and train services",
         subtitle = paste0(mylocation, ", ", year.text),
         caption = paste(caption.text)) +
    
    theme_bw() +
    
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0))
}


# function to transform financial year from '2021' to '2020/21' format
transform.fin.yr <- function(year) {
  return(paste0(year - 1, "/", substr(as.character(year), 3, 4)))
}

