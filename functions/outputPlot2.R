# alternative function for output plot - in development, not finished

# Aim is to implement https://stackoverflow.com/questions/63722900/ggplot-shape-not-matching-legend 
# which says you should be transforming/melting data.
# It works, EXCEPT that I can't get the point and line sizes to be different 
# (point size needs to be double line size)

library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(dplyr)

# dataX <- read.csv("../Tables/output_data_LGA_Boroondara.csv")
# mylocation <- "Boroondara"
# year.text <- "2004-2022"
# caption.text <- "Data sources: DTP (annual apartments), ABS census 2006 (apartment baseline), DTP (train and tram services volumes, train capacities), Yarra Trams (tram capacities), 
# GTFS data (bus service volumes), DTP and bus operators (bus capacities). 'Apartments' are dwellings classified by DTP as 'attached 4 storey or more' (to 2016) 
# or 'apartments'  (from 2017), and other developments with 100 dwellings per hectare or more. Apartments for the 2022 financial year include July to December 2021 only.  
# Bus and tram services are those operating within 800m walking distance of the relevant apartments."
# display.ratio <- 30
# 
# data <- dataX %>%
#   # omit 'hi_dens_dwel', which is not plotted
#   dplyr::select(-c("hi_dens_dwel")) %>%
#   # divide all except apartments by display ratio
#   mutate(across(contains("volume"), ~./display.ratio)) %>%
#   # convert to long form
#   pivot_longer(cols = -c("fin_year_comp"),
#                names_to = "variable",
#                values_to = "value") %>%
#   # put variable into factors in required order
#   mutate(variable = factor(variable, levels = c("cum_hi_dens_dwel", 
#                                                 "train.volume", "train.volume.capadj",
#                                                 "tram.volume", "tram.volume.capadj",
#                                                 "bus.volume", "bus.volume.capadj")))

output.plot2 <- function(data, mylocation, year.text, caption.text, display.ratio) {
  
  # create ggplot 
  ggplot(data, aes(x = fin_year_comp,
                   y = value,
                   group = variable,
                   colour = variable)) +
    # scale_y_continuous(labels = scales::comma) + 
    
    geom_line(aes(size = variable)) +
    geom_point(aes(size = variable)) +
    

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
                                   "Annual train services (RHS)",
                                   "Annual train services, adjusted for capacity (RHS)",
                                   "Annual tram services (RHS)",
                                   "Annual tram services, adjusted for capacity (RHS)",
                                   "Annual bus services (RHS)",
                                   "Annual bus services, adjusted for capacity (RHS)"
                        ),
                        values = c("black",
                                   brewer.pal(n = 6, name = 'Paired')[1],
                                   brewer.pal(n = 6, name = 'Paired')[2],
                                   brewer.pal(n = 6, name = 'Paired')[3],
                                   brewer.pal(n = 6, name = 'Paired')[4],
                                   brewer.pal(n = 6, name = 'Paired')[5],
                                   brewer.pal(n = 6, name = 'Paired')[6])) +
    
    scale_size_manual("",
                      labels = c("Apartments (LHS)",
                                 "Annual train services (RHS)",
                                 "Annual train services, adjusted for capacity (RHS)",
                                 "Annual tram services (RHS)",
                                 "Annual tram services, adjusted for capacity (RHS)",
                                 "Annual bus services (RHS)",
                                 "Annual bus services, adjusted for capacity (RHS)"
                      ),
                      values = c(2, 4, 2, 4, 2, 4, 2)) +
    
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

