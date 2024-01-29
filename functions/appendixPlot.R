# plot of data values for working paper appendix (based on dashboard.Rmd plot)

appendixPlot <- function(filtered_data) {
  # filtered_data = data.tt
  
  # extract the bases (the starting year; but may be first available
  # year for services that don't start until a different year)
  apt.base <- filtered_data %>% filter(Year == min(Year)) %>% pull(Apartments)
  pop.base <- filtered_data %>% filter(Year == min(Year)) %>% pull(Population)
  
  serv.base <- filtered_data %>% 
    filter(service > 0) %>%
    filter(Year == min(Year)) %>%
    pull(service)
  
  serv_capadj.base <- filtered_data %>% 
    filter(service.capadj > 0) %>%
    filter(Year == min(Year)) %>% 
    pull(service.capadj)
  
  # ranges for y axis (excluding 0s in service or service.capadj)
  apt.values <- filtered_data$Apartments
  pop.values <- filtered_data$Population
  serv.values <- filtered_data$service
  serv_capadj.values <- filtered_data$service.capadj
  # lowest non-zero values if any, or else 100
  min.apt <- ifelse(max(apt.values) > 0,
                    min(apt.values[apt.values > 0]) / apt.base * 100, 
                    100)
  min.pop <- ifelse(max(pop.values) > 0,
                    min(pop.values[pop.values > 0]) / pop.base * 100, 
                    100)
  min.serv <- ifelse(max(serv.values) > 0,
                     min(serv.values[serv.values > 0]) / serv.base * 100, 
                     100)
  min.serv_capadj <- ifelse(max(serv_capadj.values) > 0,
                            min(serv_capadj.values[serv_capadj.values > 0]) / serv_capadj.base * 100, 
                            100)
  # min and max for y axis
  min_y <- min(min.apt, min.pop, min.serv, min.serv_capadj)
  max_y <- max(apt.values / apt.base * 100,
               pop.values / pop.base * 100,
               serv.values / serv.base * 100,
               serv_capadj.values / serv_capadj.base * 100)
  
  
  # Create an empty plot
  p <- plot_ly()
  
  # Add a blank trace to ensure the legend is always present
  p <- p %>%
    add_trace(x = numeric(0), y = numeric(0), 
              type = "scatter", mode = "none", 
              name = "Legend Placeholder",
              showlegend = TRUE)
  
  
  p <- p %>%
    add_trace(data = filtered_data %>%
                mutate(index = Apartments / apt.base * 100,
                       change = index - 100),
              x = ~ Year, y = ~ Apartments / apt.base * 100, 
              type = "bar",
              marker = list(color = "#f2f2f2",
                            line = list(color = "#cccccc", width = 0.5)),
              name = "Apartments"  # Legend label
    )
  
  p <- p %>%
    add_trace(data = filtered_data %>%
                mutate(index = Population / pop.base * 100,
                       change = index - 100),
              x = ~ Year, y = ~ index,
              type = "scatter", mode = "lines+markers",
              marker = list(color = "#6A3D9A", size = 8, symbol = "square"),
              line = list(color = "#6A3D9A", width = 2),
              name = "Population within 800m walking distance"  # Legend label
    )
  
  p <- p %>%
    add_trace(data = filtered_data %>% 
                filter(service > 0) %>%
                mutate(index = service / serv.base * 100,
                       change = index - 100),
              x = ~ Year, y = ~ index,
              type = "scatter", mode = "lines+markers",
              marker = list(color = "#FF7F00", size = 12),
              line = list(color = "#FF7F00", width = 4),
              name = "Annual services"  # Legend label
    )
  
  p <- p %>%
    add_trace(data = filtered_data %>% 
                filter(service.capadj > 0) %>%
                mutate(index = service.capadj / serv_capadj.base * 100,
                       change = index - 100),
              x = ~ Year, y = ~ index,
              type = "scatter", mode = "lines+markers",
              marker = list(color = "#33A02C", size = 6),
              line = list(color = "#33A02C", width = 2),
              name = "Annual services, capacity adjusted"  # Legend label
    )
  
  
  # values for x-axis (last 2 digits of year only)
  x_values <- filtered_data$Year
  x_labels <- substring(x_values, 3, 4)
  
  p <- p %>%
    layout(xaxis = list(
      tickvals = x_values,  # values always present
      ticktext = x_labels,
      title = "Financial Year ending 30 June"),
      yaxis = list(title = "Values indexed to starting year = 100",
                   range = list(min_y - 5, max_y + 5)),
      
      # legend centered at top
      legend = list(orientation = "h",  # show entries horizontally
                    xanchor = "center",  # use centre of legend as anchor
                    x = 0.5, y = 1.1,   # put legend in centre at top
                    bgcolor = "rgba(0, 0, 0, 0)")  # transparent background
      
    ) 
  
  return(p)
  
}


