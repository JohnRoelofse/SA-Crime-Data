crimes <- function(data, x, y, z){
  
  data %>% filter(province == x & police_station == y & crime == z) %>% 
    group_by(year) %>% 
    summarise(n = sum(incidents)) %>% 
    ggplot() +
    geom_line(aes(year, n)) +
    ggtitle("Number of Incidents over Time") +
    theme(plot.title = element_text(size = 20),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 16)) +
    ylab("") +
    xlab("Year") +
    scale_x_discrete(limits = c(2005:2014))
}


