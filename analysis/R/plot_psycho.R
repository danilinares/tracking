plot_psycho <- function(){
  ggplot() + facet_wrap(~participant) +
    geom_ribbon(data = fit70$curves %>% merge(fitthre) %>% 
                  filter(x > thre90 & x< thre70),
                aes(x = x, ymin = .4, ymax = y),  fill = '#4daf4a', alpha = .3) +
    geom_point(data = fit70$averages, size = .8,
               aes(x =freq, y = prob, color = 'Tracking')) +
    geom_line(data = fit70$curves, size = sizeLine1,
              aes(x = x, y = y, color = 'Tracking')) + 
    geom_point(data =avpha, size = .8,
               aes(x = freq, y = m, ymin = inf, ymax = sup, 
                   color = 'Alignment')) +
    geom_line(data =avpha, size = sizeLine1, 
              aes(x = freq, y = m, ymin = inf, ymax = sup, 
                  color = 'Alignment')) +
    scale_color_manual(values = c('black', '#4daf4a'), 
                       guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
    scale_y_continuous(breaks = seq(0.5,1,.25)) +
    labs(x = 'Frequency (rps)', y = 'Probability of correct responses') +
    theme(legend.position = c(.85, .1),
          axis.title.x = element_text(hjust = 0.3),
          legend.title = element_blank())
}