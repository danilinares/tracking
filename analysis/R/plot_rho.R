plot_rho <- function() {
  ggplot() + facet_wrap(~participant, scales = 'free_x') +
    geom_rect(data = fitthre, fill = '#4daf4a', alpha =.3,
              aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
    geom_point(data = dispersionuniformity1, 
               aes(x = freq, y = rho, color = uniform_lab, fill = uniform_lab, 
                   shape = signif, size = signif)) +
    geom_point(data = dispersionuniformity2, 
                aes(x = freq, y = rho, color = uniform_lab, fill = uniform_lab, 
                    shape = signif, size = signif)) +
    geom_linerange(data = dispersionuniformity, size = sizeLine1,
                   aes(x = freq, ymin = inf, ymax = sup, color  = uniform_lab))+
    geom_line(data = dispersionsimuniformity, show.legend = FALSE,
              size = sizeLine1, 
              aes(x = freq, y = rho, color = uniform_lab)) +
    geom_line(data = dispersionsimtrackuniformity,
              show.legend = FALSE,
              size = 3 *sizeLine1, alpha = .5,
              aes(x = freq, y = rho, color = uniform_lab)) +
    scale_color_brewer(palette = 'Set1') + 
    scale_fill_brewer(palette = 'Set1') +
    scale_shape_manual(values = c(25, 24, 21), guide = FALSE) +
    scale_size_manual(values = c(.8, .8, .5), guide = FALSE) +
    labs(x = 'Frequency (rps)', y = 'Resultant vector length') +
    scale_y_continuous(breaks = seq(0,1,.5), limits = c(0,1)) +
    scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
    theme(legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.8,.13),  
          axis.title.x = element_text(hjust = 0.25))
}


