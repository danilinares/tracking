plot_raw <- function() {
ggplot(circmeans  %>% filter(!uniform)) +
  facet_wrap(~participant) +
  geom_rect(data=thres, fill = '#4daf4a', alpha = .3,
            aes(xmin = prob_0.7, xmax = prob_0.9, ymin = -110, ymax = 110)) +
  geom_point(data = datuniformity,  alpha = .3, size = .4,
             aes(x = freq, y = degSemi, color = uniform_lab)) +
  # geom_line(aes(x = freq, y = mdegSemi, color = uniform_lab), 
  #           size = sizeLine1, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(-90,90,90), limits = c(-110, 110)) +
  scale_color_brewer(palette = 'Set1') +
  xlab('Frequency (rps)') + 
  ylab('Angular error (deg)') +
  guides(colour = guide_legend(override.aes = list(size=1))) +
  theme(legend.position = c(.8, .1),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.25)) 
}