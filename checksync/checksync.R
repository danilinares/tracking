library(dplyr)
library(ggplot2)
dat <- read.table('dat.txt', header = T) %>% 
  mutate(dif=1000*(received-sent))

mean(dat$dif)
sta <- paste('SD = ', round(sd(dat$dif),2),' ms')

ggplot(dat, aes(x = dif)) + geom_histogram(binwidth = 1) +
  annotate('text', label = sta, x = 20, y = 15, size = 8) +
  xlab('Time difference (ms)')


