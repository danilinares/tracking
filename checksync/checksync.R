library(dplyr)
library(ggplot2)
dat <- read.table('dat.txt', header = T) %>% 
  mutate(dif=1000*(received-sent))

mean(dat$dif)
sd(dat$dif)

ggplot(dat, aes(x = dif- mean(dif))) + geom_histogram(binwidth = 1)


