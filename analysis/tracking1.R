library(quickpsy)
library(circular)
library(cowplot)
library(Hmisc)

### parameters #################################################################
oneColumnWidth <- 3.42
onehalfColumnWidth <- 4.5
twoColumnWidth <- 7
sizeLine1 <- .25
sizePoint1 <- 1
sizePoint2 <- 1.2
sizeText <- 2.5

theme_track <- theme_set(theme_bw(10))
theme_track <- theme_update(panel.border = element_blank(),
                            panel.grid = element_blank(),
                            strip.background = element_blank(),
                            axis.ticks= element_line(size = sizeLine1))

theme_track <- theme_update(axis.line.x = element_line(colour = 'black', 
                                                       size=sizeLine1, 
                                                       linetype='solid'),
                            axis.line.y = element_line(colour = 'black', 
                                                      size=sizeLine1, 
                                                      linetype='solid'),
                            axis.ticks= element_line(size = sizeLine1), 
                            strip.background = element_blank())

### functions ##################################################################
totalerrorToDegSemi <- function(x) (x + 90) %% 180 - 90
degSemiToDegFull <- function(x) 2 * x
degFullToDegSemi <- function(x) x / 2
degFullToRadFull <- function(x) circular(x/180*pi, units = 'radians') 
radFullToDegFull <- function(x) circular(x/pi*180, units = 'deg') 

################################################################################
### phase ######################################################################
################################################################################
datpha <- quickreadfiles(path = 'analysis/data', 
                           participant = c('pa','cc','da','al','bj','he','lk'), 
                           task = c('phase'),
                           session = as.character(1:4))
  

datpha$participant <- datpha$participant %>% 
  recode(pa='Participant 1',
         cc='Participant 2',
         da='Participant 3',
         al='Participant 4',
         bj='Participant 5',
         he='Participant 6',
         lk='Participant 7')


datpha <- datpha %>% 
  select(participant, freq, same, response) %>%
  mutate(response = ifelse(
    (same == 'True' & response == 'right') | 
      (same == 'False' & response == 'left'),
    1,0))

avpha <- datpha %>% 
  group_by(participant, freq) %>% 
  do({
    m <- smean.cl.boot(.$response)
    data.frame(m = m[[1]], inf = m[[2]], sup = m[[3]])
  })

ggplot(avpha, 
       aes(x = freq, y = m, ymin = inf, ymax = sup, color = participant))+
  geom_line()+geom_pointrange()

################################################################################
### tracking ###################################################################
################################################################################
dattrack <- quickreadfiles(path = 'analysis/data', 
                           participant = c('pa','cc','da','al','bj','he','lk'), 
                           task = c('tracking'),
                           session = as.character(1:4))

dattrack$participant <- dattrack$participant  %>% 
  recode(pa='Participant 1',
         cc='Participant 2',
         da='Participant 3',
         al='Participant 4',
         bj='Participant 5',
         he='Participant 6',
         lk='Participant 7')

dattrack <- dattrack %>% 
  select(participant, freq, same, response) %>%
  mutate(response = ifelse(
    (same == 'True' & response == 'right') | 
    (same == 'False' & response == 'left'),
    1,0))

psychom <- function(x, p) (1 - p[3]) - 
           (.5 - p[3]) * cum_normal_fun(x, c(p[1], p[2]))

pini <-  list(c(.5, 4), c(.05, 5), c(0,.5))
fit70 <- quickpsy(dattrack, freq, response, grouping = .(participant),
                  fun = psychom, parini = pini,
                  prob = .7, 
                  bootstrap = 'none')
fit90 <- quickpsy(dattrack, freq, response, grouping = .(participant),
                  fun = psychom, parini = pini,
                  prob = .9, 
                  bootstrap = 'none')
fit80 <- quickpsy(dattrack, freq, response, grouping = .(participant),
                  fun = psychom, parini = pini,
                  prob = .8, B = 1000)


fitthre70 <- fit70$thresholds %>% rename(thre70 = thre) %>% select(-prob)
fitthre90 <- fit90$thresholds %>% rename(thre90 = thre) %>% select(-prob)
fitthre <-fitthre70 %>% merge(fitthre90)

### figure
ptrack <- ggplot() + facet_wrap(~participant) +
  geom_ribbon(data = fit70$curves %>% merge(fitthre) %>% 
                filter(x > thre90 & x< thre70),
              aes(x = x, ymin = .4, ymax = y), alpha =.1) +
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
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  scale_y_continuous(breaks = seq(0.5,1,.25)) +
  labs(x = 'Frequency (rps)', y = 'Probability of correct responses') +
  theme(legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
ptrack


save_plot('analysis/figures/trackalig.pdf', ptrack,
          base_width = oneColumnWidth)

################################################################################
### sync #######################################################################
################################################################################
dat <- quickreadfiles(path = 'analysis/data', 
                      participant = c('pa','cc','da','al','bj','he','lk'), 
                      task = c('press'), 
                      session = as.character(1:4))

dat$participant <- dat$participant  %>% 
  recode(pa='Participant 1',
         cc='Participant 2',
         da='Participant 3',
         al='Participant 4',
         bj='Participant 5',
         he='Participant 6',
         lk='Participant 7')

dat <- dat %>% 
  select(participant, direction, angleLandmark, freq, response) %>%
  filter(response != 9999) # the participant didn't respond

dat <- dat %>% 
  mutate(totalerror = direction * (response - angleLandmark),
         degSemi =  totalerror %>% totalerrorToDegSemi(),
         degFull = degSemi %>% degSemiToDegFull(),
         radFull = degFull %>% degFullToRadFull())

### Null hypothesis testing: uniformity
uniformity <- dat %>% 
  group_by(participant, freq) %>% 
  summarise(rayp = rayleigh.test(radFull)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) 

datuniformity <- dat %>% merge(uniformity)

### means
circmeans <- datuniformity %>% 
  group_by(participant, freq, uniform) %>% 
  summarise(mrad = mean.circular(radFull), 
            mdegSemi = mrad %>% radFullToDegFull() %>% degFullToDegSemi()) %>% 
  filter(!uniform)

### figure
anglestoplot <- tibble(n = 1:4, 
                       x = rep(3.6,4), 
                       y = c(0,45,90,-45), 
                       angle = c('0','45',' 90\n-90','-45'),
                       participant = 'Participant 5')

zeroline <- tibble(participant = datuniformity$participant %>% unique(),
                   x = 0,
                   xend = c(3.8, 3.8, 3.8, 3.2, 3.2, 3.2, 3.2), 
                   y = 0,
                   yend = 0)

praw <- ggplot() + facet_wrap(~participant, ncol = 3) +
 geom_rect(data=fitthre, fill = 'black', alpha =.2,
            aes(xmin = thre70, xmax = thre90, ymin = -90, ymax = 90)) +
  geom_point(data= datuniformity, aes(x = freq, y = degSemi, color = uniform), 
             size = .35, alpha = 0.5, shape = 16) +
  geom_text(data = anglestoplot, aes(x = x, y = y, label= angle, group = n),
            size=sizeText) +
  geom_segment(data = zeroline, aes(y = y, yend = yend, x = x, xend = xend),
               size = sizeLine1)+
  scale_color_discrete(guide=guide_legend(title = NULL,
                                          override.aes = list(size = 1)), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_x_continuous(breaks = seq(0, 3, 1),labels = as.character(0:3)) +
  scale_y_continuous(breaks = seq(-45,90,45), limits = c(-90, 90),
                    labels =c('-45','0','45','90')) +
  xlab('Frequency (rps)') + 
  ylab('Error (deg)') +
  coord_polar(theta='y', start = pi/2, direction = -1)+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.line.y = element_line(size=sizeLine1), 
        panel.margin = unit(-.32,'lines'),
        legend.key = element_blank(),
        legend.position = c(.7,.16))
praw

save_plot('analysis/figures/raw.pdf', praw,
          base_width = oneColumnWidth)

### bootstrap 
datboot <- expand.grid(n = 1:1000) %>% 
  group_by(n) %>% 
  do({
    dat %>% 
      group_by(participant, freq) %>% 
      sample_frac(1, replace = T)
  }) 

### simulated data 
datsim <- data_frame(freqsim = unique(dat$freq)) %>% 
  merge(dat) %>%
  filter(freqsim >= freq) %>% 
  mutate(degFullSim = freqsim / freq * degFull,
         radFullSim = degFullSim %>% degFullToRadFull())

datsimboot <- expand.grid(n = 1:1000) %>% 
  group_by(n) %>% 
  do({
    datsim %>% 
      group_by(participant, freq, freqsim) %>% 
      sample_frac(1, replace = T)
  }) 


### dispersion rho 
dispersion <- dat %>% 
  group_by(participant, freq) %>%
  summarise(rho = rho.circular(radFull))

dispersionuniformity <- dispersion %>% merge(uniformity)


dispersionboot <- datboot %>% 
  group_by(participant, freq, n) %>% 
  summarise(rho = rho.circular(radFull))

dispersionbootci <- dispersionboot %>% 
  group_by(participant, freq) %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionbootciuniformity <- dispersionbootci %>% merge(uniformity)

dispersionsim <- datsim %>% 
  group_by(participant, freq, freqsim) %>%
  summarise(rho = rho.circular(radFullSim))

dispersionsimboot <- datsimboot %>% 
  filter(freq == .75) %>% 
  group_by(participant, freq, freqsim, n) %>%
  summarise(rho = rho.circular(radFullSim))

dispersionsimbootci <- dispersionsimboot %>% 
  group_by(participant, freq, freqsim) %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

### figure
prho <- ggplot() + facet_wrap(~participant, scales = 'free_x') +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_line(data = dispersionsimbootci, size = sizeLine1, 
            aes(x = freqsim, y = inf), color='grey') +
  geom_line(data = dispersionsimbootci, size = sizeLine1,
            aes(x = freqsim, y = sup), color='grey') +

  geom_line(data = dispersionuniformity,size = sizeLine1,
            aes(x = freq, y = rho, color = uniform))+
  geom_point(data = dispersionuniformity, size = .8,
            aes(x = freq, y = rho, color = uniform))+
   geom_ribbon(data = dispersionbootciuniformity,  alpha =.2,
             aes(x = freq, ymin = inf, ymax = sup,fill = uniform)) +
   scale_color_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
        legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
prho

save_plot('analysis/figures/rho.pdf', prho,
          base_width = oneColumnWidth)

### dispersion wrapped normal 
funWrap <- function(d){
  sta <- mle.wrappednormal(d$radFull)$sd
  stams <- sta / (4*pi*first(d$freq))*1000 # it is not 2*pi (it is half lap)
  data.frame(sta, stams)
}

dispersionwrap <- dat %>% 
  group_by(participant, freq) %>%
  do(funWrap(.))

dispersiowrapnuniformity <- dispersionwrap %>% merge(uniformity)


dispersionwrapboot <- datboot %>% 
  group_by(participant, freq, n) %>% 
  do(funWrap(.))

dispersionwrapbootci <- dispersionwrapboot %>% 
  group_by(participant, freq) %>% 
  summarise(inf = quantile(stams, .025), 
            sup = quantile(stams, .975))

dispersionwrapbootciuniformity <- dispersionwrapbootci %>% merge(uniformity)

dispersionwrapsim <- datsim %>% 
  group_by(participant, freq, freqsim) %>%
  do(funWrap(.))

dispersionwrapsimboot <- datsimboot %>% 
  filter(freq == .75) %>% 
  group_by(participant, freq, freqsim, n) %>%
  do(funWrap(.))

dispersionwrapsimbootci <- dispersionwrapsimboot %>% 
  group_by(participant, freq, freqsim) %>% 
  summarise(inf = quantile(stams, .025), 
            sup = quantile(stams, .975))



pwra <- ggplot() + facet_wrap(~participant, scales = 'free_x') +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 160)) +
  geom_line(data = dispersionwrapsimbootci, size = sizeLine1, 
            aes(x = freqsim, y = inf), color='grey') +
  geom_line(data = dispersionwrapsimbootci, size = sizeLine1,
            aes(x = freqsim, y = sup), color='grey') +
  
  geom_line(data = dispersiowrapnuniformity,size = sizeLine1,
            aes(x = freq, y = stams, color = uniform))+
  geom_point(data = dispersiowrapnuniformity, size = .8,
             aes(x = freq, y = stams, color = uniform))+
   geom_ribbon(data = dispersionwrapbootciuniformity,  alpha =.2,
               aes(x = freq, ymin = inf, ymax = sup,fill = uniform)) +
  scale_color_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Standard deviation (ms)') +
  #scale_y_continuous(breaks = seq(0,1,.5)) +
 scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
        legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
pwra

save_plot('analysis/figures/wrap.pdf', pwra,
          base_width = oneColumnWidth)

