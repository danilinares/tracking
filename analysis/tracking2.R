library(quickpsy)
library(circular)
library(cowplot)
library(Hmisc)

### parameters plotting ########################################################
oneColumnWidth <- 3.42
onehalfColumnWidth <- 4.5
twoColumnWidth <- 7
sizeLine1 <- .25
sizeText <- 2.5

theme_track <- theme_set(theme_classic(10))
theme_track <- theme_update(strip.background = element_blank(),
                            axis.ticks= element_line(size = sizeLine1),
                            axis.line = element_line(size=sizeLine1))
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
                         task = 'phase',
                         session = as.character(1:4))
  
datpha$participant <- datpha$participant %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7')

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
################################################################################
### tracking ###################################################################
################################################################################
dattrack <- quickreadfiles(path = 'analysis/data', 
                           participant = c('pa','cc','da','al',
                                           'bj','he','lk', 'ad'),
                           task = c('tracking'),
                           session = as.character(1:4))

dattrack$participant <- dattrack$participant  %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7', 
         ad = 'Participant 8')

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
fitthre80 <- fit70$thresholds %>% rename(thre80 = thre) %>% select(-prob)
fitthre90 <- fit90$thresholds %>% rename(thre90 = thre) %>% select(-prob)
fitthre <- fitthre70 %>% merge(fitthre90)

### binomial tests 
trackbinom <- fit70$averages %>% 
  group_by(participant, freq)  %>% 
  mutate(pvalue = binom.test(response, n)$p.value) %>% 
  mutate(chance = ifelse(pvalue > 0.05, TRUE, FALSE))

summarise(rayp = rayleigh.test(radFull)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) 

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
  geom_text(data = trackbinom %>% mutate(ast = if_else(chance, '','*')),
            aes(x = freq, label =ast), y = .5) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  scale_y_continuous(breaks = seq(0.5,1,.25)) +
  labs(x = 'Frequency (rps)', y = 'Probability of correct responses') +
  theme(legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.8,.16),
        axis.title.x = element_text(hjust = 0.01))
ptrack

save_plot('analysis/figures/trackalig.pdf', ptrack,
          base_width = twoColumnWidth)

################################################################################
### sync #######################################################################
################################################################################
dat <- quickreadfiles(path = 'analysis/data', 
                      participant = c('pa','cc','da','al','bj','he','lk','ad'),
                      task = c('press'), 
                      session = as.character(1:8)) 

dat$participant <- dat$participant  %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7', 
         ad = 'Participant 8')

dat <- dat %>% 
  select(participant, direction, angleLandmark, freq, response) %>%
  filter(response != 9999) %>% # the participant didn't respond
  mutate(totalerror = direction * (response - angleLandmark),
         degSemi =  totalerror %>% totalerrorToDegSemi(),
         degFull = degSemi %>% degSemiToDegFull(),
         radFull = degFull %>% degFullToRadFull()) %>% 
  group_by(participant, freq) 

### Null hypothesis testing: uniformity
uniformity <- dat %>% 
  summarise(rayp = rayleigh.test(radFull)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, '>.05', 
                          ifelse(rayp > .01, '<.05',
                                 ifelse(rayp > .001, '<.01','<.001'))))
         
                                      
         
         
                                  
                    
         

datuniformity <- dat %>% left_join(uniformity)

###  means
circmeans <- datuniformity %>% 
  group_by(participant, freq, uniform) %>% 
  summarise(mrad = mean.circular(radFull), 
            mdegSemi = mrad %>% radFullToDegFull() %>% degFullToDegSemi()) %>% 
  filter(!uniform)

pmean <- ggplot(circmeans) +
  facet_wrap(~participant) +
  geom_point(data = datuniformity, alpha = .4,
             aes(x = freq, y = degSemi, color = uniform)) +
  geom_line(aes(x = freq, y = mdegSemi))
pmean

save_plot('analysis/figures/mean.pdf', pmean, base_width = twoColumnWidth)

### circular representation
anglestoplot <- tibble(n = 1:4, 
                       x = rep(3.6, 4), 
                       y = c(0, 45, 90, -45), 
                       angle = c('0', '45', ' 90\n-90', '-45'),
                       participant = 'Participant 5')

zeroline <- tibble(participant = datuniformity$participant %>% unique(),
                   x = 0,
                   xend = c(3.8, 3.8, 3.8, 3.2, 3.2, 3.2, 3.2, 3.2), 
                   y = 0,
                   yend = 0)

### figure raw
praw <- ggplot() + facet_wrap(~participant, ncol = 3) +
   geom_rect(data=fitthre, fill = 'black', alpha =.2,
             aes(xmin = thre70, xmax = thre90, ymin = -90, ymax = 90)) +
  geom_point(data= datuniformity, aes(x = freq, y = degSemi, color = uniform), 
             size = .35, alpha = 0.5, shape = 16) +
             #size=1) +
  geom_text(data = anglestoplot, aes(x = x, y = y, label= angle, group = n),
            size=sizeText) +
  geom_segment(data = zeroline, aes(y = y, yend = yend, x = x, xend = xend),
               size = sizeLine1)+
  scale_color_manual(name = 'p',
                     values=c('#08519c','#3182bd','#6baed6', '#b30000')) +
  scale_x_continuous(breaks = seq(0, 3, 1),labels = as.character(0:3)) +
  scale_y_continuous(breaks = seq(-45,90,45), limits = c(-90, 90),
                     labels =c('-45','0','45','90')) +
  xlab('Frequency (rps)') + 
  ylab('Error (deg)') +
  coord_polar(theta='y', start = pi/2, direction = -1)+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(size=sizeLine1), 
        panel.spacing = unit(-.32,'lines'),
        legend.key = element_blank(),
        legend.key.height =  unit(.7,'lines'),
        legend.position = c(.85,.16)) +
  guides(colour = guide_legend(override.aes = list(size=2)))
praw

save_plot('analysis/figures/raw.pdf', praw, base_width = oneColumnWidth)

### correlations
nonuniformspeed <- uniformity %>% 
  filter(!uniform) %>% 
  summarise(freq = max(freq)) %>% 
  rename(frequni=freq)

nontrackspeed <- trackbinom %>% 
  group_by(participant) %>% 
  filter(!chance) %>% 
  summarise(freq = max(freq)) %>% 
  rename(freqtrack=freq)

freqs <- nonuniformspeed %>% 
  left_join(nontrackspeed) %>% 
  left_join(fitthre80)


ggplot(freqs, aes(frequni, thre90)) + 
  geom_point() +
  geom_abline() +
  coord_equal(xlim = c(0, 2.5), ylim = c(0, 2.5)) 
  
cor.test(freqs$frequni, freqs$thre90)


### t-test

t.test(freqs$frequni,freqs$thre90, paired = TRUE)


### bootstrap 
datboot <- expand.grid(n = 1:1000) %>% 
  group_by(n) %>% 
  do({
    dat %>% 
      group_by(participant, freq) %>% 
      sample_frac(1, replace = T)
  }) %>% 
  group_by(participant, freq, n)

### simulated data 
datsim <- data_frame(freqsim = unique(dat$freq)) %>% 
  group_by(freqsim) %>%
  do({dat}) %>% 
  filter(freqsim >= freq) %>% 
  mutate(degFullSim = freqsim / freq * degFull,
         radFullSim = degFullSim %>% degFullToRadFull()) %>% 
  group_by(participant, freq, freqsim)

### bootstrap simulated data 
datsimboot <- expand.grid(n = 1:1000) %>% 
  group_by(n) %>% 
  do({
    datsim %>% 
      group_by(participant, freq, freqsim) %>% 
      sample_frac(1, replace = T)
  })%>% 
  group_by(participant, freq, freqsim, n) 

### uniformity sim
uniformitysim <-  datsim %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) 

###  rho 
dispersion <- dat %>% summarise(rho = rho.circular(radFull))

dispersionboot <- datboot %>% 
  summarise(rho = rho.circular(radFull %>% circular()))

dispersionbootci <- dispersionboot %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionuniformity <- dispersion %>% left_join(dispersionbootci) %>% 
  left_join(uniformity)

###  rho sim
dispersionsim <- datsim %>% summarise(rho = rho.circular(radFullSim))

dispersionsimboot <- datsimboot %>%
  summarise(rho = rho.circular(radFullSim %>% circular()))

dispersionsimbootci <- dispersionsimboot %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionsimuniformity <- dispersionsim %>% 
  left_join(dispersionsimbootci) %>% 
  left_join(uniformitysim)

dispersionsimuniformityplot <- dispersionsimuniformity %>% 
  mutate(freq2 = freq, freqsim2 = freqsim)

### figure dispersion
prho <- ggplot() + facet_grid(participant~freq2, scales = 'free') +
  geom_line(data = dispersionsimuniformityplot, size = sizeLine1, 
            aes(x = freqsim2, y = rho, color = uniform)) +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_point(data = dispersionuniformity, size = .8,
             aes(x = freq, y = rho, color = uniform))+
  geom_ribbon(data = dispersionuniformity,  alpha =.2,
               aes(x = freq, ymin = inf, ymax = sup,fill = uniform)) +
  geom_text(data = trackbinom %>% mutate(ast = if_else(chance, '','*')),
            aes(x = freq, label =ast), y = 0) +
  scale_color_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
      #  legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
prho


### figure dispersion
prho <- ggplot() + facet_grid(participant~freq2, scales = 'free') +
  geom_line(data = dispersionsimuniformityplot, size = sizeLine1, 
            aes(x = freqsim2, y = rho, color = uniform)) +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_point(data = dispersionuniformity, size = .8,
             aes(x = freq, y = rho, color = uniform))+
  geom_ribbon(data = dispersionuniformity,  alpha =.2,
               aes(x = freq, ymin = inf, ymax = sup,fill = uniform)) +
  geom_text(data = trackbinom %>% mutate(ast = if_else(chance, '','*')),
            aes(x = freq, label =ast), y = 0) +
  scale_color_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
      #  legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
prho

save_plot('analysis/figures/rho.pdf', prho,
          base_width = 2*twoColumnWidth, 
          base_height = twoColumnWidth)

### two lower speeds 
oneminfreqsim <- dispersion %>% group_by(participant) %>% do(head(.,1))
twominfreqsim <- dispersion %>% group_by(participant) %>% do(head(.,2))

datsim2 <- datsim %>% semi_join(twominfreqsim) %>% 
  group_by(participant, freqsim)

uniformitysim2 <-  datsim2 %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) 

      
dispersionsim2 <- datsim2 %>% 
  summarise(rho = rho.circular(radFullSim)) %>% 
  anti_join(oneminfreqsim) %>% 
  left_join(uniformitysim2)

  
prho2 <- ggplot() + facet_wrap(~participant, scales = 'free') +
   geom_line(data = dispersionsim2, size = sizeLine1, 
             aes(x = freqsim, y = rho, color = uniform)) +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_point(data = dispersionuniformity, size = .8,
             aes(x = freq, y = rho, color = uniform))+
  geom_ribbon(data = dispersionuniformity,  alpha =.2,
              aes(x = freq, ymin = inf, ymax = sup,fill = uniform)) +
  scale_color_discrete(guide=guide_legend(title = NULL), 
                       breaks = c(T, F), 
                       labels = c('Uniform', 'Non-uniform')) +
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
        #  legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
prho2

### uniformity analysis
dispersionbootciunifotest <-  datsim %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) %>% 
  rename(freq2 = freq, freqsim2 = freqsim)

prhouni <- ggplot() + facet_wrap(~freq) +
  #   geom_rect(data = fitthre, fill = 'black', alpha =.1,
  #             aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_line(data = dispersionbootciunifotest %>% filter(freq < 2.5), 
            aes(x = freqsim, y = m)) +
  geom_line(data = dispersionuniformityplot,size = sizeLine1,
            aes(x = freqsim, y = rho, color = uniform))+
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5)) +
  theme(legend.key = element_blank(),
        #  legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))+
  geom_vline(xintercept = 2)+geom_hline(yintercept = .05)
prhouni 


dispersionsimbootciunifotest <-  datsimboot %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) %>% 
  group_by(participant, freq, freqsim) %>%
  summarise(m = mean(uniform))

dispersionuniformityplot <- dispersionuniformity %>%
  rename(freqsim=freq)

prhounisim <- ggplot() + facet_wrap(~freq) +
#   geom_rect(data = fitthre, fill = 'black', alpha =.1,
#             aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_line(data = dispersionsimbootciunifotest %>% filter(freq < 2.5), 
            aes(x = freqsim, y = m)) +
   geom_line(data = dispersionuniformityplot,size = sizeLine1,
             aes(x = freqsim, y = rho, color = uniform))+
  scale_fill_discrete(guide=guide_legend(title = NULL), 
                      breaks = c(T, F), 
                      labels = c('Uniform', 'Non-uniform')) +
  labs(x = 'Frequency (rps)', y = 'Rho') +
  scale_y_continuous(breaks = seq(0,1,.5)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5)) +
  theme(legend.key = element_blank(),
      #  legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))+
  geom_vline(xintercept = 2)+geom_hline(yintercept = .05)
prhounisim 

save_plot('analysis/figures/rhounif.pdf', prhouni,
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
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,1)) +
  theme(legend.key = element_blank(),
        legend.position = c(.7,.16),
        axis.title.x = element_text(hjust = 0.01))
pwra

save_plot('analysis/figures/wrap.pdf', pwra,
          base_width = oneColumnWidth)


dif <- dispersionsim %>% filter(freq == .75) %>% 
  ungroup() %>% 
  select(-freq) %>% 
  rename(freq= freqsim, rhosim = rho) %>% 
  merge(dispersion) %>% 
  group_by(participant, freq) %>% 
  summarise(dif = rhosim - rho)


ggplot()+
  facet_wrap(~participant) +
  geom_rect(data = fitthre, fill = 'black', alpha =.1,
            aes(xmin = thre70, xmax = thre90, ymin = 0, ymax = 1)) +
  geom_line(data = dif,
            aes(x = freq, y = dif)) 
  
  
  geom_line(data = dispersionsim %>% filter(freq == .75),
            aes(x =freqsim, rho)) +
  geom_line(data = dispersion,
            aes(x =freq, rho))





