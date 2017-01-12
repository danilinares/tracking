library(quickpsy)
library(tidyverse)
library(circular)
library(cowplot)
library(PropCIs)
library(R.utils)
sourceDirectory('R')
source('analysis/graphical_parameters.R')

################################################################################
################################################################################
### phase ######################################################################
################################################################################
################################################################################
datpha <- quickreadfiles(path = 'analysis/data', 
                         participant = c('pa','cc','da','al',
                                         'bj','he','lk', 'ad'), 
                         task = 'phase', session = as.character(1:4))
  
datpha$participant <- datpha$participant %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7',
         ad = 'Participant 8')

datpha <- datpha %>% select(participant, freq, same, response) %>%
  mutate(response = ifelse( (same == 'True' & response == 'right') | 
                            (same == 'False' & response == 'left'), 1, 0))

avpha <- datpha %>% 
  group_by(participant, freq) %>% 
  summarise(n = n(), k = sum(response)) %>% 
  group_by(participant, freq) %>% 
  do({
    m <- .$k / .$n
    ci <- exactci(.$k, .$n, .05)$conf.int
    data.frame(m = m, inf = ci[1], sup = ci[2])
  })

################################################################################
################################################################################
### tracking ###################################################################
################################################################################
################################################################################
dattrack <- quickreadfiles(path = 'analysis/data', 
                           participant = c('pa','cc','da','al',
                                           'bj','he','lk', 'ad'),
                           task = c('tracking'), session = as.character(1:4))

dattrack$participant <- dattrack$participant  %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7', 
         ad = 'Participant 8')

dattrack <- dattrack %>% select(participant, freq, same, response) %>%
  mutate(response = ifelse(
    (same == 'True' & response == 'right') | 
    (same == 'False' & response == 'left'),
    1,0))

fit70 <- fit_psycho(dattrack, .7)
fit80 <- fit_psycho(dattrack, .8)
fit90 <- fit_psycho(dattrack, .9)

thres <- fit70$thresholds %>% 
  bind_rows(fit80$thresholds) %>% 
  bind_rows(fit90$thresholds) %>% 
  spread(key = prob, value = thre, sep = '_')

################################################################################
################################################################################
### sync #######################################################################
################################################################################
################################################################################
dat <- quickreadfiles(path = 'analysis/data', 
                      participant = c('pa','cc','da','al','bj','he','lk','ad'),
                      task = c('press'), 
                      session = as.character(1:4)) 

dat$participant <- dat$participant  %>% 
  recode(pa = 'Participant 1',
         cc = 'Participant 2',
         da = 'Participant 3',
         al = 'Participant 4',
         bj = 'Participant 5',
         he = 'Participant 6',
         lk = 'Participant 7', 
         ad = 'Participant 8')

################################################################################
### data  ######################################################################
################################################################################
dat <- dat %>% 
  select(participant, direction, angleLandmark, freq, response) %>%
  filter(response != 9999) %>% # the participant didn't respond
  mutate(totalerror = direction * (response - angleLandmark), 
         degSemi =  totalerror %>% totalerrorToDegSemi(), 
         degFull = degSemi %>% degSemiToDegFull(), 
         radFull = degFull %>% degFullToRadFull()) %>% 
  group_by(participant, freq) 

minfreq <- dat %>% group_by(participant) %>% summarise(freq = min(freq))

allfreqs <- dat %>% group_by(participant) %>% 
  do({tibble(freqsim = unique(.$freq))})

uniformity <- dat %>% 
  summarise(rayp = rayleigh.test(radFull)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE , FALSE),
         uniform_lab = if_else(uniform, 'Uniform', 'Non-uniform'))

datuniformity <- dat %>% left_join(uniformity) 

dispersion <- dat %>% summarise(rho = rho.circular(radFull)) 

datboot <- tibble(n = 1:1000) %>% group_by(n) %>% 
  do({
    dat %>% 
      group_by(participant, freq) %>% 
      sample_frac(1, replace = T)
  }) %>% 
  group_by(participant, freq, n)

dispersionboot <- datboot %>% 
  summarise(rho = rho.circular(radFull %>% circular())) 

dispersionbootci <- dispersionboot %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionuniformity <- dispersion %>% 
  left_join(dispersionbootci) %>% 
  left_join(uniformity) 

################################################################################
### constant variability simulation ############################################
################################################################################
datsim <-  dat %>% ungroup() %>% distinct(freq) %>% rename(freqsim = freq) %>% 
  group_by(freqsim) %>% do({dat}) %>% 
  filter(freqsim >= freq) %>% 
  mutate(degFullSim = freqsim / freq * degFull,
         radFullSim = degFullSim %>% degFullToRadFull()) %>% 
  semi_join(allfreqs) %>% 
  group_by(participant, freq, freqsim)

uniformitysim <-  datsim %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE),
         uniform_lab = if_else(uniform, 'Uniform', 'Non-uniform'))

dispersionsim <- datsim %>% summarise(rho = rho.circular(radFullSim))

datsimboot <- tibble(n = 1:1000)  %>% 
  group_by(n) %>% 
  do({
    datsim %>% 
      group_by(participant, freq, freqsim) %>% 
      sample_frac(1, replace = T)
  }) %>% 
  group_by(participant, freq, freqsim, n) 

dispersionsimboot <- datsimboot %>%
  summarise(rho = rho.circular(radFullSim %>% circular()))

dispersionsimbootci <- dispersionsimboot %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionsimuniformity <- dispersionsim %>% 
  left_join(dispersionsimbootci) %>% 
  left_join(uniformitysim) %>% 
  semi_join(minfreq) %>% semi_join(allfreqs) %>% 
  ungroup() %>% select(-freq) %>% rename(freq = freqsim)


### comparison rho and rho sim all points 
dispersionsimbootlowfreq <- dispersionsimboot %>% 
  semi_join(minfreq) %>% semi_join(allfreqs) %>% ungroup() %>% 
  select(-freq) %>% rename(freq = freqsim) 

rhosimrho <- (dispersionboot %>% mutate(simu = 1)) %>% 
  bind_rows(dispersionsimbootlowfreq %>% mutate(simu = 2))

difrhosimrho <- rhosimrho %>% group_by(participant, freq, n) %>% 
  summarise(dif = diff(rho)) %>% group_by(participant, freq) %>% 
  do({
    dif <- .$dif %>% mean()
    ci <- quantile(.$dif, c(.025,.975))
    tibble(dif, difinf = ci[[1]], difsup = ci[[2]])
  }) %>% 
  mutate(signif = if_else(difinf * difsup > 0, 'Dif from var', 'Non-significant')) %>% 
  select(participant, freq, signif)

dispersionuniformity1 <- dispersionuniformity %>% left_join(difrhosimrho)

################################################################################
### constant variability + tracking simulation #################################
################################################################################
trackingprob <- fit80$averages %>% select(participant, freq, prob) 
trackingprobminfreq <- trackingprob %>% semi_join(minfreq) %>% ungroup() %>% 
  rename(probfreqmin = prob) %>% select(-freq)

trackingprob <- trackingprob %>% left_join(trackingprobminfreq) %>% 
  mutate(probnotrack = 2*(1 - prob)) %>% # another possibility is to replace 
  # 1 by probfreqmin
  mutate(probnotrack = ifelse(probnotrack > 1, 1, probnotrack),
         probnotrack = ifelse(probnotrack < 0, 0, probnotrack)) %>% 
  rename(freqsim=freq)

datsimtrack <- datsim %>% 
  select(participant, freqsim, freq, radFullSim) %>% 
  left_join(trackingprob) %>% 
  do({
    n <-length(.$radFullSim)
    probnotrack <- unique(.$probnotrack)
    nnotrack <- round(n * probnotrack)
    track <- sample_n(.,n - nnotrack) 
    notrack <- sample_n(.,nnotrack) 
    notrack$radFullSim <- runif(nnotrack,0, 2*pi)
    rbind(track, notrack)
  })

uniformitysimtrack <-  datsimtrack %>% 
  summarise(rayp = rayleigh.test(radFullSim)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE),
         uniform_lab = if_else(uniform, 'Uniform', 'Non-uniform')) 

dispersionsimtrack <- datsimtrack %>% summarise(rho = rho.circular(radFullSim))

datsimboottrack <- tibble(n = 1:1000)  %>% 
  group_by(n) %>% 
  do({
    datsimtrack %>% 
      group_by(participant, freq, freqsim) %>% 
      sample_frac(1, replace = T)
  }) %>% 
  group_by(participant, freq, freqsim, n) 

dispersionsimboottrack <- datsimboottrack %>%
  summarise(rho = rho.circular(radFullSim %>% circular()))

dispersionsimboottrackci <- dispersionsimboottrack %>% 
  summarise(inf = quantile(rho, .025), 
            sup = quantile(rho, .975))

dispersionsimtrackuniformity <- dispersionsimtrack %>% 
  left_join(dispersionsimboottrackci) %>% 
  left_join(uniformitysimtrack) %>% 
  mutate(simu = 'track') %>% 
  semi_join(minfreq) %>% semi_join(allfreqs) %>% 
  ungroup() %>% select(-freq) %>% rename(freq = freqsim)

### comparison rho and rho sim track 
dispersionsimboottracklowfreq <- dispersionsimboottrack %>% 
  semi_join(minfreq) %>% semi_join(allfreqs) %>% ungroup() %>% 
  select(-freq) %>% rename(freq = freqsim) 

rhosimrhotrack <- (dispersionboot %>% mutate(simu = 1)) %>% 
  bind_rows(dispersionsimboottracklowfreq %>% mutate(simu = 2))

difrhosimrhotrack <- rhosimrhotrack %>% group_by(participant, freq, n) %>% 
  summarise(dif = diff(rho)) %>% group_by(participant, freq) %>% 
  do({
    dif <- .$dif %>% mean()
    ci <- quantile(.$dif, c(.025,.975))
    tibble(dif, difinf = ci[[1]], difsup = ci[[2]])
  }) %>% 
  mutate(signif = if_else(difinf * difsup > 0, 'Dif from track', 'Non-significant')) %>% 
  select(participant, freq, signif)

dispersionuniformity2 <- dispersionuniformity %>% left_join(difrhosimrhotrack)

################################################################################
### temporal variability #######################################################
################################################################################
dispersionwrap <- dat %>% 
  do({
    sta <- mle.wrappednormal(.$radFull)$sd
    stams <- sta / (4*pi*first(.$freq))*1000 # it is not 2*pi (it is half lap)
    tibble(sta, stams)
  })

dispersionwrapmin <- dispersionwrap %>% semi_join(minfreq)
dispersionwrapmin$stams %>% mean()

################################################################################
### figures ####################################################################
################################################################################
praw <- plot_raw()
save_plot('analysis/figures/fig2.pdf', praw, 
          base_width = oneColumnWidth, base_height = .8 * oneColumnWidth)

ppsycho <- plot_psycho()
save_plot('analysis/figures/fig3.pdf', ppsycho,
          base_width = oneColumnWidth, base_height = .8 * oneColumnWidth)

prho <- plot_rho()
save_plot('analysis/figures/fig4.pdf', prho, base_width = oneColumnWidth)




