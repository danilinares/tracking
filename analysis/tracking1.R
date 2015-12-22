library(quickpsy)
library(circular)

### parameters
B <- 100 #bootstrap replications
alpha <- .95 # significance level

### functions
load('analysis/CircStatsInR.RData')
toCircleRad <- function(x) circular(2*x/180*pi, units = 'radians', modulo = '2pi')
toAxialDeg <- function(x) ((x/pi*180) / 2 + 90) %% 180 - 90

################################################################################
### tracking ###################################################################
################################################################################

### making  data ready 
dattrack <- quickreadfiles(path = 'analysis/data', 
                           participant = c('dl'), 
                           task = c('tracking'))


dattrack <- dattrack %>% 
  select(participant,freq, same, response) %>%
  mutate(response = ifelse(
    (same == 'True' & response == 'right') | 
    (same == 'False' & response == 'left'),
    1,0))

### analyse  data
psychom <- function(x, p) (1 - p[3]) - (.5 - p[3]) * cum_normal_fun(x, c(p[1],p[2]))
fit <- quickpsy(dattrack, freq, response, grouping = .(participant),
                fun = psychom, parini = c(2,1, .1), prob = .85, B = B)
plot(fit)

################################################################################
### sync ###################################################################
################################################################################

### Making  data ready 
dat <- read.table('analysis/data/dlsynch.txt', header = T, 
                  stringsAsFactors=FALSE) 
dat <- dat %>% 
  select(direction, angleLandmark, freq, response) %>%
  filter(response != 'None') %>% mutate(response = as.numeric(response))

dat <- dat %>% 
  mutate(totalerror = direction*(response - angleLandmark),
         error = (totalerror + 90) %% 180 - 90,
         errorc = toCircleRad(error),
         errort =  error / (freq *360) * 1000)

### Null hypothesis testing: isotropy
uniformity <- dat %>% 
  group_by(freq) %>% 
  summarise(rayp = rayleigh.test(errorc)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE))

### Null hypothesis testing: reflective symmetry
symmetry <- dat %>% 
  group_by(freq) %>% 
  summarise(symp = RSTestBoot(errorc,B)) %>%
  mutate(sym = ifelse(symp > 0.05, TRUE, FALSE))

### Plug-in estimator of location: mean direction
locationFun <- function(d, alpha, B) {
   pointEst <- ConfIntBoot(d$errorc, 1, alpha * 100, B) # check that is uniform
   mc <- pointEst[[1]][1] ; mminc <- pointEst[[1]][2]; mmaxc <- pointEst[[1]][3]
   m <- toAxialDeg(mc); mmin = toAxialDeg(mminc); mmax = toAxialDeg(mmaxc)
  data.frame(m, mmin, mmax)
}
location <- dat %>% merge(uniformity) %>% filter(uniform == F) %>%
  group_by(freq) %>%
  do(locationFun(., alpha, B)) 

pLocation <- ggplot() + facet_wrap(~participant) +
  geom_rect(data=fit$thresholdsci, fill = 'blue', alpha =.3,
            aes(xmin=threinf,xmax=thresup,ymin=-Inf,ymax=Inf)) +
  geom_vline(data=fit$thresholds, aes(xintercept= thre), lty = 2) +
  geom_point(data = dat,aes(freq, error), size = 6, alpha = .15, shape = 15) + 
  geom_point(data = location, size = 4, aes(x = freq, y = m, color = 'mean')) +
  geom_linerange(data = location, aes(x = freq, ymin = mmin, ymax = mmax, color = 'mean')) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5)) +
  scale_y_continuous(breaks = seq(-90,90,45)) +
  xlab('Frequency (rps)') + 
  ylab('Mean direction (deg)') +
  theme_bw()
pLocation

locationT <- location %>%
  mutate(mt = m / (unique(freq) *360) * 1000,
         mmint = mmin / (unique(freq) *360) * 1000,
         mmaxt = mmax/ (unique(freq) *360) * 1000)
         

pLocationT <- ggplot() + facet_wrap(~participant) +
  geom_rect(data=fit$thresholdsci, fill = 'blue', alpha =.3,
            aes(xmin=threinf,xmax=thresup,ymin=-Inf,ymax=Inf)) +
  geom_vline(data=fit$thresholds, aes(xintercept= thre), lty = 2) +
  geom_point(data = dat,aes(freq, errort), size = 6, alpha = .15, shape = 15)+
  geom_point(data = locationT, size = 4, aes(x = freq, y = mt, color = 'mean'))+
  geom_linerange(data = locationT, aes(x = freq, ymin = mmint, ymax = mmaxt, 
                                       color = 'mean')) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5))+
  xlab('Frequency (rps)') + 
  ylab('Mean time (ms)') +
  theme_bw()
pLocationT

### Plug-in estimator of dispersion: mean resultant length
dispersionFun <- function(d, alpha, B) {
  pointEst <- ConfIntBoot(d$errorc, 1, alpha * 100, B) # check that is uniform
  rho <- pointEst[[2]][1]; rhomin <- pointEst[[2]][2]; rhomax <- pointEst[[2]][3]
  data.frame(rho, rhomin, rhomax)
}
dispersion <- dat %>% merge(uniformity) %>% #filter(uniform == F) %>%
  group_by(freq) %>%
  do(dispersionFun(., alpha, B)) 
  
pDispersion <- ggplot() + facet_wrap(~participant) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_rect(data=fit$thresholdsci, fill = 'blue', alpha =.3,
            aes(xmin=threinf,xmax=thresup,ymin=-Inf,ymax=Inf)) +
  geom_vline(data=fit$thresholds, aes(xintercept= thre), lty = 2) +
  geom_point(data = dispersion, size = 4, aes(x = freq, y = rho, color = 'rho'))+
  geom_linerange(data = dispersion, aes(x = freq, ymin = rhomin, ymax = rhomax, 
                                       color = 'rho')) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5)) +
#  scale_y_continuous(limits = c(0,1)) +
  xlab('Frequency (rps)') + 
  ylab('Mean resultant length') +
  theme_bw()
pDispersion


### Wrapped normal 
wrapFun <- function(d) {
  sdc <- mle.wrappednormal(d$errorc)$sd
  sd <- toAxialDeg(sdc)
  sdMs <- sd/ (unique(d$freq) *360) * 1000
  data.frame(sd, sdMs)
}
wrap <- dat %>% merge(uniformity) %>% filter(uniform == F) %>%
  group_by(freq) %>%
  do(wrapFun(.)) 

pwrap <- ggplot() + facet_wrap(~participant) +
  geom_rect(data=fit$thresholdsci, fill = 'blue', alpha =.3,
            aes(xmin=threinf,xmax=thresup,ymin=-Inf,ymax=Inf)) +
  geom_vline(data=fit$thresholds, aes(xintercept= thre), lty = 2) +
  geom_point(data = wrap,aes(freq, sd)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5)) +
  scale_y_continuous(limits = c(0,90)) +
  xlab('Frequency (deg)') + 
  ylab('SD (ms)') 
pwrap

pwrapT <- ggplot() + facet_wrap(~participant) +
  geom_rect(data=fit$thresholdsci, fill = 'blue', alpha =.3,
            aes(xmin=threinf,xmax=thresup,ymin=-Inf,ymax=Inf)) +
  geom_vline(data=fit$thresholds, aes(xintercept= thre), lty = 2) +
  geom_point(data = wrap,aes(freq, sdMs)) +
  scale_x_continuous(limits = c(0,3.5), breaks = seq(0,3.5,.5))+
  scale_y_continuous(limits = c(0,100)) +
  xlab('Frequency (rps)') + 
  ylab('SD (ms)') 
pwrapT
