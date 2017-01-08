library(dplyr)
library(circular)

### functions
funWrapSim <- function(d){
  sta <- mle.wrappednormal(d$responseangle)$sd
  stamswrap <- sta / (4*pi*first(d$freq))*1000 
  data.frame(stamswrap)
}


### simulating data
sim <- expand.grid(freq = seq(.5,5,.5), 
                   stams = seq(10,110, 20),
                   n = 1:48) %>% 
  group_by(freq, stams, n) %>% 
  mutate(responsems = rnorm(1, 0,stams), 
         responseangle = responsems / 1000 * freq * 4 * pi)

### calculating the sta dev of the wrapped normal from the sim data
simwrap <- sim %>% 
  group_by(freq, stams) %>%
  do(funWrapSim(.))

### ploting the original sta dev against the one obtained from the wrapped
ggplot(simwrap, aes(x = stams, y = stamswrap, color=factor(freq))) +
  geom_abline(lty = 2) +
         geom_line()

### calculating uniformity and rho
simsum <- sim %>% 
  group_by(freq, stams) %>% 
  summarise(rho = rho.circular(responseangle))

simuniformity <- sim %>% 
  group_by(freq, stams) %>% 
  summarise(rayp = rayleigh.test(responseangle)$p) %>% 
  mutate(uniform = ifelse(rayp > 0.05, TRUE, FALSE)) 

simsumuniformity <- simsum %>% merge(simuniformity)

### plotting uniformity and rho
ggplot(simsumuniformity, 
       aes(x = freq, y= rho, color= uniform )) +
  facet_wrap(~stams, scales='free_x') +
  geom_line()
