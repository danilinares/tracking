library(quickpsy)
library(circular)

cci <- function(x) circular(x,'angles','degrees', modulo = '2pi')

dat <- read.table('analysis/data/dl.txt', header = T) 

dat <- dat %>% 
  select(direction, angleLandmark, freq, response) %>%
  mutate(response1 = response %% 360,
         response2 = circular(response,'angles','degrees', modulo = 'pi'),
        angleLandmark = circular(angleLandmark,'angles','degrees', modulo = 'pi'))

