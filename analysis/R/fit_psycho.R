fit_psycho <- function(d, prob) {
  quickpsy(d, freq, response, grouping = .(participant),
           fun = psychom, parini = list(c(.5, 4), c(.05, 5), c(0,.5)),
           prob = prob, 
           bootstrap = 'none')
}