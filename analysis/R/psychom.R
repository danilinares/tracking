psychom <- function(x, p) (1 - p[3]) - 
  (.5 - p[3]) * cum_normal_fun(x, c(p[1], p[2]))