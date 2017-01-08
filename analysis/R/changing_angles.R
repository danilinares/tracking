totalerrorToDegSemi <- function(x) (x + 90) %% 180 - 90
degSemiToDegFull <- function(x) 2 * x
degFullToDegSemi <- function(x) x / 2
degFullToRadFull <- function(x) circular(x/180*pi, units = 'radians') 
radFullToDegFull <- function(x) circular(x/pi*180, units = 'deg') 