oneColumnWidth <- 3.42
onehalfColumnWidth <- 4.5
twoColumnWidth <- 7
sizeLine1 <- .25
sizeText <- 2.5

theme_track <- theme_set(theme_classic(10))
theme_track <- theme_update(strip.background = element_blank(),
                            axis.ticks= element_line(size = sizeLine1),
                            axis.line = element_line(size=sizeLine1))
