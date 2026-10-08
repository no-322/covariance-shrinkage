indus <- read.csv( "data/47indus-day.csv" )
indus <- indus[indus$X>20001231 & indus$X<20260101,]
rf <- indus$RF
mkt <- indus$RmRF / 100
ym   <- indus[,1] %/% 100 
filtered_indus <- indus[, 2: 48]
filtered_indus <- (filtered_indus- rf) / 100
