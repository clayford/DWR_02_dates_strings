library(lubridate)

mcu <- read.csv("marvel_movies.csv", stringsAsFactors = FALSE)
mcu$release_date <- dmy(mcu$release_date)


as.duration(mcu$run_time)
as.period(as.duration(mcu$run_time))

mcu$release_date <- as.period(as.duration(mcu$run_time))
