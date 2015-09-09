library(weatherData)
getWeatherForDate("GNV", "2014-04-14")

ts <- as.data.frame(
  getDailyMinMaxTemp(station_id = "GNV", 
                   start_date = "2008-03-01",
                   end_date = "2013-12-01")
)

ts$date <- as.Date(substr(ts$TimeMin, 1, 10), 
                   format="%Y-%m-%d")
write.csv(ts, "E:/workingdirectory/mosquito/rainAndTemp/a_tsTemp2008-2013.csv")

