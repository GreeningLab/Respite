library(EnvCpt)
library(magrittr)

df = readr::read_delim('Logger 8.csv', ';') %>%
  dplyr::select(!X7) %>%
  dplyr::rename(date=`Time (YYYY-MM-DD hh:mm:ss)`, time_ms=`Time (ms)`, time=`Time since start (s)`, sensor_mv=`Raw, Sensor 1 - O2 (MilliVolt)` , sensor_mol= `Sensor 1 - O2 (μmol/L)`, cal_id=`Cal. ID, Sensor 1 - O2`) %>%
  mutate(id=rank(date))

model_env = EnvCpt::envcpt(df$sensor_mol, models="trendcpt")
model_struc = strucchange::breakpoints(sensor_mol ~ time + 1, data=df, h=0.05)

df %>% 
  ggplot() + 
  geom_line(aes(x=id, y=sensor_mol)) + 
  geom_vline(xintercept = model_env$trendcpt@cpts, color='red') + 
  geom_vline(xintercept = model_struc$breakpoints, color='green')
