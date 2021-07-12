library(EnvCpt)
library(magrittr)

df = readr::read_delim('Logger 8.csv', ';') %>%
  dplyr::select(!X7) %>%
  dplyr::rename(date=`Time (YYYY-MM-DD hh:mm:ss)`, time_ms=`Time (ms)`, time=`Time since start (s)`, sensor_mv=`Raw, Sensor 1 - O2 (MilliVolt)` , sensor_mol= `Sensor 1 - O2 (μmol/L)`, cal_id=`Cal. ID, Sensor 1 - O2`) %>%
  dplyr::mutate(id=rank(date))

model_env = EnvCpt::envcpt(df$sensor_mol, models="trendcpt")
model_struc = strucchange::breakpoints(sensor_mol ~ time + 1, data=df, h=0.05)
model_ecp = ecp::e.divisive(df %>% dplyr::select(sensor_mol))

df %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x=id, y=sensor_mol)) + 
  # ggplot2::geom_vline(xintercept = model_env$trendcpt@cpts, color='red') + 
  # ggplot2::geom_vline(xintercept = model_struc$breakpoints, color='green') +
  ggplot2::geom_vline(xintercept = model_ecp$estimates, color='blue')
