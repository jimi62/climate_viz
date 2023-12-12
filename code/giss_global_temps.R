library(tidyverse)
library(glue)
library(lubridate)
library(ggtext)

wx_data_url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

### read in the data from nasa giss website
wx_data <- read_csv(wx_data_url, skip = 1, na = "***")


### global temps Jan through december value ( 'J-D') ###
global_wx <- wx_data%>%
  select(year =Year, t_diff = `J-D`)

