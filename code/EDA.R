library(tidyverse)
library(glue)
library(lubridate)
library(ggtext)
library(scales)


# get source Data temperature data
source("/Users/jamesiverson/Documents/DS/scripts/local_weather_RI_Script.R")
# source("/Users/jamesiverson/Documents/DS/scripts/Dav_Airport_WX.R")
# source("/Users/jamesiverson/Documents/DS/scripts/wx_NB_temp.R")

station_id <- "USC00117391"
station_location <- "Rock Island, IL"
current_year <- year(today())
max_year <- current_year -1

### Average daily temperature for each day of the year ###
avg_yearly_temps <- local_wx%>%
  select(date, year, tmax)%>%
  filter(year != current_year)%>% 
  group_by(year)%>%
  mutate(date = date, year = year, tmax_f = (9/5 * tmax) + 32,
    avg_temp = mean(tmax_f)
     )%>%
  distinct(year,avg_temp)

### calculate last year for baseline
min_year = min(avg_yearly_temps$year)

### get approx 1/3rd of the years for calculating baseline
cnt_sample_years <- round((max_year - min_year) / 3, 0)

### n years of data for baseline average ###
sampled_years <- avg_yearly_temps%>%
  filter(year <= (min_year + cnt_sample_years))

### Calc the first n years mean/baseline temp ###
std_ann_temp <- round(mean(sampled_years$avg_temp),2)

### Ready for plot ###
final_data <- avg_yearly_temps%>%
  ### t_diff = this years temp - baseline temp
  mutate(t_diff = avg_temp - std_ann_temp)

### Total Records Examined ###
total_years <- nrow(final_data)

### df for label ###
label_years <-  as.data.frame(
  x = factor(c(min_year, max_year))
)

### add t_diff
label_years%>%
  mutate(t_diff = 0)

label_data <- glue("Baseline mean of {std_ann_temp} F. calculated using the first {cnt_sample_years} of {total_years} years")

### Make a column plot ###
final_data%>%
  ggplot(aes(x = year, y = t_diff, fill = t_diff)) +
  geom_col(show.legend = TRUE) + 
  geom_hline(yintercept = 0, color = "white") +
  scale_fill_gradientn(
      colors=c("blue","white","red"),
      values = rescale(c(min(final_data$t_diff), 0, max(final_data$t_diff))),
      limits = c(min(final_data$t_diff) , max(final_data$t_diff)) ,
        name = "Farenheit"
      ) +
  theme_void() +
  labs(x=NULL,
       title = label_data,
       subtitle = glue("{station_location} WX Station {station_id} : {min_year} to {max_year}")  ) +
  theme(
      plot.background = element_rect(fill="black"),
      plot.title = element_text(hjust = .5, color = "white", size = rel(1)),
      plot.subtitle = element_text(hjust = .5, color = "white"),
      legend.text = element_text(color="white"),
      legend.title = element_text(color = "white" ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(t=20,r=10,b=25,l=10, unit = "pt")
    )
   

 ### save the chart ###
### ggsave("RI_Var_Mean.png", width = 6, height = 4)

