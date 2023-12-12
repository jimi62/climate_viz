library(tidyverse)
library(glue)
library(lubridate)
library(ggtext)
library(scales)

source("~/Documents/DS/Climate/climate/code/giss_global_temps.R")

current_year <- year(today())
max_year <- current_year -1

access_date <- today()

### Total Records Examined ###
total_years <- nrow(global_wx)

### df for label ###
label_years <-  as.data.frame(
  x = factor(c(min_year, max_year))
)


sub_title_data = glue("  Credit : GISTEMP Team, 2023: GISS Surface Temperature Analysis. \n  NASA Goddard Institute for Space Studies. \n  Dataset accessed {access_date} at https://data.giss.nasa.gov/gistemp/")

label_data <- "This graph shows the change in global surface \n temperature compared to the long-term average \n from 1951 to 1980."

global_wx%>%
ggplot(aes(x = year, y = t_diff)) +
  geom_line(aes(color = "1"), show.legend = FALSE) + 
  geom_point(fill = "white", aes(color = "1"), shape = 21, show.legend = TRUE) +
  geom_smooth(se = FALSE, size = 0.5, aes(color = "2"), span = 0.1 , show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1880, 2023, 20), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1), expand = c(0,0)) +
  scale_color_manual(name = label_data,
                     breaks = c(1,2),
                     values = c("gray","black"),
                     labels = c("Annual Mean", "Lowess Smoothing") ,
                     guide = guide_legend(override.aes = list(shape=15, size=5))
                     ) +
  labs( x= "YEAR", 
        y = "Temperature Anamoly in (C)", 
        title = "Global Land-Ocean Temperature Index", subtitle = sub_title_data) +
  theme_light() +
  theme(axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b=10),   color = "red" , face = "bold"),
        plot.subtitle = element_text(margin = margin(b=15),   color = "black"),
        legend.position = c(.2,.75) ,
        legend.margin = margin(.75,0.75,0.75,0.75) ,
        legend.title =  element_text(size= 5, face = "italic"),
        legend.background = element_rect(fill=NULL, color = "lightgray")
        )


 ### save the chart ###
 ggsave("figures/Global temperature Index.png", width = 7, height = 5)

