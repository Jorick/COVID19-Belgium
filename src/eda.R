library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(lubridate)

# set up colors
color_set <- list("darkblue" = "#121F21",
                  "blue" = "#709293",
                  "grey" = "#C1BBA7",
                  "yellow" = "#F8CA91",
                  "red" = "#E63E25"
                  )

df <- read_excel("data/COVID19BE.xlsx")

df <- df %>% 
  group_by(DATE, PROVINCE, REGION) %>%
  mutate(DATE = ymd(DATE))

cases_province <- df %>%
  summarise(CASES = sum(CASES))
  
ggplot(data = cases_province, aes(x = DATE, y = CASES)) +
  geom_line(color = color_set$blue) +
  geom_smooth(method = "loess", se = F, span = 0.1,
              size = 1, alpha = 0.8, color = color_set$red) +
  facet_wrap(~ PROVINCE) +
  theme_minimal() +
  labs(title = "COVID-19 in Belgium",
       subtitle = "Plots for each province with total number of cases",
       x = "Date",
       y = "Number of confirmed cases",
       caption = "Data obtained from Sciensano: https://www.sciensano.be/en") +
  theme(plot.background = element_rect(fill = color_set$grey),
        text = element_text(family = "Helvetica"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey10",
                                          size = 0.1),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color = color_set$darkblue,
                                  hjust = 0.5,
                                  size = 20),
        plot.margin = unit(c(1,1,1,1), "lines"),
        plot.subtitle = element_text(color = color_set$darkblue,
                                     hjust = 0.5,
                                     size = 15),
        plot.caption = element_text(color = color_set$darkblue,
                                    size = 7),
        axis.line = element_line(color = color_set$darkblue),
        axis.text =  element_text(color = color_set$darkblue,
                                    hjust = 0.5),
        axis.title = element_text(color = color_set$darkblue,
                                    hjust = 0.5),
          )

ggsave(filename = "COVID_Belgium_by_province.png",
       path = "output",
       device = "png",
       dpi = "retina")
