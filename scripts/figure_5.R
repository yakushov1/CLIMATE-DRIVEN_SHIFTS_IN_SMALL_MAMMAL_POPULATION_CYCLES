library(tidyverse)


data <- read_csv2('data/numbers_of_days_with_bad_snow.csv') |> 
  arrange(Year) |> 
  mutate(catch = case_when(is.na(catch) ~ 'n',
                           .default = catch))


figure_5 <-   ggplot(data, aes(Year, count))+
  geom_col(aes(fill = as.factor(catch)), size = 4)+
  labs(x = "Year",
       y = "Number of days")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c( "blue", "transparent", "red"))+
  theme(text = element_text(size = 18))

ggsave(figure_5, file= "images/figure_5.png",
       device = png,
       width = 2480, height = 1100, units = "px")
