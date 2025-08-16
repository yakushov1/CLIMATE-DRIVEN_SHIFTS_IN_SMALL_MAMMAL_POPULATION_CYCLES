library(tidyverse)
library(patchwork)
library(ggpmisc)


# Import ------------------------------------------------------------------
monthly <- read_csv2("data/Bakhta_monthly_temperature.csv")
annualy <- read_csv2("data/Bakhta_annualy_temperature.csv")



# Graphs ------------------------------------------------------------------
annualy_graph <-  ggplot(annualy, aes(Year, y = Tavg_roll_mean_from_1976))+
  geom_line(aes(Year, Tavg), alpha = 0.2)+
  geom_line(aes(Year, Tavg_roll_mean), color = "#4682B4", linewidth = 1.1)+
  stat_poly_line(color = "red", alpha = 0.5, na.rm = T)+
  stat_poly_eq(use_label("eq"), na.rm = T)+
  stat_poly_eq(use_label(c("R2","p")), label.y = 0.9, na.rm = T)+
  theme_minimal()+
  labs(x = "Year",
       y = "Temperature, \u00B0C")+
  scale_y_continuous(labels = scales::label_number(accuracy = 1))



monthly_graph <- monthly |> 
  mutate(Month = case_when(Month == 1 ~ 'January',
                           Month == 2 ~ 'February',
                           Month == 3 ~ 'March',
                           Month == 4 ~ 'April',
                           Month == 5 ~ 'May',
                           Month == 6 ~ 'June',
                           Month == 7 ~ 'July',
                           Month == 8 ~ 'August',
                           Month == 9 ~ 'September',
                           Month == 10 ~ 'October',
                           Month == 11 ~ 'November',
                           Month == 12 ~ 'December',
                           )) |> 
  mutate(Month = factor(Month, levels = c('January', 'February', 'March', 'April',
                                          'May', 'June',  'July','August', 'September', 
                                          'October', 'November', 'December'))) |> 
  ggplot(aes(Year, y = Tavg_roll_mean_from_1976))+
  geom_line(aes(Year, Tavg), alpha = 0.2)+
  geom_line(aes(Year, Tavg_roll_mean), color = "#4682B4", linewidth = 1.1)+
  stat_poly_line(color = "red", alpha = 0.5, na.rm = T)+
  stat_poly_eq(use_label("eq"), na.rm = T)+
  stat_poly_eq(use_label(c("R2","p")), label.y = 0.9, na.rm = T)+
  theme_minimal()+
  labs(x = "Year",
       y = "Temperature, \u00B0C")+
  scale_y_continuous(labels = scales::label_number(accuracy = 1))+
  facet_wrap(~Month, scales = "free")+
  theme(axis.text.x = element_text(angle = 60))

  

total_graph <- annualy_graph / monthly_graph +
  plot_layout(heights = c(1, 3))

ggsave(total_graph, file= "images/figure_2.png", 
       device = png,
       width = 2480, height = 3100, units = "px")



