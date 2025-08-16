library(tidyverse)
library(readxl)
library(MuMIn)

# XXI century -------------------------------------------------------------
XXI <- read_csv2('data/bad_snow_2005_2023.csv') 

XXI_for_model <- XXI |> 
  select(Sn_description, Sn, Tavg, diff) 
  

model_XXI <- glm(Sn_description~., XXI_for_model, family = "binomial")  
summary(model_XXI)
confint(model_XXI)


for_graph <- XXI |> 
  filter(Year > 2007,
         Sn_description == 1) |>
  filter(diff>1) |> 
  group_by(Year, Month) |> 
  summarise(N = n()) |> 
  mutate(cond = case_when(Year > 2012 ~ "good", .default = "bad"))

# number of days when snow was "bad" and temperature crossing zero 
# more then 1 times per day
# (figure 4)
trends <- ggplot(for_graph, aes(Year, N))+
  geom_col() +
  theme_minimal(base_size = 18)+
  labs(y = "Number of days",
       x = "Year")+
  scale_y_continuous(breaks = c(2,5,7))

ggsave(trends, file= "images/figure_4.png", device = png,
       width = 2500, height = 1500, units = "px")


# XX century --------------------------------------------------------------
XX <- read_csv2('data/bad_snow_1976_1994.csv')

model_XX <- glm(Sn_description~., XX, family = "binomial")  
summary(model_XX)
confint(model_XX)
