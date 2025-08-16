library(tidyverse)
library(patchwork)
library(WaveletComp)
library(biwavelet)
library(viridis)


# Numbers of dominant species (right bank) --------------------------------
dominant_species <- read_csv2('data/numbers_of_dominant_species.csv')

numbers_graph <- ggplot(dominant_species, aes(date, num))+
  geom_line()+
  geom_point()+
  facet_grid(~century, scale = 'free')+
  labs(x = NULL,
       y = 'Catch index')+
  theme_minimal(base_size = 14)+
  theme(strip.text = element_blank())



# Wavelet analysis (biwavelet) ---------------------------------------------------------------

# row data
XX <- as.matrix(dominant_species |> 
  filter(date<2000) |> 
  select(-century))

XXI <- as.matrix(dominant_species |> 
                  filter(date>2000) |> 
                  select(-century))

# wavelet-transform
XX_wavelet <- biwavelet::wt(XX, sig.level = 0.95)
XXI_wavelet <- biwavelet::wt(XXI, sig.level = 0.95)



# preparing tibble for ggplot2
# XX century
XX_wavelet_df <- as_tibble(XX_wavelet$power) |> # Wavelet-power
  rename_with(~as.character(1976:1994)) |> 
  mutate(Period = XX_wavelet$period) |> 
  pivot_longer(cols = -Period, names_to = 'Year', values_to = 'Power') |> 
  left_join(
    (# significance levels
    as_tibble(XX_wavelet$signif) |>
      rename_with(~as.character(1976:1994)) |>
      mutate(Period = XX_wavelet$period) |>
      pivot_longer(cols = -Period, names_to = 'Year', values_to = 'sign')),
    by = c('Period', 'Year')) |> 
  mutate(Year = as.numeric(Year))

# cone of influence
XX_coi <- as_tibble(XX_wavelet$coi) |> 
  mutate(Year = c(1976:1994)) |> 
  rename(coi = value)


# XXI century
XXI_wavelet_df <- as_tibble(XXI_wavelet$power) |> # Power
  rename_with(~as.character(2008:2023)) |> 
  mutate(Period = XXI_wavelet$period) |> 
  pivot_longer(cols = -Period, names_to = 'Year', values_to = 'Power') |> 
  left_join(
    (
    # significance levels
    as_tibble(XXI_wavelet$signif) |>
      rename_with(~as.character(2008:2023)) |>
      mutate(Period = XXI_wavelet$period) |>
      pivot_longer(cols = -Period, names_to = 'Year', values_to = 'sign')),
    by = c('Period', 'Year')) |> 
  mutate(Year = as.numeric(Year))

# cone of influence
XXI_coi <- as_tibble(XXI_wavelet$coi) |> 
  mutate(Year = c(2008:2023)) |> 
  rename(coi = value)


# Combine all the data into one tibble
total_wavelet <- rbind(
  XX_wavelet_df |> 
  left_join(XX_coi, by = 'Year') |> 
  mutate(Century = 'XX'),
XXI_wavelet_df |> 
  left_join(XXI_coi, by = 'Year') |> 
  mutate(Century = 'XXI')
) |> 
  group_by(Century) |> 
  mutate(Power_scaled = scales::rescale(Power),# otherwise, the colors of the facet wrap will be incorrect
         Century = as.factor(Century)) |> 
  ungroup() 





spectr <- ggplot(total_wavelet) +
  geom_tile(aes(x = Year, y = Period, fill = Power_scaled)) +
  geom_contour(aes(x = Year, y = Period, z = as.numeric(sign<1)),
               breaks = 0.5, 
               color = "white", 
               size = 1)+
  geom_line(aes(x = Year, y = coi), 
            color = "black", size = 0.5, linetype = 2)+
  scale_fill_viridis(option = "C") +
  scale_y_continuous(trans = 'log2',
                     breaks = c(2, 4)) +
  labs(x = "Year", y = "Period") +
  coord_cartesian(ylim = c(2,5))+
  theme_minimal(base_size = 14)+
  theme(legend.position = 'None',
         strip.text = element_blank())+
  facet_grid(~Century, scale = 'free')

# Average-wavelet (Waveletcomp) -------------------------------------------
XX_waveletcomp <- as_tibble((WaveletComp::analyze.wavelet(
  (dominant_species |>
     filter(date<2000) |> 
     select(-century)),
  "num",
  loess.span=0,
  make.pval=T,
  lowerPeriod = 2,
  upperPeriod = 5)
)[c(9, 11, 13)]) |> 
  mutate(Significant = case_when(Power.avg.pval<0.05 ~ Power.avg),
         Century = 'XX')


XXI_waveletcomp <- as_tibble((WaveletComp::analyze.wavelet(
  (dominant_species |>
     filter(date>2000) |> 
     select(-century)),
  "num",
  loess.span=0,
  make.pval=T,
  lowerPeriod = 2,
  upperPeriod = 5)
)[c(9, 11, 13)]) |> 
  mutate(Significant = case_when(Power.avg.pval<0.05 ~ Power.avg),
         Century = 'XXI')

waveletcomp_avg_data <- rbind(XX_waveletcomp, XXI_waveletcomp)


avg_power_graph <- ggplot(waveletcomp_avg_data)+
  geom_line(aes(Period, Power.avg))+
  #geom_point(aes(Period, Significant), na.rm = T, size = 2, col = 'red')+
  facet_grid(~Century)+
  theme_minimal(base_size = 14)+
  theme(strip.text = element_blank(),
        legend.position = 'none')+
  labs(y = 'Power')+
  coord_flip()


# Total Graphs ------------------------------------------------------------------

numbers_spectr_average <- numbers_graph / spectr / avg_power_graph +
  plot_annotation(tag_levels = 'A')

ggsave('images/figure_1.png',
       numbers_spectr_average,
       device = png,
       width = 2480, height = 3100, units = "px")



