library(tidyverse)
library(patchwork)
library(corrplot)
library(FactoMineR) # PCA
library(factoextra) # PCA visualizing
library(DHARMa)
library(mgcv)  # GLM & GAM
library(MuMIn)
library(glmnet)
library(boot)



# Mortality calculations -------------------------------------------------------
winter_survival <- read_csv2('data/winter_survival.csv')

ggplot(winter_survival, aes(Year, survival_rate))+
  geom_col()+
  facet_wrap(~Spec)


four_spec_only <- winter_survival |> 
  filter(Spec %in% c('S._araneus', 'S._isodon', 'M._oeconomus', 'C._rutilus')) |> 
  group_by(Year) |> 
  summarise(across(c(Cyl_day_100_aug, Cyl_day_100_jun, survival_rate), sum)) |> 
  mutate(survival_rate = case_when(survival_rate > 0 ~ 0,
                                   .default = survival_rate)) |> 
  mutate(survival_rate = abs(survival_rate)) |> 
  filter(survival_rate>0) 



ggplot(four_spec_only, aes(Year, survival_rate))+
  geom_col() 


# Climatic data ----------------------------------------------------

# Amount of days with 'bad' snow cover 

bad_snow <- read_csv2('data/climatic_details/all_station_bad_snow.csv') |> 
  filter(Station == "Бахта") |> 
  select(Year, count)
  

# amount of melting-freezing by month
melt_freeze_by_month <- read_csv2('data/climatic_details/melt_freeze_by_month.csv') |> 
  pivot_wider(id_cols = Year, values_from = Melt_freeze, names_from = Month, names_prefix = 'Melt_freeze_') |> 
  mutate(across(c(Melt_freeze_9, Melt_freeze_10, Melt_freeze_11), \(x) lag(x))) |>  # will affect to the next year
  mutate(across(everything(), \(x) case_when(is.na(x) == T ~ 0, .default = x)))



season_by_temperature <- read_csv2('data/climatic_details/season_by_temperature.csv') |> 
  select(Year, Season, Tavg, Sn_avg, Duration) |> 
  filter(Season != 'Summer') |> 
  pivot_wider(id_cols = Year, names_from = Season, values_from = c(Tavg, Sn_avg, Duration)) |> 
  mutate(across(ends_with('Autumn'), \(x) lag(x))) |> 
  mutate(across(everything(), \(x) case_when(is.na(x) == T ~ 0, .default = x)))


for_model <- four_spec_only |> 
  select(Year, survival_rate) |> 
  left_join(bad_snow, by = 'Year') |> 
  mutate(count = case_when(is.na(count)~ 0, .default = count)) |> 
  # mutate(First_half = case_when(is.na(First_half)~ 0, .default = First_half),
  #        Last_half = case_when(is.na(Last_half)~ 0, .default = Last_half)) |> 
  left_join(melt_freeze_by_month, by = 'Year') |> 
  left_join(season_by_temperature, by = 'Year') |> 
  mutate(Period = case_when(Year %in% c(1976:1990, 2017:2023)~1, .default = 0)) |> 
  select(-Year)

rm(bad_snow, four_spec_only, melt_freeze_by_month, season_by_temperature, winter_survival)


# Corr matrix  --------------------------------------------------

cor_mat <- round(cor(for_model),2)

corrplot(cor_mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)


# Target's distribution ------------------------------------------

ggplot(for_model, aes(survival_rate))+
  geom_histogram()

# PCA ------------------------------

for_GLM_PCA <- for_model |> 
  select(contains('Melt_freeze'), contains('Spring'),
         contains('Autumn'),
         contains('Winter'),
         count) |> 
  mutate(Melt_freeze_spring = Melt_freeze_4 + Melt_freeze_5 + Melt_freeze_3, 
         Melt_freeze_autumn = Melt_freeze_10 + Melt_freeze_11) |> 
  select(7:18)


pca <- PCA(for_GLM_PCA, scale.unit = TRUE, ncp = 4, graph = TRUE)

pca_for_text <- as_tibble(get_eigenvalue(pca), rownames = 'PC') |> 
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  mutate(PC = str_replace(PC, 'Dim.', 'PC')) |> 
  slice(1:5) |> 
  rename(Eigenvalue = eigenvalue,
         `Variance percent` = variance.percent,
         `Cumulative variance percent` = cumulative.variance.percent)


# first 4 principal comp
dimdesc(pca, axes=c(1:4))

# correlation between PC and variables - for text
PC1_load <- as_tibble(dimdesc(pca, axes=c(1:4))$Dim.1[['quanti']], rownames = 'Predictor') |> 
  mutate(PC = 'PC1')

PC2_load <- as_tibble(dimdesc(pca, axes=c(1:4))$Dim.2[['quanti']], rownames = 'Predictor') |> 
  mutate(PC = 'PC2')

PC3_load <- as_tibble(dimdesc(pca, axes=c(1:4))$Dim.3[['quanti']], rownames = 'Predictor') |> 
  mutate(PC = 'PC3')

PC4_load <- as_tibble(dimdesc(pca, axes=c(1:4))$Dim.4[['quanti']], rownames = 'Predictor') |> 
  mutate(PC = 'PC4')

all_load <- rbind(PC1_load,PC2_load,PC3_load, PC4_load) |> 
  rename(`p-value` = p.value) |> 
  pivot_wider(id_cols = Predictor, names_from = PC, values_from = c(correlation, `p-value`)) |> 
  select(Predictor, contains('PC1'), contains('PC2'), contains('PC3'), contains('PC4')) |> 
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  arrange(Predictor) |> 
  select(!contains('p-value'))

rm(PC1_load, PC2_load, PC3_load, PC4_load)

# Graphs
fviz_pca_var(pca,
             col.var = "contrib",          # color according contribution
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE,                  
             select.var = list(contrib = 5), # top 5variables
             title = "PCA - loads",
             axes = c(1,2)) 



# contributing of variables to first 4 PC
(((fviz_contrib(pca, choice = "var", axes = 1, top = 8))+
(fviz_contrib(pca, choice = "var", axes = 2, top = 8)))/
((fviz_contrib(pca, choice = "var", axes = 3, top = 8))+
(fviz_contrib(pca, choice = "var", axes = 4, top = 8))))


for_GLM <- cbind(for_model |> 
                   select(survival_rate, Period),
                 as_tibble(pca$ind$coord)) |> 
  mutate(Period = as_factor(Period)) |> 
  rename(mortality = survival_rate,
         PC1 = Dim.1,
         PC2 = Dim.2,
         PC3 = Dim.3,
         PC4 = Dim.4)




# Model -------------------------------------------------------------------
# the influence of climatic conditions on winter mortality

model_with_interaction <- glm(
  mortality ~ Period * (PC1 + PC2 + PC3 + PC4),  
  family = Gamma(link = "log"),
  data = for_GLM
)


# Model diagnostics
sim_res <- DHARMa::simulateResiduals(model_with_interaction)
plot(sim_res) # no significant problems were detected

testResiduals(sim_res)



# GLM  coefficients for article -----------------------------------

conf_intervals <- as_tibble(exp(confint.default(model_with_interaction)), rownames = 'Par')


summary_table <- as_tibble(summary(model_with_interaction)$coefficients, rownames = 'Par') |>
  left_join(conf_intervals, by = 'Par') |> 
  mutate(across(where(is.numeric), \(x) round(x, 3)))




# Lasso  -----------------------------------------------------
# the influence of climatic conditions on regime shift

for_glmnet <- for_model |> 
  select(!survival_rate) |>
  mutate(Melt_freeze_spring = Melt_freeze_4 + Melt_freeze_5 + Melt_freeze_3, 
         Melt_freeze_autumn = Melt_freeze_10 + Melt_freeze_11) |> 
  mutate(Period = as.factor(Period)) |> 
  select(1, 8:19) |> 
  mutate(across(c(1:10, 12,13), \(x) as.vector(scale(x))))


X <- for_glmnet |> 
  select(-Period) |> 
  as.matrix()

y = for_glmnet |> 
  select(Period) |> 
  as.matrix()

# alpha = 1  # 1 — Lasso, 0 — Ridge, 0.5 — Elastic Net
set.seed(123) 
lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")


# Set bootstrap parameter
n_bootstrap <- 1000  # amount of bootstrap samples
alpha <- 0.5        # parameter for Elastic Net
lambda <- lasso$lambda.1se  # use optimal lambda

# Matrix for bootstrap coefficient
bootstrap_coefs <- matrix(NA, nrow = n_bootstrap, ncol = ncol(X))
colnames(bootstrap_coefs) <- colnames(X)

# Start bootstrapping
set.seed(123)  
for (i in 1:n_bootstrap) {
  bootstrap_indices <- sample(nrow(X), replace = TRUE)
  X_boot <- X[bootstrap_indices, ]
  y_boot <- y[bootstrap_indices]
  
  boot_model <- glmnet(X_boot, y_boot, alpha = alpha, lambda = lambda, family = "binomial")
  

  bootstrap_coefs[i, ] <- as.vector(coef(boot_model))[-1]  # except intercept
}

# Analysis of results of bootstrap
# 1. Confidence intervals
confidence_intervals <- apply(bootstrap_coefs, 2, function(x) quantile(x, c(0.025, 0.975)))

# 2. non zero proportions
non_zero_proportions <- colMeans(bootstrap_coefs != 0)

# 3. Average coefficients
mean_coefs <- colMeans(bootstrap_coefs)


results <- tibble(
  Feature = colnames(X),
  Mean_Coefficient = mean_coefs,
  CI_lower = confidence_intervals[1, ],
  CI_upper = confidence_intervals[2, ],
  Non_zero_proportion = non_zero_proportions
) |> 
  mutate(across(2:5, \(x) round(x,3))) |> 
  arrange(desc(abs((Mean_Coefficient)))) # Отсортируем по важности (по абсолютному значению коэффициента)




results_top <- results |> 
  slice(1:3) |> 
  mutate(Feature = case_when(Feature == 'Tavg_Spring' ~ 'Tavg (spring)',
                             Feature == 'Sn_avg_Spring' ~ 'Sn avg (spring)',
                             .default = Feature))



lasso_graph <- ggplot(results_top, aes(x = reorder(Feature, -Mean_Coefficient), y = Mean_Coefficient)) +
  geom_col(fill = "skyblue", width = 0.7) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(x = "", y = "Standardized coefficient value") +
  theme_minimal(base_size = 14)


ggsave('images/figure_3.png', lasso_graph, device = 'png', bg = 'white')
