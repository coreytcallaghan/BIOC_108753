# Figure 11

# Data Import
towns_summary <- readRDS("data/towns_summary.RDS")

# Define town obervation cutoff.
# Using a town observation cutoff of 30
town_obs_cutoff <- 30
towns_summary <- towns_summary %>% filter(obs_number >= town_obs_cutoff)
#------------------------------------------------------------------------------
# Median Observation Score vs. Town Urban Score Linear Model
mod <- lm(median_species_urbanness_dist ~ median_underlying_score,
           data = towns_summary)

# Visualizaing the model. 
ggplot(data = towns_summary,
       aes(x = median_underlying_score,
           y = median_species_urbanness_dist)) +
  geom_point(inherit.aes = TRUE) +
  geom_smooth(inherit.aes = TRUE, method = "lm") +
  scale_x_log10() +
  scale_y_log10()

# Extracting Model Residuals and Binding to Town Observation Count.
town_residuals <- bind_cols(
  select(towns_summary, TOWN, obs_number),
  select(broom::augment(mod), .resid)
)

# Visualizing residuals against town observation count.
ggplot(data = town_residuals,
       aes(x = obs_number,
           y = .resid)) +
  geom_point(inherit.aes = TRUE) +
  geom_smooth(inherit.aes = TRUE, method = "lm", color = "orange") +
  scale_x_log10() +
  labs(
    subtitle = "Regression Residual Equation: Town Biodiversity Urbanness Index (TBUI) ~ Town Underlying Urbanness Index (TUUI)",
    x = "Number of Observations in Town (log10)",
    y = "Regression Residuals"
  ) + 
  theme_bw() + 
  theme(axis.text=element_text(color="black"))

#ggsave(filename = "outputs/graphics/TBUI_TUUI_lm_resid_vs_obs_number.png", width = 10)
