## an R script to get the urban score for every species
## by taking the median of their night-time lights values
## first will start with an initial exploratory figure though

# packages
library(ggplot2)
library(dplyr)
library(tidyr)

# read in inat observation data
inat_dat <- readRDS("data/lvl2_terra.rds")

# read in lights data
# where each record is assigned a lights value
lights <- readRDS("data/viirs_lights_obs.RDS")

# join the two
data <- inat_dat %>%
  left_join(., lights, by="catalogNumber") %>% 
  # Filters out observations without light pixel values.
  filter(!is.na(lights))

# number of species
length(unique(data$species))

# histogram of records per species
data %>%
  group_by(species) %>%
  summarize(N=n()) %>%
  ggplot(., aes(x=N))+
  geom_histogram(bins=50, color="black", fill="orange")+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of occurrences")+
  ylab("Count")

ggsave("outputs/graphics/number_records_by_species.png")

# calculate statistics of night-time lights value for every species
# and the number of records for each species into a dataframe
urban_scores <- data %>%
  group_by(species) %>%
  summarize(median_lights=median(lights),
            mean_lights=mean(lights),
            min_lights=min(lights),
            max_lights=max(lights),
            sd_lights=sd(lights),
            number_of_records=n())

saveRDS(urban_scores, "data/urbanness_summaries.RDS")