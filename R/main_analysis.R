### An analysis script highlighting the figures and results in the analysis

# packages
library(readr)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(ggcorrplot)
library(tidyr)
library(patchwork)
library(RColorBrewer)
library(performance)

# READ IN THE VARIOUS DATA SETS NECESSARY

# assigned urbanness scores
urbanness <- readRDS("data/urbanness_summaries.RDS")

# read in cnc only data
cnc_data <- readRDS("data/inat_cnc.rds")

# species list from CNC area
species_in_cnc <- cnc_data %>%
  dplyr::select(species) %>%
  distinct()

# read in regional/semi-continental lvl2 data
# split into four rds to fit into repository
lvl2.1 <- readRDS("data/lv2.1.rds")
lvl2.2 <- readRDS("data/lv2.2.rds")
lvl2.3 <- readRDS("data/lv2.3.rds")
lvl2.4 <- readRDS("data/lv2.4.rds")

lvl2 <- lvl2.1 %>%
  bind_rows(lvl2.2) %>%
  bind_rows(lvl2.3) %>%
  bind_rows(lvl2.4)

rm(lvl2.1)
rm(lvl2.2)
rm(lvl2.3)
rm(lvl2.4)

# read in sf of cnc_area
cnc_area <- readRDS("data/cnc_area.rds")

# read in viirs_lights_obs
lights_data <- readRDS("data/viirs_lights_obs.RDS")

# lights for each town data
lights_towns <- readRDS("data/viirs_lights_towns.RDS")

# native and non-native status
native_status <- read_csv("data/common_species_status.csv")


# trim urbanness values to those with > 100 observations
species_urban_scores <- urbanness %>%
  dplyr::filter(species %in% species_in_cnc$species) %>%
  dplyr::filter(number_of_records>=100)

# look at the relationship between the 0.25, median, mean etc.
# for each species
# based off lights data (i.e., urbanization level)
lvl2 %>%
  dplyr::filter(species %in% species_urban_scores$species) %>%
  left_join(., lights_data, by="catalogNumber") %>%
  group_by(species) %>%
  summarize(med=median(lights, na.rm=TRUE),
            mean=mean(lights, na.rm=TRUE),
            quantile_0.25=quantile(lights, 0.25, na.rm=TRUE),
            quantile_0.75=quantile(lights, 0.75, na.rm=TRUE)) %>%
  dplyr::select(-species) %>%
  cor(.) %>%
  ggcorrplot(lab=TRUE)

#ggsave("outputs/graphics/distribution_summary_correlation.png", width=6.8, height=5, units="in")

# plot histogram of these 1004 species
ggplot(species_urban_scores, aes(x=median_lights))+
  geom_histogram(fill="orange", color="black", bins=40)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Count")+
  xlab("Urban score (median lights)")

#ggsave("outputs/graphics/urban_score_histogram.png", width=6.8, height=5, units="in")

# re make this plot, but with a log-transformed axis
ggplot(species_urban_scores, aes(x=median_lights))+
  geom_histogram(fill="orange", color="black", bins=40)+
  theme_classic()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  ylab("Count")+
  xlab("log Urban score (median lights)")

#ggsave("outputs/graphics/urban_score_histogram_log-scale.png", width=6.8, height=5, units="in")

# now try and get a list of 6 or so species to plot where they occur along this axis
# first create a tiny dataframe with this
example_species <- species_urban_scores %>%
  dplyr::filter(species %in% c("Pseudacris crucifer",
                               "Symplocarpus foetidus",
                               "Asclepias syriaca",
                               "Bombus impatiens",
                               "Reynoutria japonica",
                               "Sciurus carolinensis",
                               "Maianthemum canadense",
                               "Thamnophis sirtalis")) %>%
  mutate(y=c(170, 125, 125, 125, 125, 125, 125, 125)) %>%
  mutate(Common_name=c("Common Milkweed", "Common Eastern Bumble Bee", 
                       "Canada Mayflower",  "Spring Peeper",
                       "Japanese Knotweed", "Eastern Gray Squirrel",
                       "Eastern Skunk Cabbage", "Common Garter Snake"))


# now combine with previous plot
example_species2 <- example_species %>%
  dplyr::filter(species != "Asclepias syriaca") %>%
  dplyr::filter(species != "Bombus impatiens") %>%
  dplyr::filter(species != "Maianthemum canadense")

ggplot()+
  geom_histogram(data=species_urban_scores, aes(x=median_lights), 
                 fill="orange", color="black", bins=40)+
  geom_segment(data=example_species2, aes(x=median_lights, xend=median_lights, 
                                          y=y, yend=0), color="red", 
               size=1.5, linetype="dashed")+
  geom_label(data=example_species2, aes(x=median_lights, y=y, label=Common_name))+
  theme_classic()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  ylab("Number of species")+
  xlab("log Urban score (median lights)")+
  ylim(0, 200)

#ggsave("outputs/graphics/urban_score_histogram_log-scale_with_labels.png", width=7.5, height=5, units="in")


ggplot()+
  geom_histogram(data=species_urban_scores, aes(x=median_lights), 
                 fill="orange", color="black", bins=40)+
  geom_segment(data=example_species2, aes(x=median_lights, xend=median_lights, 
                                          y=y, yend=0), color="red", 
               size=1.5, linetype="dashed")+
  #geom_label(data=example_species2, aes(x=median_lights, y=y, label=Common_name))+
  theme_bw()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  ylab("Number of species")+
  xlab("log Urban score (median lights)")+
  ylim(0, 200)

#ggsave("outputs/graphics/urban_score_histogram_log-scale_without_labels.png", width=7.5, height=5, units="in")

# make example plots of the distributions
# for our 6 example species
light_dat_subsetted <- lvl2 %>%
  dplyr::filter(species %in% example_species$species) %>%
  left_join(., lights_data, by="catalogNumber") %>%
  group_by(species) %>%
  mutate(med_lights=median(lights, na.rm=TRUE)) %>%
  left_join(., example_species)

ggplot(light_dat_subsetted, aes(x=lights, fill=species))+
  geom_density(color="black")+
  scale_x_log10()+
  facet_wrap(~Common_name, ncol=2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Dark2")

#ggsave("outputs/graphics/species_distribution_examples.png", width=7.5, height=5, units="in")

# plot the same thing, but with free scales
ggplot(light_dat_subsetted, aes(x=lights, fill=species))+
  geom_density(color="black")+
  scale_x_log10()+
  facet_wrap(~Common_name, scales="free_y", ncol=2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Dark2")

#ggsave("outputs/graphics/species_distribution_examples_free_y_scales.png", width=7.5, height=5, units="in")

# Now lets look at the relationship between the 'continental/regional' scores
# and the score sif they were assigned locally
# first get a list of the most abundant species within the cnc area
# but put this in a function to do for different levels of cutoffs
# based on comments from Mark
get_local_data_trimmed <- function(cutoff_level){
  
  common_species_in_cnc <- cnc_data %>%
    group_by(species) %>%
    summarize(N=n()) %>%
    dplyr::filter(N>=cutoff_level) %>%
    .$species
  
  cnc_urbanness <- cnc_data %>%
    dplyr::filter(species %in% common_species_in_cnc) %>%
    left_join(., lights_data, by="catalogNumber") %>%
    group_by(species) %>%
    summarize(cnc_urban_score=median(lights, na.rm=TRUE)) %>%
    mutate(Minimum_number_obs=cutoff_level)
  
  return(cnc_urbanness)
}

cnc_urbanness <- bind_rows(lapply(c(100, 90, 80, 70, 60, 50, 40, 30, 20), get_local_data_trimmed))


# now look at the cnc urban score versus the regional urban score
# with a cutoff of 50
plot_temp <- cnc_urbanness %>%
  dplyr::filter(Minimum_number_obs==50) %>%
  left_join(., urbanness, by="species") %>%
  dplyr::select(species, regional_urban_score=median_lights, number_of_records, cnc_urban_score) %>%
  dplyr::filter(number_of_records>=100)

ggplot(plot_temp, aes(y=cnc_urban_score, x=regional_urban_score))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Boston CNC urbanness (log)")+
  xlab("Regional urbanness (log)")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  ggtitle(paste0("N = ", nrow(plot_temp), " species"))

#ggsave("outputs/graphics/regional_vs_local_urbanness_scores.png", width=7.5, height=5, units="in")

summary(lm(cnc_urban_score ~ regional_urban_score, data=plot_temp))


# make a new plot with all levels of 'cutoff' used
plot_temp2 <- cnc_urbanness %>%
  left_join(., urbanness, by="species") %>%
  dplyr::select(species, regional_urban_score=median_lights, 
                number_of_records, cnc_urban_score, Minimum_number_obs) %>%
  dplyr::filter(number_of_records>=100) %>%
  mutate(Minimum_number_obs=as.character(as.integer(Minimum_number_obs)))

ggplot(plot_temp2, aes(y=cnc_urban_score, x=regional_urban_score, 
                       color=Minimum_number_obs))+
  #geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Boston CNC urbanness (log)")+
  xlab("Regional urbanness (log)")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", se=FALSE)+
  geom_abline(intercept = 0, slope = 1, color="blue")


# number of obs in CNC area that will be used
# i.e., removing the species in the cnc area which have < 100 observations througout
# the level 2 terra region as a whole

# first get list of species with >100 obs
greater_than_100_obs <- species_urban_scores %>%
  dplyr::filter(number_of_records>=100) %>%
  .$species

# remove obs
cnc_trimmed <- cnc_data %>%
  dplyr::filter(species %in% greater_than_100_obs)

(nrow(cnc_trimmed)/nrow(cnc_data))*100


# summarize these taxonomically
length(unique(cnc_trimmed$kingdom))
length(unique(cnc_trimmed$phylum))
length(unique(cnc_trimmed$class))
length(unique(cnc_trimmed$order))
length(unique(cnc_trimmed$family))
cnc_trimmed %>% 
  group_by(phylum) %>%
  summarize(N=n()) %>%
  mutate(total_obs=nrow(cnc_trimmed)) %>%
  mutate(percent=(N/total_obs)*100) -> phyla


# make a Table S1 for the manuscript
# get number of total obs
# urban score
# and number of Boston obs
boston_obs <- cnc_trimmed %>%
  group_by(species) %>%
  summarize(`Number of obs in Boston CNC`=n()) %>%
  rename(Species=species)

table_s1 <- species_urban_scores %>%
  dplyr::select(species, median_lights, number_of_records) %>%
  rename(`Urban score`=median_lights) %>%
  rename(`Number of regional obs.`=number_of_records) %>%
  left_join(., cnc_trimmed %>%
              dplyr::select(species, family, class, phylum, kingdom) %>%
              distinct(), by="species") %>%
  rename(Species=species) %>%
  rename(Family=family) %>%
  rename(Class=class) %>%
  rename(Phylum=phylum) %>%
  rename(Kingdom=kingdom) %>%
  left_join(., boston_obs) %>%
  dplyr::select(1, 4:7, 2, 3, 8)

write_csv(table_s1, "outputs/table_s1.csv")

# how many observers
length(unique(cnc_trimmed$recordedBy))

# sd, min, max obs by observers
observers <- cnc_trimmed %>%
  group_by(recordedBy) %>%
  summarize(N=n())

min(observers$N)
max(observers$N)
sd(observers$N)
mean(observers$N)

# summarize the urbanness
mean(species_urban_scores$median_lights)
sd(species_urban_scores$median_lights)

cnc_number_obs <- cnc_data %>%
  group_by(species) %>%
  summarize(N=n())

top_3 <- species_urban_scores %>%
  arrange(desc(median_lights)) %>%
  slice(1:3)

top_3_cnc_obs <- cnc_number_obs %>%
  dplyr::filter(species %in% top_3$species)

bottom_3 <- species_urban_scores %>%
  arrange(median_lights) %>%
  slice(1:3)

bottom_3_cnc_obs <- cnc_number_obs %>%
  dplyr::filter(species %in% bottom_3$species)

# analysis of native vs non-native species
natives_plot <- native_status %>%
  dplyr::filter(`CNC Sample Size` >= 20) 

natives_plot %>%
  lm(log10(`LVL2 Median Urban Score`) ~ `Native/non-native`, data=.) %>%
  summary()

ggplot(natives_plot, aes(x=`LVL2 Median Urban Score`, fill=`Native/non-native`))+
  geom_histogram(color="black", bins=50, alpha=0.6)+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Number of species")+
  xlab("log Urban score (median lights)")+
  scale_fill_brewer(palette="Dark2")+
  labs(fill="Status: ")+
  theme(legend.position="bottom")+
  ggtitle("b)") -> natives_vs_nonnatives

brewer.pal(name="Dark2", n=4)

ggplot()+
  geom_histogram(data=species_urban_scores, aes(x=median_lights), 
                 fill="#7570B3", color="black", bins=40)+
  geom_segment(data=example_species2, aes(x=median_lights, xend=median_lights, 
                                          y=y, yend=0), color="red", 
               size=1.5, linetype="dashed")+
  #geom_label(data=example_species2, aes(x=median_lights, y=y, label=Common_name))+
  theme_bw()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  ylab("Number of species")+
  xlab("log Urban score (median lights)")+
  ylim(0, 200)+
  ggtitle("a)") -> all_species

all_species + natives_vs_nonnatives + plot_layout(ncol=1)

#ggsave("outputs/graphics/urban_score_histogram_both.png", width=7.5, height=10, units="in")

example_3 <- example_species2 %>%
  dplyr::filter(Common_name %in% c("Spring Peeper", "Eastern Gray Squirrel", "Eastern Skunk Cabbage"))

lvl2 %>%
  dplyr::filter(species %in% example_species$species) %>%
  left_join(., lights_data, by="catalogNumber") %>%
  group_by(species) %>%
  mutate(med_lights=median(lights, na.rm=TRUE)) %>%
  left_join(., example_species) %>%
  dplyr::filter(species %in% example_3$species) %>%
  ggplot(., aes(x=lights, fill=species))+
  geom_density(color="black")+
  scale_x_log10()+
  facet_wrap(~Common_name, ncol=3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text=element_text(size=4))+
  theme(axis.title=element_text(size=6))+
  theme(strip.text.x = element_text(size = 5))

#ggsave("outputs/graphics/urban_scores_for_figure_5.png", width=7.5, height=3.5, units="cm")

table(natives_plot$`Native/non-native`)

natives_plot %>% group_by(`Native/non-native`) %>% summarize(mean=mean(`LVL2 Median Urban Score`),
                                                             sd=sd(`LVL2 Median Urban Score`))

natives_plot %>% dplyr::filter(`LVL2 Median Urban Score` < 2) %>% group_by(`Native/non-native`) %>% summarize(N=n())


natives_plot %>% dplyr::filter(`LVL2 Median Urban Score` > 10) %>% group_by(`Native/non-native`) %>% summarize(N=n())

# Now do some spatial stuff
# looking at neighborhoods as the spatial unit of interest
# read in towns
towns <- st_read("data/towns_in_cnc_area/towns_in_cnc_area.shp")

# Read in observations joined to towns.
town_obs <- readRDS(file = "data/town_obs.RDS")

# make a histogram of the number of data points from
# all the towns
sample_size <- town_obs %>%
  filter(!is.na(TOWN)) %>% 
  group_by(TOWN) %>%
  summarize(N=n()) %>% 
  arrange(N)

sample_size %>%
  ggplot(., aes(x=N))+
  geom_histogram(color="black", fill="blue")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of observations (log)")+
  ylab("Count")+
  scale_x_log10()

#ggsave("outputs/graphics/number_of_obs_per_town.png", width=7.5, height=5, units="in")

plot_sample_size_dat <- towns %>%
  dplyr::select(TOWN) %>%
  left_join(., sample_size, by="TOWN")

ggplot()+
  geom_sf(data=cnc_area)+
  geom_sf(data=plot_sample_size_dat, aes(fill=N))+
  theme_classic()+
  ggtitle("Number of iNat submissions")

#ggsave("outputs/graphics/number_of_obs_per_town_map.png", width=7.5, height=5, units="in")

ggplot()+
  geom_sf(data=cnc_area)+
  geom_sf(data=plot_sample_size_dat, aes(fill=log10(N)))+
  scale_fill_continuous(labels=comma)+
  theme_classic()+
  ggtitle("Number of iNaturalist submissions")

#ggsave("outputs/graphics/number_of_obs_per_town_map_log10.png", width=7.5, height=5, units="in")

# Now start looking at the relationship between
# the night-time lights within a town
# and the observation sampling within a town
# let's pick 6 towns and then remake the plots for the 'species' example above
example_towns <- c("WALTHAM", "MAYNARD", "PLYMOUTH", "WELLESLEY", "CONCORD", "BOSTON")

# make example plots of the distributions
# for our 6 example cities
town_dat_subsetted <- lights_towns %>%
  dplyr::filter(TOWN %in% example_towns) %>%
  group_by(TOWN) %>%
  mutate(med_lights=median(viirs_values, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.))

ggplot(town_dat_subsetted, aes(x=viirs_values, fill=TOWN))+
  geom_density(color="black")+
  scale_x_log10()+
  facet_wrap(~TOWN)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Dark2")

#ggsave("outputs/graphics/town_light_distribution_examples.png", width=7.5, height=5, units="in")

# get the median lights of all pixels for each town
town_urban_summary <- lights_towns %>%
  group_by(TOWN) %>%
  summarize(median_underlying_score=median(viirs_values, na.rm=TRUE),
            sd_underlying_score=sd(viirs_values, na.rm=TRUE),
            number_pixels=n())

# now to make sure this makes sense
# lets make a map showing the median scores for each town polygon
plot_median_scores_dat <- towns %>%
  dplyr::select(TOWN) %>%
  left_join(., town_urban_summary, by="TOWN")

ggplot()+
  geom_sf(data=cnc_area)+
  geom_sf(data=plot_median_scores_dat, aes(fill=log(median_underlying_score)))+
  theme_classic()+
  ggtitle("Urbanness of towns")

#ggsave("outputs/graphics/map_of_town_urbanness.png", width=7.5, height=5, units="in")

# it appears to make logical sense

# now we want to look at the distribution of the actual observations in a given town/spatial unit
# once again, use the example six towns from above
town_dat_observations <- town_obs %>%
  left_join(., lights_data, by="catalogNumber") %>%
  dplyr::filter(TOWN %in% example_towns) %>%
  group_by(TOWN) %>%
  mutate(med_lights=median(lights, na.rm=TRUE)) %>%
  ungroup()

ggplot(town_dat_observations, aes(x=lights, fill=TOWN))+
  geom_density(color="black")+
  scale_x_log10()+
  facet_wrap(~TOWN)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Dark2")

#ggsave("outputs/graphics/town_observation_distribution_examples.png", width=7.5, height=5, units="in")


town_species_summary <- town_obs %>%
  left_join(., urbanness) %>% 
  group_by(TOWN) %>%
  summarize(species_median_all_obs=median(median_lights),
            sd_all_obs=sd(median_lights),
            number_obs=n(),
            number_species=length(unique(species))) %>%
  left_join(., town_obs %>%
              left_join(., urbanness) %>%
              dplyr::select(TOWN, species, median_lights) %>%
              distinct() %>%
              group_by(TOWN) %>%
              summarize(species_median_distinct=median(median_lights),
                        sd_median_distinct=sd(median_lights)), by="TOWN")

# now summarize the observations within a town, regardless of species
town_observations_summary <- town_obs %>%
  group_by(TOWN) %>%
  left_join(., lights_data, by="catalogNumber") %>%
  summarize(observation_median=median(lights, na.rm=TRUE),
            observation_sd=sd(lights, na.rm=TRUE))

town_observations_summary %>%
  left_join(., town_urban_summary) %>%
  left_join(town_species_summary) %>%
  dplyr::filter(number_obs >=30) %>%
  ggplot(., aes(y=observation_median, x=median_underlying_score))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Opportunistic Observation Index")+
  xlab("Town Underlying Urbanness Index")+
  geom_smooth(method="lm", color="orange")

#ggsave("outputs/graphics/relationship_between_sampling_and_town_urbanization.png", width=7.5, height=5, units="in")

town_observations_summary %>%
  left_join(., town_urban_summary) %>%
  left_join(town_species_summary) %>%
  dplyr::filter(number_obs >=30) %>%
  lm(log10(median_underlying_score) ~ log10(observation_median), data=.) %>%
  summary()

fig_3_plot_dat <- town_observations_summary %>%
  left_join(., town_urban_summary) %>%
  left_join(town_species_summary) %>%
  dplyr::filter(number_obs >=30)

brewer.pal(name="Dark2", n=4)

ggplot()+
  geom_abline(intercept = 0, slope = 1, color="#7570B3", linetype="dashed", size=2)+
  geom_point(data=fig_3_plot_dat, aes(x=median_underlying_score, y=observation_median), color="#1B9E77")+
  geom_point(data=fig_3_plot_dat, aes(x=median_underlying_score, y=species_median_distinct), color="#D95F02")+
  geom_smooth(data=fig_3_plot_dat, aes(x=median_underlying_score, y=observation_median), method="lm", color="#1B9E77")+
  geom_smooth(data=fig_3_plot_dat, aes(x=median_underlying_score, y=species_median_distinct), method="lm", color="#D95F02")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Urbanness distributions")+
  xlab("Town Underlying Urbanness Index")+
  annotate("text", x=16, y=45, label="Opportunistic Observation Index")+
  annotate("text", x=16, y=1, label="Town Biodiversity Urbanness Index")

#ggsave("outputs/graphics/relationship_between_town_and_species_and_sampling.png", width=7.5, height=5, units="in")


## Summarize the top towns
top_towns <- town_observations_summary %>%
  left_join(., town_urban_summary) %>%
  left_join(town_species_summary) %>%
  dplyr::filter(number_obs >=30)

median(top_towns$median_underlying_score, na.rm=TRUE)

top_towns %>% dplyr::filter(complete.cases(TOWN)) %>% dplyr::filter(median_underlying_score <4) %>% nrow(.)

top_towns %>% 
  dplyr::filter(complete.cases(TOWN)) %>% 
  dplyr::filter(median_underlying_score >4 & median_underlying_score <10) %>% 
  nrow(.)

# plot an example of two towns, with the three distributions for each town
town_dat_lights_ex <- town_dat_subsetted %>%
  dplyr::filter(TOWN %in% c("WALTHAM", "CONCORD")) %>%
  dplyr::rename(lights=viirs_values) %>%
  dplyr::select(TOWN, lights, med_lights) %>%
  mutate(distribution="Town Underlying Urbanness Index")

town_dat_obs_ex <- town_obs %>%
  left_join(., lights_data, by="catalogNumber") %>%
  dplyr::filter(TOWN %in% c("WALTHAM", "CONCORD")) %>%
  group_by(TOWN) %>%
  mutate(med_lights=median(lights, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::select(TOWN, lights, med_lights) %>%
  mutate(distribution="Opportunstic Observation Index")

town_dat_species_ex <- town_obs %>%
  left_join(., urbanness) %>%
  dplyr::filter(TOWN %in% c("WALTHAM", "CONCORD")) %>%
  dplyr::select(TOWN, median_lights) %>%
  dplyr::rename(lights=median_lights) %>%
  group_by(TOWN) %>%
  mutate(med_lights=median(lights)) %>%
  ungroup() %>%
  mutate(distribution="Town Biodiversity Urban Index")

bind_rows(town_dat_lights_ex,
          town_dat_obs_ex,
          town_dat_species_ex) %>%
  ggplot(., aes(x=lights, fill=distribution))+
  geom_density(color="black", alpha=0.6)+
  scale_x_log10()+
  facet_wrap(~TOWN, scales="free")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  geom_vline(aes(xintercept=med_lights), colour='red')+
  scale_fill_brewer(palette="Dark2")+
  labs(fill="Distribution:")+
  theme(legend.position="bottom")

#ggsave("outputs/graphics/two_towns_example_of_three_distributions.png", width=8, height=5, units="in")            

# create a large table that can be used for comparisons
summary_dataframe <- town_urban_summary %>%
  left_join(., town_observations_summary) %>%
  left_join(., town_species_summary)

# look at the relationship between the median urban score in a town
# and the underlying urbanization
summary_dataframe %>%
  dplyr::filter(number_obs >= 30) %>%
  ggplot(., aes(y=species_median_distinct, x=median_underlying_score))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Median score of all distinct species")+
  xlab("Median score of pixels in a town")+
  geom_smooth(method="lm", color="orange")+
  geom_abline(intercept = 0, slope = 1, color="blue")

#ggsave("outputs/graphics/species_median_vs_underlying_median.png", width=7.5, height=5, units="in")


summary_dataframe %>%
  dplyr::filter(number_obs >= 30) %>%
  lm(log10(species_median_distinct) ~ log10(median_underlying_score), data=.) %>%
  summary()

resids <- data.frame(residual_urbanness = summary_dataframe %>%
                       dplyr::filter(number_obs >= 30) %>%
                       lm(log10(species_median_distinct) ~ log10(median_underlying_score), data=.) %>%
                       summary() %>%
                       .$resid,
                     TOWN=summary_dataframe %>%
                       dplyr::filter(number_obs >= 30) %>%
                       .$TOWN) %>%
  left_join(., summary_dataframe %>%
              dplyr::select(1,2, 9, 11)) %>%
  rename(`Number of observations`=number_obs) %>%
  rename(`Town urbanness` = species_median_distinct) %>%
  rename(`Town urbanization level`=median_underlying_score) %>%
  rename(`Town ranking`=residual_urbanness)

saveRDS(resids, "data/data_for_residual_figure.RDS")

# plot residual urbanness versus the urbanization of a town
resids %>%
  left_join(., summary_dataframe) %>%
  ggplot(., aes(x=median_underlying_score, y=`Town ranking`))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method="lm")

# mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## read in google earth engine stuff
impervious <- read_csv("data/town_ecological_data/nlcd_impervious_hs.csv") %>%
  dplyr::select(TOWN, mean) %>%
  rename(impervious_mean = mean) %>%
  group_by(TOWN) %>%
  summarize(impervious_mean=mean(impervious_mean, na.rm=TRUE))


globcover <- read_csv("data/town_ecological_data/esa_globcover_hs.csv") %>%
  dplyr::select(TOWN, mode) %>%
  rename(globcover_mode = mode) %>%
  group_by(TOWN) %>%
  summarize(globcover_mode=Mode(globcover_mode))


gls_tree <- read_csv("data/town_ecological_data/gls_tree_hs.csv") %>%
  dplyr::select(TOWN, mean) %>%
  rename(tree_mean = mean) %>%
  group_by(TOWN) %>%
  summarize(tree_mean=mean(tree_mean, na.rm=TRUE))

gls_water <- read_csv("data/town_ecological_data/gls_water_hs.csv") %>%
  dplyr::select(TOWN, area_m2, area_value_m2) %>%
  group_by(TOWN) %>%
  summarize(area_m2=sum(area_m2),
            area_value_m2=sum(area_value_m2)) %>%
  mutate(proportion_water = area_value_m2/area_m2) %>%
  dplyr::select(TOWN, proportion_water)


evi_2013 <- read_csv("data/town_ecological_data/evi_2013_hs.csv") %>%
  mutate(Year=2013) %>%
  dplyr::select(TOWN, mean, Year)
evi_2014 <- read_csv("data/town_ecological_data/evi_2014_hs.csv") %>%
  mutate(Year=2014) %>%
  dplyr::select(TOWN, mean, Year)
evi_2015 <- read_csv("data/town_ecological_data/evi_2015_hs.csv") %>%
  mutate(Year=2015) %>%
  dplyr::select(TOWN, mean, Year)
evi_2016 <- read_csv("data/town_ecological_data/evi_2016_hs.csv") %>%
  mutate(Year=2016) %>%
  dplyr::select(TOWN, mean, Year)
evi_2017 <- read_csv("data/town_ecological_data/evi_2017_hs.csv") %>%
  mutate(Year=2017) %>%
  dplyr::select(TOWN, mean, Year)
evi_2018 <- read_csv("data/town_ecological_data/evi_2018_hs.csv") %>%
  mutate(Year=2018) %>%
  dplyr::select(TOWN, mean, Year)


evi <- bind_rows(evi_2013, evi_2014,
                 evi_2015, evi_2016,
                 evi_2017, evi_2018) %>%
  group_by(TOWN) %>%
  summarise(mean_EVI = mean(mean, na.rm=TRUE))

TOWN_df <- resids %>%
  left_join(., summary_dataframe) %>%
  left_join(., gls_tree) %>%
  left_join(., gls_water) %>%
  left_join(., evi) %>%
  left_join(., globcover) %>%
  left_join(., impervious)

ggplot(TOWN_df, aes(x=tree_mean, y=`Town ranking`))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(TOWN_df, aes(x=proportion_water, y=`Town ranking`))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_log10()


hist(TOWN_df$`Town ranking`)
hist(TOWN_df$species_median_distinct)
hist(log(TOWN_df$species_median_distinct))

# rescale data
model_data <- TOWN_df %>%
  mutate(tree_mean_scaled=scales::rescale(tree_mean)) %>%
  mutate(proportion_water_scaled=scales::rescale(proportion_water)) %>%
  mutate(mean_EVI_scaled=scales::rescale(mean_EVI)) %>%
  mutate(impervious_mean_scaled=scales::rescale(impervious_mean))

mod3 <- lm(species_median_distinct ~ tree_mean + mean_EVI + impervious_mean, data=model_data)
summary(mod3)
check_model(mod3)

mod4 <- lm(log10(species_median_distinct) ~ median_underlying_score + tree_mean  + 
             mean_EVI + impervious_mean, data=model_data)
summary(mod4)
check_model(mod4)

mod5 <- lm(log10(species_median_distinct) ~ median_underlying_score + tree_mean + 
             mean_EVI + impervious_mean, weights=number_obs, data=model_data)
summary(mod5)
check_model(mod5)

brewer.pal(5, name="Dark2")

model_data %>%
  dplyr::select(species_median_distinct, tree_mean, mean_EVI, impervious_mean) %>%
  rename(Trees=tree_mean) %>%
  rename(EVI=mean_EVI) %>%
  rename(`Impervious surface`=impervious_mean) %>%
  pivot_longer(-species_median_distinct, names_to="Variable", values_to="value") %>%
  ggplot(., aes(x=value, y=species_median_distinct, color=Variable))+
  geom_point()+
  facet_wrap(~Variable, scales="free_x")+
  geom_smooth(method="lm", color="black")+
  scale_y_log10()+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Town Biodiversity Urbanness Index")+
  xlab("Value of predictor variable")+
  guides(color=FALSE)

#ggsave("outputs/graphics/model_raw_data_plot.png", width=7, height=5, units="in")

mod5 <- lm(log10(species_median_distinct) ~ median_underlying_score + tree_mean + 
             mean_EVI + impervious_mean, weights=number_obs, data=model_data)
summary(mod5)
