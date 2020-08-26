# make figure 6

library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(patchwork)


# read in sf of cnc_area
cnc_area <- readRDS("data/cnc_area.rds")

towns <- st_read("data/towns_in_cnc_area/towns_in_cnc_area.shp")

town_residuals <- readRDS("data/data_for_residual_figure.RDS") %>%
  mutate(Rank=dense_rank(`Town ranking`)) %>%
  mutate(Status=ifelse(`Town ranking` < 0, "Negative", "Positive")) %>%
  rename(`Residual score` = `Town ranking`) %>%
  rename(Town=TOWN) %>%
  mutate(`Residual score` = round(`Residual score`, digits=3))


fig <- ggplot()+
  geom_vline(xintercept=0, linetype="dashed", color="blue")+
  geom_point(data=town_residuals, aes(x=`Residual score`, y=Rank, color=`Residual score`, 
                                      label=Town, label2=`Number of observations`))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Residual score")+
  ylab("Town ranking")+
  scale_color_gradient(low="red", high="yellow", name="Town performance: ", 
                       breaks=c(-0.3, 0, 0.3), labels=c("Over", "", "Under"))+
  ggtitle("Relative ranking of towns in the Boston CNC area")+
  xlim(-0.4, 0.4)+
  theme(legend.position="bottom")

plot_bits <- ggplot_build(fig)$town_residuals

fig_p <- ggplotly(fig, width = 900, height = 850, tooltip=c("y", "x", "label", "label2"))

fig_p2 <- hide_legend(fig_p)

setwd("docs")
saveWidget(widget = fig_p2, file = "town_urbanness.html")
setwd("..")


plot_dat <- towns %>%
  rename(Town=TOWN) %>%
  left_join(., town_residuals)

map_figure <- ggplot()+
  geom_sf(data=cnc_area)+
  geom_sf(data=plot_dat, aes(fill=`Residual score`))+
  theme_classic()+
  scale_fill_gradient(low="red", high="yellow", 
                      name="Town performance: ", breaks=c(-0.3, 0, 0.3), 
                      labels=c("Over", "", "Under"))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(color="black"))+
  theme(panel.border=element_rect(color="black", fill="transparent"))

fig + map_figure + plot_layout(ncol=2)

#ggsave("final_figures_for_paper/Figure_6.png")
