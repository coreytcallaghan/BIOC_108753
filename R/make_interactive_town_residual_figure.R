# make an ineractive figure with the residuals of each town 'ranking' them

library(plotly)
library(htmlwidgets)
library(ggplot2)
library(dplyr)

town_residuals <- readRDS("data/data_for_residual_figure.RDS") %>%
  mutate(Rank=dense_rank(desc(`Town ranking`))) %>%
  mutate(Status=ifelse(`Town ranking` < 0, "Negative", "Positive")) %>%
  rename(`Residual score` = `Town ranking`) %>%
  rename(Town=TOWN) %>%
  mutate(`Residual score` = round(`Residual score`, digits=3))


fig <- ggplot()+
  geom_vline(xintercept=0, linetype="dashed", color="blue")+
  geom_point(data=town_residuals, aes(x=`Residual score`, y=Rank, color=Status, 
                                      label=Town, label2=`Number of observations`))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Under or over performance")+
  ylab("Town ranking")+
  scale_color_brewer(palette="Set1")+
  ggtitle("Relative ranking of towns in the Boston CNC area")+
  xlim(-0.4, 0.4)

plot_bits <- ggplot_build(fig)$town_residuals

fig_p <- ggplotly(fig, width = 900, height = 850, tooltip=c("y", "x", "label", "label2"))

fig_p2 <- hide_legend(fig_p)

setwd("town_rankings")
saveWidget(widget = fig_p2, file = "town_urbanness.html")
setwd("..")
