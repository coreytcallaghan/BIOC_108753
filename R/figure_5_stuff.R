library(ggplot2)
library(patchwork)

adapter <- ggplot(data = data.frame(x = c(-3, 3)), aes(x))+
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color="blue", size=1.2)+ 
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("")+
  theme(plot.title = element_text(size = 10))



avoider <- ggplot(data = data.frame(x = c(0, 20)), aes(x))+
  stat_function(fun = dexp, n = 101, args = list(rate=0.3), color="blue", size=1.2)+ 
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("")+
  theme(plot.title = element_text(size = 10))



exploiter <- ggplot(data = data.frame(x = c(0, 4)), aes(x))+
  stat_function(fun = exp, n = 101, color="blue", size=1.2)+ 
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("")+
  theme(plot.title = element_text(size = 10))


avoider + adapter + exploiter + plot_layout(ncol=3)


ggsave("outputs/graphics/theoretical_figure_5.png", width=7.5, height=3.5, units="cm")
