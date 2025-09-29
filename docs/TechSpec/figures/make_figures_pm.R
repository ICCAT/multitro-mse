rm(list = ls())
require(r4ss)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(tidyr)
require(fmsb)
source('sharepoint_path.R')
fig_dir = 'docs/TechSpec/figures'

# Read PM outputs
pm_df = readRDS(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics.rds'))
pm_df_long = pm_df %>% pivot_longer(cols = minB:maxTc, names_to = 'metric')

# number of OM collapsed:
coll_df = pm_df %>% group_by(stock, Ftgt, Btgt) %>% summarise(n_collapsed = sum(collapsed))

# -------------------------------------------------------------------------
# Explore PM values per OM:

p1 = ggplot(data = pm_df_long, aes(x = stock, y = value)) +
  geom_boxplot(aes(fill = collapsed), outlier.size = 0.5) +
  theme(legend.position = 'bottom') +
  facet_wrap(~ metric, scales = 'free_y') 
ggsave(filename = file.path(fig_dir, 'PM_OMs.png'), plot = p1, 
       width = 170, height = 140, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# Radar plot

# Select metrics to be plotted:
plot_data = pm_df %>% filter(metric %in% c('pBmsy', 'Clong', 'Cchange', 'pGreen')) %>%
              group_by(stock, Ftgt, Btgt, metric) %>% summarise(value = median(value), .groups = 'drop')

# Subset for one Ftgt type:
subdat = plot_data %>% filter(Ftgt == '0.8') %>% select(-c(stock, Ftgt)) %>%
  pivot_wider(names_from = metric, values_from = value) 
labels_1 = subdat$Btgt
subdat = subdat %>% select(-Btgt) %>% add_row(Cchange = c(0, 1), Clong = c(0, 40000), 
                                              pBmsy = c(0, 1), pGreen = c(0,1), .before = 1) %>% 
          as.data.frame

colors_border = viridis::viridis(n = length(labels_1)) 


# Plot:
radarchart( subdat, axistype=1, 
            pcol=colors_border , plwd=4, plty=1 ,  #custom polygon
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=1.1, #custom the grid
            vlcex=0.8 #custom labels
)
legend(x=0.85, y=0, legend = labels_1, bty = "n", pch=20 , col=colors_border , 
       text.col = "black", cex=0.9, pt.cex=1.6)
