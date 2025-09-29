rm(list = ls())
require(r4ss)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(sf)
library(stringr)
require(rnaturalearthdata)
require(rnaturalearth)
source('sharepoint_path.R')
fig_dir = 'docs/TechSpec/figures'
dir.create(file.path(fig_dir, 'BET'), showWarnings = FALSE)
dir.create(file.path(fig_dir, 'SKJ'), showWarnings = FALSE)
dir.create(file.path(fig_dir, 'YFT'), showWarnings = FALSE)

# Read SS3 model
betmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/BET/Uncertainty_grid/M20_h0.8_sigmaR0.4"), covar = FALSE)
skjmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/SKJ/ESKJ_SS3_project_detailed/noBuoy_50thGrowth_h0.8"), covar = FALSE)
yftmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/YFT/update/grid/22_ref_case_midM_h08"), covar = FALSE)

# Polygon data for kobe plot
datapoly = data.frame(id = rep(1:4, each = 5), 
                      x = c(1,1,20,20,1,-1,-1,1,1,-1,-1,-1,1,1,-1,1,1,20,20,1),
                      y = c(1,-1,-1,1,1,1,-1,-1,1,1,20,1,1,20,20,20,1,1,20,20))


# -------------------------------------------------------------------------
# Make map regions in SS3
worldmap = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
col_vals = RColorBrewer::brewer.pal(n = 4, name = "Accent")[c(1,4,3,2)]

# R1:
bbox = st_bbox(c(xmin = -100, ymin = 25, xmax = -8, ymax = 42), crs = st_crs(worldmap))
r1 = st_as_sfc(bbox, crs = st_crs(worldmap))
# R2N:
x <- c(-98, -88.2, -84.3, -83.8, 0, 0, -98)
y <- c(17, 14.7, 11.5, 10, 10, 25, 25)
r2n_coords <- data.frame(lon = x, lat = y)
r2n_sf = st_as_sf(r2n_coords, coords = c("lon", "lat"), crs = st_crs(worldmap))
r2n = r2n_sf %>% summarise(do_union  = FALSE) %>% st_cast("POLYGON")
r2n = st_as_sfc(r2n, crs = st_crs(worldmap))
# R2S:
bbox = st_bbox(c(xmin = -65, ymin = -15, xmax = 20, ymax = 10), crs = st_crs(worldmap))
r2s = st_as_sfc(bbox, crs = st_crs(worldmap))
# R3:
bbox = st_bbox(c(xmin = -60, ymin = -38, xmax = 20, ymax = -15), crs = st_crs(worldmap))
r3 = st_as_sfc(bbox, crs = st_crs(worldmap))

# Make map:
p1 = ggplot() + 
  geom_sf(data = r1, fill = col_vals[1], color = col_vals[1]) +
  geom_sf(data = r2n, fill = col_vals[2], color = col_vals[2]) +
  geom_sf(data = r2s, fill = col_vals[3], color = col_vals[3]) +
  geom_sf(data = r3, fill = col_vals[4], color = col_vals[4]) +
  geom_sf(data = worldmap, fill = "black", color = "black") +
  coord_sf(expand = FALSE, xlim = c(-100, 25), ylim = c(-45, 45)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = c(-40, -20, 0, 20, 40)) + 
  scale_x_continuous(breaks = c(-80, -40, 0)) +
  theme_classic() + 
  annotate("text", x = -45, y = 34, label = "R1", size = 5) + 
  annotate("text", x = -45, y = 18, label = "R2n", size = 5) +
  annotate("text", x = -23, y = -1, label = "R2s", size = 5) +
  annotate("text", x = -18, y = -25, label = "R3", size = 5)
ggsave(filename = file.path(fig_dir, 'regions_map.png'), plot = p1, 
       width = 150, height = 110, units = "mm", dpi = 300)

# -------------------------------------------------------------------------
# Data 
SSplotData(replist = betmod, plotdir = file.path(fig_dir, 'BET'), print = TRUE, 
           subplots = 1, maxsize = 0.1, 
           punits = 'mm', pwidth = 150, pheight = 200)
SSplotData(replist = skjmod, plotdir = file.path(fig_dir, 'SKJ'), print = TRUE, 
           subplots = 1, maxsize = 0.1, 
           punits = 'mm', pwidth = 150, pheight = 160)
SSplotData(replist = yftmod, plotdir = file.path(fig_dir, 'YFT'), print = TRUE, 
           subplots = 1, maxsize = 0.1,  
           punits = 'mm', pwidth = 150, pheight = 280)

# -------------------------------------------------------------------------
# Biology

png(filename = file.path(fig_dir, 'BET', 'biology.png'), width = 170, height = 60, res = 300, units = "mm")
par(mfrow = c(1,3))
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = betmod, subplots = c(1), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = betmod, subplots = c(6), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = betmod, subplots = c(21), mainTitle = FALSE)
dev.off()

png(filename = file.path(fig_dir, 'SKJ', 'biology.png'), width = 170, height = 60, res = 300, units = "mm")
par(mfrow = c(1,3))
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = skjmod, subplots = c(1), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = skjmod, subplots = c(6), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = skjmod, subplots = c(21), mainTitle = FALSE)
dev.off()

png(filename = file.path(fig_dir, 'YFT', 'biology.png'), width = 170, height = 60, res = 300, units = "mm")
par(mfrow = c(1,3))
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = yftmod, subplots = c(1), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = yftmod, subplots = c(6), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = yftmod, subplots = c(21), mainTitle = FALSE)
dev.off()

# -------------------------------------------------------------------------
# Plot indices:

plot_data = betmod$cpue
fleet_order = plot_data %>% group_by(Fleet) %>% summarise(Fleet_name = unique(Fleet_name)) %>% arrange(Fleet)
plot_data$Fleet_name = factor(plot_data$Fleet_name, levels = fleet_order$Fleet_name)
p1 = ggplot(data = plot_data, aes(x = Time, y = Obs)) +
  geom_line() + theme_bw() + ylab("CPUE") + xlab(NULL) +
  theme(axis.text = element_text(size = 7.5),
        strip.background = element_blank()) +
  facet_wrap(~ Fleet_name, scales = 'free_y')
ggsave(filename = file.path(fig_dir, 'BET', 'cpue_obs.png'), plot = p1, 
       width = 150, height = 60, units = "mm", dpi = 300)

plot_data = skjmod$cpue
fleet_order = plot_data %>% group_by(Fleet) %>% summarise(Fleet_name = unique(Fleet_name)) %>% arrange(Fleet)
plot_data$Fleet_name = factor(plot_data$Fleet_name, levels = fleet_order$Fleet_name)
p1 = ggplot(data = plot_data, aes(x = Time, y = Obs)) +
  geom_line() + theme_bw() + ylab("CPUE") + xlab(NULL) +
  theme(axis.text = element_text(size = 7.5),
        strip.background = element_blank()) +
  facet_wrap(~ Fleet_name, scales = 'free_y')
ggsave(filename = file.path(fig_dir, 'SKJ', 'cpue_obs.png'), plot = p1, 
       width = 150, height = 60, units = "mm", dpi = 300)

plot_data = yftmod$cpue
fleet_order = plot_data %>% group_by(Fleet) %>% summarise(Fleet_name = unique(Fleet_name)) %>% arrange(Fleet)
plot_data$Fleet_name = factor(plot_data$Fleet_name, levels = fleet_order$Fleet_name)
p1 = ggplot(data = plot_data, aes(x = Time, y = Obs)) +
  geom_line() + theme_bw() + ylab("CPUE") + xlab(NULL) +
  theme(axis.text = element_text(size = 7.5),
        strip.background = element_blank()) +
  facet_wrap(~ Fleet_name, scales = 'free_y')
ggsave(filename = file.path(fig_dir, 'YFT', 'cpue_obs.png'), plot = p1, 
       width = 150, height = 100, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# OEM figure: rho by model and stock
bet_tab = rbind(read.csv(file.path(shrpoint_path, "OEM/BET/Tables/ARpar_Fl4_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '4_PS_FAD_9119'),
                read.csv(file.path(shrpoint_path, "OEM/BET/Tables/ARpar_Fl11_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '11_Japan_LL_TRO')
                )
skj_tab = rbind(read.csv(file.path(shrpoint_path, "OEM/SKJ/Tables/ARpar_Fl4_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '4_PS_FAD_91'),
                read.csv(file.path(shrpoint_path, "OEM/SKJ/Tables/ARpar_Fl9_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '9_BB_North25Lat'),
                read.csv(file.path(shrpoint_path, "OEM/SKJ/Tables/ARpar_Fl11_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '11_Acoustic_Buoy')
                )
yft_tab = rbind(read.csv(file.path(shrpoint_path, "OEM/YFT/Tables/ARpar_Fl3_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '3_PS_ESFR_FS_9122'),
                read.csv(file.path(shrpoint_path, "OEM/YFT/Tables/ARpar_Fl4_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '4_PS_ESFR_FOB_9122'),
                read.csv(file.path(shrpoint_path, "OEM/YFT/Tables/ARpar_Fl11_ALL.csv")) %>% select(nm.sc, rho, lag) %>% mutate(Fleet = '11_Japan_LL_TRO')
                )
plot_dat = rbind(bet_tab %>% mutate(stock = 'Bigeye'),
                skj_tab %>% mutate(stock = 'Skipjack'),
                yft_tab %>% mutate(stock = 'Yellowfin'))
plot_dat = plot_dat %>% na.omit # not sure why there are NAs
# Remove rows for SKJ when some fleets not used:
plot_dat = plot_dat %>% filter(!(str_starts(nm.sc, 'noPS') & Fleet == '4_PS_FAD_91'),
                    !(str_starts(nm.sc, 'noBuoy') & Fleet == '11_Acoustic_Buoy'))

# Make plot:
p1 = ggplot(data = plot_dat, aes(x = Fleet, y = rho, color = stock)) +
  geom_jitter(width = 0.1, size = 1) +
  facet_wrap(~ stock, scales = 'free') +
  theme_bw() + ylab(expression(rho)) + xlab(NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7.5),
        strip.background = element_blank(),
        legend.position = 'none') +
  scale_color_manual(values = c('Bigeye' = '#dca06d', 'Skipjack' = '#a55b4b', 'Yellowfin' = '#4f1c51'))
ggsave(filename = file.path(fig_dir, 'OEM_stock.png'), plot = p1, 
       width = 150, height = 85, units = "mm", dpi = 300)

# -------------------------------------------------------------------------
# Figure HCR example:
p1 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_segment(x = 0, y = 0.1, xend = 0.4, yend = 0.1, lwd = 1, color = 'black') +
  geom_segment(x = 0.4, y = 0.1, xend = 0.8, yend = 0.8, lwd = 1, color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 3, yend = 0.8, lwd = 1, color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 0.8, yend = 0, linetype = 'dashed', color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 0, yend = 0.8, linetype = 'dashed', color = 'black') +
  theme_bw() +
  ylab(NULL) + xlab(NULL) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0.4, 0.8, 1), labels = c(expression(B[lim]), expression(B[thr]), expression(B[msy]))) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0.1, 0.8, 1), labels = c(expression(F[min]), expression(F[tgt]), expression(F[msy]))) +
  theme(legend.direction="horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900'))
ggsave(filename = file.path(fig_dir, 'hcr_example.png'), plot = p1, 
       width = 80, height = 70, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# Figure HCR by case:
f_vec = c(0.8, 1, 1.2)
b_vec = c(0.8, 1, 1.2)
mydat = as.data.frame(expand.grid(ffmsy = f_vec, bbmsy = b_vec))
mydat = mydat %>% mutate(bbmsy_t = factor(bbmsy, levels = b_vec, 
                                        labels = c(expression("0.8"*B[msy]),
                                                   expression(B[msy]),
                                                   expression("1.2"*B[msy]))),
                         ffmsy_t = factor(ffmsy, levels = f_vec, 
                                        labels = c(expression("0.8"*F[msy]),
                                                   expression(F[msy]),
                                                   expression("1.2"*F[msy]))))

p2 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_segment(x = 0, y = 0.1, xend = 0.4, yend = 0.1, color = 'black') +
  geom_segment(data = mydat, aes(x = 0.4, y = 0.1, xend = bbmsy, yend = ffmsy), color = 'black') +
  geom_segment(data = mydat, aes(x = bbmsy, y = ffmsy, xend = 3, yend = ffmsy), color = 'black') +
  theme_bw() +
  ylab(NULL) + xlab(NULL) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0.4, 1), labels = c(expression(B[lim]), expression(B[msy]))) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0.1, 1), labels = c(expression(F[min]), expression(F[msy]))) +
  theme(legend.direction="horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
		axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  facet_grid(ffmsy_t ~ bbmsy_t, labeller = label_parsed )
ggsave(filename = file.path(fig_dir, 'hcr_combs.png'), plot = p2, 
       width = 170, height = 170, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# Figure for performance metrics example:

# Simulate time series:
n_ts = 30 # number of years to simulate
x = seq(0, 10, length.out = 100)
y = 2 * sin(3 * pi * 0.5 * x + pi / 4) + 1.5 * sin(2 * pi * 0.3 * x + pi / 6) # For SSB
plot(x[1:n_ts], y[1:n_ts], type = 'b')
y1 = -2 * sin(3 * pi * 0.5 * x + pi / 4) + 1.5 * sin(2 * pi * 0.5 * x + pi / 6) # For SSB
# plot(x[1:n_ts], y1[1:n_ts], type = 'b')
# plot(y[1:n_ts], y1[1:n_ts], type = 'b')
# abline(h = 1, v = -1)

# Define ref points:
Blim = 0.4
Bmsy = 1
Fmsy = 1
# Create a data frame:
exvec = data.frame(Bio_all = y[1:n_ts], Fval = y1[1:n_ts])
# Scale between min max:
df = preProcess(exvec, method = 'range', rangeBounds = c(0.25, 1.75))
exvec = predict(df, as.data.frame(exvec))
# Create relevant columns:
exvec = exvec %>% mutate(Yr_sim = 1:n_ts, 
                         red = if_else(Bio_all < Bmsy & Fval > Fmsy, '0', '1'),
                         green = if_else(Bio_all >= Bmsy & Fval <= Fmsy, '0', '1'))
exvec = exvec %>% mutate(pblim = if_else(Bio_all < Blim, '0', '1'))
exvec = exvec %>% mutate(pbmsy = if_else(Bio_all < Bmsy & Bio_all > Blim, '0', '1'))
# Define x axis label:
x_lab = 'Simulation years'

# Status
p1 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point() +
  geom_point(data = exvec %>% filter(min(Bio_all) == Bio_all), 
             aes(x = Yr_sim, y = Bio_all), color = 'blue', size = 2) +
  theme_bw() + ylab(expression(B/B[msy])) + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "minB", 
           size = 3, color = 'blue')

p2 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = mean(exvec$Bio_all), color = 'blue') +
    theme_bw() + ylab(expression(B/B[msy])) + xlab(x_lab) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    annotate("text", x = 15, y = 1.8, label = "meanB", 
           size = 3, color = 'blue')

p3 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = mean(exvec$Fval), color = 'blue') +
  theme_bw() + ylab(expression(F/F[msy])) + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "meanF", 
           size = 3, color = 'blue')

p4 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_path(data = exvec, aes(x = Bio_all, y = Fval), color = 'gray60') +
  geom_point(data = exvec, aes(x = Bio_all, y = Fval, color = green)) +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() +
  ylab(expression("F/"*F[msy])) + xlab(expression("B/"*B[msy])) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) +
  theme(legend.position="none",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  annotate("text", x = 1, y = 1.9, label = "pGreen",  size = 3, color = 'blue')

p5 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_path(data = exvec, aes(x = Bio_all, y = Fval), color = 'gray60') +
  geom_point(data = exvec, aes(x = Bio_all, y = Fval, color = red)) +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() +
  ylab(expression("F/"*F[msy])) + xlab(expression("B/"*B[msy])) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) +
  theme(legend.position="none",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  annotate("text", x = 1, y = 1.9, label = "pRed",  size = 3, color = 'blue')

# Merge status plots:
merged_plot = grid.arrange(p1, p2, p3, p4, p5, ncol = 3)
ggsave(filename = file.path(fig_dir, 'status_ex.png'), plot = merged_plot, 
       width = 170, height = 120, units = "mm", dpi = 300)

# Safety:
p6 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point(aes(color = pblim)) +
  geom_hline(yintercept = Blim, linetype = 'dashed', color = 'gray60') +
  scale_color_manual(values = c('0' = 'gray40', '1' = 'blue')) +
  theme_bw() + ylab('Spawning biomass (t)') + xlab(x_lab) +
  scale_y_continuous(breaks = Blim, labels = expression(B[lim])) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "pBlim", 
           size = 3, color = 'blue')
 
p7 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point(aes(color = pbmsy)) +
  geom_hline(yintercept = Blim, linetype = 'dashed', color = 'gray60') +
  geom_hline(yintercept = Bmsy, linetype = 'dashed', color = 'gray60') +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() + ylab('Spawning biomass (t)') + xlab(x_lab) +
  scale_y_continuous(breaks = c(Blim, Bmsy), labels = c(expression(B[lim]), expression(B[msy]))) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "pBmsy", 
           size = 3, color = 'blue')

# Merge safety plots:
merged_plot = grid.arrange(p6, p7, ncol = 2)
ggsave(filename = file.path(fig_dir, 'safety_ex.png'), plot = merged_plot, 
       width = 140, height = 60, units = "mm", dpi = 300)


# Yield:
# add noise to catch:
p8 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_line() + geom_point() +
  geom_segment(x = 1, xend = 3, y = mean(exvec$Fval[1:3]), yend = mean(exvec$Fval[1:3]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "Csht", 
           size = 3, color = 'blue', parse = TRUE)

p9 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_line() + geom_point() +
  geom_segment(x = 5, xend = 10, y = mean(exvec$Fval[5:10]), yend = mean(exvec$Fval[5:10]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "Cmed", 
           size = 3, color = 'blue', parse = TRUE)

p10 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_line() + geom_point() +
  geom_segment(x = 15, xend = n_ts, y = mean(exvec$Fval[15:n_ts]), yend = mean(exvec$Fval[15:n_ts]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "Clon", 
           size = 3, color = 'blue', parse = TRUE)

# Merge yield plots:
merged_plot = grid.arrange(p8, p9, p10, ncol = 3)
ggsave(filename = file.path(fig_dir, 'yield_ex.png'), plot = merged_plot, 
       width = 170, height = 60, units = "mm", dpi = 300)


# Stability
plot_df = exvec %>% select(Yr_sim, Fval)
plot_df$Yr_sim_end = NA
plot_df$Fval_end = NA
plot_df$Yr_sim_end[1:(nrow(plot_df)-1)] = plot_df$Yr_sim[2:nrow(plot_df)]
plot_df$Fval_end[1:(nrow(plot_df)-1)] = plot_df$Fval[2:nrow(plot_df)]
plot_df2 = plot_df %>% na.omit

p11 = ggplot(data = plot_df, aes(x = Yr_sim, y = Fval)) +
  geom_point() +
  geom_segment(data = plot_df2, aes(x = Yr_sim, xend = Yr_sim_end, y = Fval, yend = Fval_end), 
               arrow = arrow(length = unit(0.15, "cm")), color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "Cc", 
           size = 3, color = 'blue', parse = T)

p12 = ggplot(data = plot_df, aes(x = Yr_sim, y = Fval)) +
  geom_point() + geom_line() +
  geom_segment(data = plot_df, aes(x = Yr_sim, xend = Yr_sim, y = mean(Fval), yend = Fval), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  geom_hline(yintercept = mean(plot_df$Fval), color = 'black') +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "Csd", 
           size = 3, color = 'blue', parse = TRUE)

plot_df3 = plot_df
plot_df3$Fval[c(14:16)] = 0
plot_df3 = plot_df3 %>% mutate(tac = if_else(Fval == 0, '0', '1'))
p13 = ggplot(data = plot_df3, aes(x = Yr_sim, y = Fval)) +
  geom_point(aes(color = tac)) + 
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_y_continuous(breaks = 0) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "pShw", 
           size = 3, color = 'blue')

plot_df4 = exvec %>% select(Yr_sim, Fval)
tac_period = 1
plot_df4 = plot_df4[seq(from = 1, by = tac_period, length.out = nrow(plot_df4)/tac_period), ]
plot_df4$Yr_sim_end = NA
plot_df4$Fval_end = NA
plot_df4$Yr_sim_end[1:(nrow(plot_df4)-1)] = plot_df4$Yr_sim[2:nrow(plot_df4)]
plot_df4$Fval_end[1:(nrow(plot_df4)-1)] = plot_df4$Fval[2:nrow(plot_df4)]
plot_df4 = plot_df4 %>% na.omit
plot_df4$diff = abs(plot_df4$Fval_end - plot_df4$Fval)
plot_df4 = plot_df4 %>% mutate(diff_type = if_else(diff > 0.3, '1', '0'))
p14 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_point() + 
  geom_segment(data = plot_df4, 
               aes(x = Yr_sim, xend = Yr_sim_end, 
                   y = Fval, yend = Fval_end,
                   color = diff_type), 
               arrow = arrow(length = unit(0.15, "cm"))) +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_color_manual(values = c('0' = 'gray40', '1' = 'blue')) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position = "none") +
  annotate("text", x = 15, y = 1.8, label = "pTX", 
           size = 3, color = 'blue')

p15 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_point() + 
  geom_segment(data = plot_df4 %>% filter(which.max(diff) == row_number()), 
               aes(x = Yr_sim, xend = Yr_sim_end, y = Fval, yend = Fval_end), 
               arrow = arrow(length = unit(0.15, "cm")), color = 'blue') +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_x_continuous(breaks = seq(10, 30, 10)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "maxTc", 
           size = 3, color = 'blue', parse = T)

# Merge stability plots:
merged_plot = grid.arrange(p11, p12, p13, p14, p15, ncol = 3)
ggsave(filename = file.path(fig_dir, 'stability_ex.png'), plot = merged_plot, 
       width = 170, height = 120, units = "mm", dpi = 300)
