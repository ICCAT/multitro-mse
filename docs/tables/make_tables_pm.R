rm(list = ls())
require(dplyr)
require(tidyr)
source('sharepoint_path.R')
tbl_dir = 'docs/TechSpec/tables'

# Read PM outputs
pm_df = readRDS(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics.rds'))
# Remove collapsed OMs?
# pm_df = pm_df %>% filter(!collapsed)

# Make table:
tbl_data = pm_df %>% group_by(stock, Ftgt, Btgt) %>% summarise_at(vars(minB:maxTc), median)

# Table for BET:
out_tbl = tbl_data %>% filter(stock == 'BET') 
saveRDS(out_tbl, file.path(tbl_dir, 'PM_summary_bet.rds'))
# Table for SKJ:
out_tbl = tbl_data %>% filter(stock == 'SKJ')
saveRDS(out_tbl, file.path(tbl_dir, 'PM_summary_skj.rds'))
# Table for YFT:
out_tbl = tbl_data %>% filter(stock == 'YFT') 
saveRDS(out_tbl, file.path(tbl_dir, 'PM_summary_yft.rds'))
