require(r4ss)
require(dplyr)
source('sharepoint_path.R')

# BET models:
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/BET/Uncertainty_grid")
mod_vec = list.files(grid_folder)
bet_df = data.frame(Stock = 'Bigeye', Model = mod_vec)

# SKJ models:
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/SKJ/ESKJ_SS3_project_detailed")
mod_vec = list.files(grid_folder)
skj_df = data.frame(Stock = 'Skipjack', Model = mod_vec)

# yft models:
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/YFT/update/grid")
mod_vec = list.files(grid_folder)[-1]
yft_df = data.frame(Stock = 'Yellowfin', Model = mod_vec)

# Combine all models:
save_mod = bind_rows(list(bet_df, skj_df, yft_df))
write.csv(save_mod, 'docs/TechSpec/tables/selected_OM.csv', row.names = FALSE)
