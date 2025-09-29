rm(list = ls())
require(dplyr)
require(r4ss)
source('sharepoint_path.R')
tbl_dir = 'docs/TechSpec/tables'
dir.create(file.path(tbl_dir, 'BET'), showWarnings = FALSE)
dir.create(file.path(tbl_dir, 'SKJ'), showWarnings = FALSE)
dir.create(file.path(tbl_dir, 'YFT'), showWarnings = FALSE)

# Read SS3 model
betmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/BET/Uncertainty_grid/M20_h0.8_sigmaR0.4"), covar = FALSE)
skjmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/SKJ/ESKJ_SS3_project_detailed/noBuoy_50thGrowth_h0.8"), covar = FALSE)
yftmod = SS_output(file.path(shrpoint_path, "SS3_outputs/Grids3Species/YFT/update/grid/22_ref_case_midM_h08"), covar = FALSE)

# -------------------------------------------------------------------------
# Fleet table

# BET
mytab = data.frame(Fleet = betmod$FleetNames)
mytab$CPUE = 'No'
mytab$CPUE[sort(unique(betmod$cpue$Fleet))] = 'Yes'
mytab = mytab %>% mutate(Description = c("Purse seine (all fleets except Ghana, USA, and Venezuela)",
                                         "Purse seine (all fleets except Ghana, USA, and Venezuela)",
                                         "Purse seine free school (all fleets except Ghana, USA, and Venezuela)",
                                         "Purse seine FADs (all fleets except Ghana, USA, and Venezuela)",
                                         "Ghana baitboat and purse seine",
                                         "Baitboat South Dakar (all fleets except Ghana)",
                                         "Baitboat North Dakar (all fleets except Ghana)",
                                         "Baitboat North Dakar (all fleets except Ghana)",
                                         "Baitboat North Azores (all fleets except Ghana)",
                                         "Longline North Japan",
                                         "Longline Tropical Japan",
                                         "Longline South Japan",
                                         "Longline North Other (all fleets except Japan and Chinese Taipei)",
                                         "Longline Tropical Other (all fleets except Japan and Chinese Taipei)",
                                         "Longline South Other (all fleets except Japan and Chinese Taipei)",
                                         "Longline North Chinese Taipei",
                                         "Longline Tropical Chinese Taipei",
                                         "Longline South Chinese Taipei",
                                         "RR West Atlantic USA, Canada, UK-Sta Helena",
                                         "Handline Brazil",
                                         "Purse seine West Atlantic (USA, Venezuela)",
                                         "Other"
                                         ),
                         Region = c("R2/R1", "R2/R1", "R2/R1", "R2/R3", "R2", "R2s", "R2n", "R2n",
                                  "R1", "R1", "R2", "R3", "R1", "R2", "R3", "R1", "R2", "R3",
                                  "R1", "R2", "R1", "R1/R2/R3"), 
                         .before = 'CPUE')
saveRDS(mytab, file.path(tbl_dir, 'BET', 'FleetInfo.rds'))

# SKJ
mytab = data.frame(Fleet = skjmod$FleetNames)
mytab$CPUE = 'No'
mytab$CPUE[sort(unique(skjmod$cpue$Fleet))] = 'Yes'
mytab = mytab %>% mutate(Description = c("Purse seine EU (Spain, France)",
                                         "Purse seine EU (Spain, France)",
                                         "Purse seine free school",
                                         "Purse seine FADs",
                                         "Ghana baitboat and purse seine",
                                         "Baitboat South Dakar",
                                         "Baitboat Dakar",
                                         "Baitboat Dakar",
                                         "Baitboat North25",
                                         "All longline fleets",
                                         "Acoustic buoy index"
                                          ),
                         Region = c("East R1/R2/R3", "East R1/R2/R3", "East R1/R2/R3", "East R1/R2/R3", "East R1/R2/R3", 
                                   "East R2s/R3", "East R2n", "East R2n", "East R1", "East R1/R2/R3", "East R1/R2/R3"), 
                          .before = 'CPUE')
saveRDS(mytab, file.path(tbl_dir, 'SKJ', 'FleetInfo.rds'))

# YFT
mytab = data.frame(Fleet = yftmod$FleetNames)
mytab$CPUE = 'No'
mytab$CPUE[sort(unique(yftmod$cpue$Fleet))] = 'Yes'
mytab = mytab %>% mutate(Description = c("Purse seine EU (Spain, France)",
                                         "Purse seine EU (Spain, France)",
                                         "Purse seine free school EU (Spain, France)",
                                         "Purse seine FADs EU (Spain, France)",
                                         "Ghana baitboat and purse seine",
                                         "Baitboat South Dakar",
                                         "Baitboat North Dakar",
                                         "Baitboat North Dakar",
                                         "Baitboat North Azores",
                                         "Longline North Japan",
                                         "Longline Tropical Japan",
                                         "Longline South Japan",
                                         "Longline North Other",
                                         "Longline Tropical Other",
                                         "Longline South Other",
                                         "Handline Brazil",
                                         "RR West Atlantic",
                                         "Purse seine West Atlantic",
                                         "Other"
                                        ),
                                        Region = c("R2/R1", "R2/R1", "R2/R1", "R2/R3", "R2", "R2s", "R2n", "R2n",
                                                 "R1", "R1", "R2", "R3", "R1", "R2", "R3", 
                                                 "R2", "R1", "R1", "R1/R2/R3"), 
                                        .before = 'CPUE')
saveRDS(mytab, file.path(tbl_dir, 'YFT', 'FleetInfo.rds'))


# -------------------------------------------------------------------------
# Table OEM rho residuals:

load(file.path(shrpoint_path, 'OEM/InputMSE_OEM/ProjRes/Rand_And_ResidualsAR_Fl1_BaseCase.RData'))
these_cpue = sort(unique(ss3$cpue$Fleet))
save_df = list()
for(i in seq_along(these_cpue)) {
  err_df = ss3$cpue %>% filter(Fleet == these_cpue[i])
  err_vec = err_df %>% pull(Dev)
  fleet_name = unique(err_df$Fleet_name)
  ac_check = acf(err_vec, plot = FALSE)
  significance_level = qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(err_vec)))
  tmp_df = data.frame(Fleet = fleet_name, rho = ac_check$acf[,,1][2], 
                      sigma = sd(err_vec), lag = 1,
                      sig_level = significance_level)
  tmp_df = tmp_df %>% mutate(sig_acf = if_else(abs(rho) > sig_level, 'Yes', 'No'))
  save_df[[i]] = tmp_df
}
save_df = bind_rows(save_df)
save_df$rho = round(save_df$rho, 2)
save_df$sigma = round(save_df$sigma, 2)
saveRDS(save_df, file.path(tbl_dir, 'OEM_residuals.rds'))
