rm(list = ls())
require(r4ss)
require(dplyr)
require(tidyr)
require(stringr)
source('sharepoint_path.R')
dat_dir = 'docs/TechSpec/data'
dir.create(file.path(dat_dir, 'BET'), showWarnings = FALSE)
dir.create(file.path(dat_dir, 'SKJ'), showWarnings = FALSE)
dir.create(file.path(dat_dir, 'YFT'), showWarnings = FALSE)


# -------------------------------------------------------------------------
# BET
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/BET/Uncertainty_grid")
mod_vec = list.files(grid_folder)
save_par = list()
save_bio = list()
save_dq = list()
save_ts = list()
save_kobe = list()
for(j in 1:length(mod_vec)) {
    this_report = SS_output(file.path(grid_folder, mod_vec[j]), 
                            covar = FALSE, verbose = FALSE, printstats = FALSE, hidewarn = TRUE)
    # Parameters:
    par_est = this_report$parameters
    par_df = data.frame(LAmin = par_est['L_at_Amin_Fem_GP_1', 'Value'],
                        LAmax = par_est['L_at_Amax_Fem_GP_1', 'Value'],
                        K = par_est['VonBert_K_Fem_GP_1', 'Value'],
                        gamma = par_est['Richards_Fem_GP_1', 'Value'],
                        CV1 = par_est['CV_young_Fem_GP_1', 'Value'],
                        CVA = par_est['CV_old_Fem_GP_1', 'Value'],
                        R0 = par_est['SR_LN(R0)', 'Value'],
                        h = par_est['SR_BH_steep', 'Value'],
                        sigmaR = par_est['SR_sigmaR', 'Value'])
    par_df$mod_type = mod_vec[j]
    save_par[[j]] = par_df
    # Biology:
    bio_df = this_report$endgrowth %>% filter(Morph == 1) %>% select(Seas, Age_Beg, M, Len_Beg) 
    bio_df$mod_type = mod_vec[j]
    save_bio[[j]] = bio_df
    # Derived quantities:
    quant_est = this_report$derived_quants
    quant_df = quant_est %>% filter(Label %in% c('SSB_Virgin', "SSB_MSY", "annF_MSY", "Dead_Catch_MSY",
                                      paste0("SSB_", this_report$endyr),
                                      paste0("F_", this_report$endyr))) %>%
                select(Label, Value)
    quant_df$mod_type = mod_vec[j]
    # Do this because F is reported as F/Fmsy:
    quant_df$Value[quant_df$Label == paste0("F_", this_report$endyr)] = quant_df$Value[quant_df$Label == paste0("F_", this_report$endyr)] * quant_df$Value[quant_df$Label == "annF_MSY"]
    save_dq[[j]] = quant_df
    # Time series:
    ts_df = quant_est %>% filter(Label %in% c(paste0("SSB_", this_report$startyr:this_report$endyr),
                                      paste0("F_", this_report$startyr:this_report$endyr))) %>%
      select(Label, Value) %>% mutate(year = as.numeric(str_split_i(Label, "_", 2)),
                                      var = str_split_i(Label, "_", 1))
    ts_df$mod_type = mod_vec[j]
    # Do this because F is reported as F/Fmsy:
    ts_df$Value[ts_df$var == "F"] = ts_df$Value[ts_df$var == "F"] * quant_df$Value[quant_df$Label == "annF_MSY"]
    save_ts[[j]] = ts_df
    # Kobe:
    kobe_df = ts_df
    kobe_df$Value[kobe_df$var == 'SSB'] = kobe_df$Value[kobe_df$var == 'SSB'] / quant_est$Value[quant_est$Label == 'SSB_MSY']
    kobe_df$Value[kobe_df$var == 'F'] = kobe_df$Value[kobe_df$var == 'F'] / quant_est$Value[quant_est$Label == 'annF_MSY']
    kobe_df$mod_type = mod_vec[j]
    save_kobe[[j]] = kobe_df
    cat(paste0("Model set ", j, " of ", length(mod_vec), " completed.\n"))
}

# Merge:
save_par = bind_rows(save_par)
save_bio = bind_rows(save_bio)
save_dq = bind_rows(save_dq)
save_ts = bind_rows(save_ts)
save_kobe = bind_rows(save_kobe)

# Save:
saveRDS(save_par, file = file.path(dat_dir, 'BET', 'summ_par.rds'))
saveRDS(save_bio, file = file.path(dat_dir, 'BET', 'summ_bio.rds'))
saveRDS(save_dq, file = file.path(dat_dir, 'BET', 'summ_dq.rds'))
saveRDS(save_ts, file = file.path(dat_dir, 'BET', 'summ_ts.rds'))
saveRDS(save_kobe, file = file.path(dat_dir, 'BET', 'summ_kobe.rds'))


# -------------------------------------------------------------------------
# SKJ
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/SKJ/ESKJ_SS3_project_detailed")
mod_vec = list.files(grid_folder)
save_par = list()
save_bio = list()
save_dq = list()
save_ts = list()
save_kobe = list()
for(j in 1:length(mod_vec)) {
  this_report = SS_output(file.path(grid_folder, mod_vec[j]), 
                          covar = FALSE, verbose = FALSE, printstats = FALSE, hidewarn = TRUE)
  # Parameters:
  par_est = this_report$parameters
  par_df = data.frame(M = par_est['NatM_Lorenzen_Fem_GP_1', 'Value'],
                      LAmin = par_est['L_at_Amin_Fem_GP_1', 'Value'],
                      LAmax = par_est['L_at_Amax_Fem_GP_1', 'Value'],
                      K = par_est['VonBert_K_Fem_GP_1', 'Value'],
                      CV1 = par_est['CV_young_Fem_GP_1', 'Value'],
                      CVA = par_est['CV_old_Fem_GP_1', 'Value'],
                      R0 = par_est['SR_LN(R0)', 'Value'],
                      h = par_est['SR_BH_steep', 'Value'],
                      sigmaR = par_est['SR_sigmaR', 'Value'])
  par_df$mod_type = mod_vec[j]
  save_par[[j]] = par_df
  # Biology:
  bio_df = this_report$endgrowth %>% filter(Morph == 1) %>% select(Seas, Age_Beg, M, Len_Beg) 
  bio_df$mod_type = mod_vec[j]
  save_bio[[j]] = bio_df
  # Derived quantities:
  quant_est = this_report$derived_quants
  quant_df = quant_est %>% filter(Label %in% c('SSB_Virgin', "SSB_MSY", "annF_MSY", "Dead_Catch_MSY",
                                               paste0("SSB_", this_report$endyr),
                                               paste0("F_", this_report$endyr))) %>%
    select(Label, Value)
  quant_df$mod_type = mod_vec[j]
  # Do this because F is reported as F/Fmsy:
  quant_df$Value[quant_df$Label == paste0("F_", this_report$endyr)] = quant_df$Value[quant_df$Label == paste0("F_", this_report$endyr)] * quant_df$Value[quant_df$Label == "annF_MSY"]
  save_dq[[j]] = quant_df
  # Time series:
  ts_df = quant_est %>% filter(Label %in% c(paste0("SSB_", this_report$startyr:this_report$endyr),
                                            paste0("F_", this_report$startyr:this_report$endyr))) %>%
    select(Label, Value) %>% mutate(year = as.numeric(str_split_i(Label, "_", 2)),
                                    var = str_split_i(Label, "_", 1))
  ts_df$mod_type = mod_vec[j]
  # Do this because F is reported as F/Fmsy:
  ts_df$Value[ts_df$var == "F"] = ts_df$Value[ts_df$var == "F"] * quant_df$Value[quant_df$Label == "annF_MSY"]
  save_ts[[j]] = ts_df
  # Kobe:
  kobe_df = ts_df
  kobe_df$Value[kobe_df$var == 'SSB'] = kobe_df$Value[kobe_df$var == 'SSB'] / quant_est$Value[quant_est$Label == 'SSB_MSY']
  kobe_df$Value[kobe_df$var == 'F'] = kobe_df$Value[kobe_df$var == 'F'] / quant_est$Value[quant_est$Label == 'annF_MSY']
  kobe_df$mod_type = mod_vec[j]
  save_kobe[[j]] = kobe_df
  cat(paste0("Model set ", j, " of ", length(mod_vec), " completed.\n"))
}

# Merge:
save_par = bind_rows(save_par)
save_bio = bind_rows(save_bio)
save_dq = bind_rows(save_dq)
save_ts = bind_rows(save_ts)
save_kobe = bind_rows(save_kobe)

# Save:
saveRDS(save_par, file = file.path(dat_dir, 'SKJ', 'summ_par.rds'))
saveRDS(save_bio, file = file.path(dat_dir, 'SKJ', 'summ_bio.rds'))
saveRDS(save_dq, file = file.path(dat_dir, 'SKJ', 'summ_dq.rds'))
saveRDS(save_ts, file = file.path(dat_dir, 'SKJ', 'summ_ts.rds'))
saveRDS(save_kobe, file = file.path(dat_dir, 'SKJ', 'summ_kobe.rds'))

# -------------------------------------------------------------------------
# YFT
grid_folder = file.path(shrpoint_path, "SS3_outputs/Grids3Species/YFT/update/grid")
mod_vec = list.files(grid_folder)[-1] # remove ref case
save_par = list()
save_bio = list()
save_dq = list()
save_ts = list()
save_kobe = list()
for(j in 1:length(mod_vec)) {
  this_report = SS_output(file.path(grid_folder, mod_vec[j]), 
                          covar = FALSE, verbose = FALSE, printstats = FALSE, hidewarn = TRUE)
  # Parameters:
  par_est = this_report$parameters
  par_df = data.frame(M = par_est['NatM_Lorenzen_Fem_GP_1', 'Value'],
                      LAmin = par_est['L_at_Amin_Fem_GP_1', 'Value'],
                      LAmax = par_est['L_at_Amax_Fem_GP_1', 'Value'],
                      K = par_est['VonBert_K_Fem_GP_1', 'Value'],
                      gamma = par_est['Richards_Fem_GP_1', 'Value'],
                      CV1 = par_est['CV_young_Fem_GP_1', 'Value'],
                      CVA = par_est['CV_old_Fem_GP_1', 'Value'],
                      R0 = par_est['SR_LN(R0)', 'Value'],
                      h = par_est['SR_BH_flat_steep', 'Value'],
                      sigmaR = par_est['SR_sigmaR', 'Value'])
  par_df$mod_type = sub(pattern = '22_ref_case_', replacement = '', x = mod_vec[j])
  save_par[[j]] = par_df
  # Biology:
  bio_df = this_report$endgrowth %>% filter(Morph == 1) %>% select(Seas, Age_Beg, M, Len_Beg) 
  bio_df$mod_type = sub(pattern = '22_ref_case_', replacement = '', x = mod_vec[j])
  save_bio[[j]] = bio_df
  # Derived quantities:
  quant_est = this_report$derived_quants
  quant_df = quant_est %>% filter(Label %in% c('SSB_Virgin', "SSB_MSY", "annF_MSY", "Dead_Catch_MSY",
                                               paste0("SSB_", this_report$endyr),
                                               paste0("F_", this_report$endyr))) %>%
    select(Label, Value)
  quant_df$mod_type = sub(pattern = '22_ref_case_', replacement = '', x = mod_vec[j])
  save_dq[[j]] = quant_df
  # Time series:
  ts_df = quant_est %>% filter(Label %in% c(paste0("SSB_", this_report$startyr:this_report$endyr),
                                            paste0("F_", this_report$startyr:this_report$endyr))) %>%
    select(Label, Value) %>% mutate(year = as.numeric(str_split_i(Label, "_", 2)),
                                    var = str_split_i(Label, "_", 1))
  ts_df$mod_type = sub(pattern = '22_ref_case_', replacement = '', x = mod_vec[j])
  save_ts[[j]] = ts_df
  # Kobe:
  kobe_df = ts_df
  kobe_df$Value[kobe_df$var == 'SSB'] = kobe_df$Value[kobe_df$var == 'SSB'] / quant_est$Value[quant_est$Label == 'SSB_MSY']
  kobe_df$Value[kobe_df$var == 'F'] = kobe_df$Value[kobe_df$var == 'F'] / quant_est$Value[quant_est$Label == 'annF_MSY']
  kobe_df$mod_type = sub(pattern = '22_ref_case_', replacement = '', x = mod_vec[j])
  save_kobe[[j]] = kobe_df
  cat(paste0("Model set ", j, " of ", length(mod_vec), " completed.\n"))
}

# Merge:
save_par = bind_rows(save_par)
save_bio = bind_rows(save_bio)
save_dq = bind_rows(save_dq)
save_ts = bind_rows(save_ts)
save_kobe = bind_rows(save_kobe)

# Save:
saveRDS(save_par, file = file.path(dat_dir, 'YFT', 'summ_par.rds'))
saveRDS(save_bio, file = file.path(dat_dir, 'YFT', 'summ_bio.rds'))
saveRDS(save_dq, file = file.path(dat_dir, 'YFT', 'summ_dq.rds'))
saveRDS(save_ts, file = file.path(dat_dir, 'YFT', 'summ_ts.rds'))
saveRDS(save_kobe, file = file.path(dat_dir, 'YFT', 'summ_kobe.rds'))
