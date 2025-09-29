rm(list = ls())
require(quarto)
require(here)
selected_om = read.csv("docs/TechSpec/tables/selected_OM.csv")
this_path = file.path(here::here(), 'docs/TechSpec/om_check')

# Render OM check html:
setwd(this_path)
for(m in 1:nrow(selected_om)) {
  clean_model = sub(pattern = '22_ref_case_', replacement = '', x = selected_om$Model[m])
  output_file = paste0(paste('OM', 
                             selected_om$Stock[m], 
                             clean_model, sep = '_'), 
                       '.html')
  # Render document:
  quarto_render(input = 'om_check.qmd', 
                output_format = 'html',
                output_file = output_file,
                execute_params = list(Stock = selected_om$Stock[m], 
                                      Model = selected_om$Model[m]))
}
setwd(here::here())
