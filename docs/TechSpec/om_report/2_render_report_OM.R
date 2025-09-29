rm(list = ls())
require(quarto)
require(here)
this_path = file.path(here::here(), 'docs/TechSpec/om_report')
stock_vec = c('bigeye', 'skipjack', 'yellowfin')

# Render OM check html:
setwd(this_path)
for(m in 1:length(stock_vec)) {
  output_file = paste0(paste('om_report', 
                             stock_vec[m], sep = '_'), 
                       '.html')
  # Render document:
  quarto_render(input = 'om_report.qmd', 
                output_format = 'html',
                output_file = output_file,
                execute_params = list(stock = stock_vec[m]))
}
setwd(here::here())
