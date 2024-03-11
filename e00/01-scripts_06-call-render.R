# https://www.johnmackintosh.net/blog/2021-11-01-scheduling/

# Conditional chunck execution
# https://www.nagraj.net/notes/conditional-r-markdown/

library(rmarkdown)
library(jsonlite)

json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))

folder <- json_args$folder
rsp <- Sys.getenv("RSTUDIO_PANDOC")

# Call render
Sys.setenv(RSTUDIO_PANDOC = rsp)
rmarkdown::render(
  input       = paste0(getwd(),"/01-scripts_07-mkdn-report.Rmd"),
  output_file = paste0(getwd(),"/04-report_01-report.html"),
  params      = list(folder = folder)
)

