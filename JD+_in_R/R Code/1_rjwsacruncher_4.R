install.packages("rjwsacruncher")
library(rjwsacruncher)
browseVignettes("rjwsacruncher")
options(cruncher_bin_directory = "C:/Software/Cruncher_2.2.3/jwsacruncher-2.2.3-bin/bin")
getOption("cruncher_bin_directory")
options(default_tsmatrix_series = c("y","sa","s","d18","ycal","t","i"))


cruncher_and_param(workspace = "Workspaces/industrie.xml", #target: workspace's master xml file
                   rename_multi_documents = FALSE, # rename multi documents
                   delete_existing_file = TRUE, # replace old files
                   policy = "lastoutliers", # Policy
                   csv_layout = "vtable" # vertical layout
                   ,log_file = "ipi_refresh_log.txt" #log file
                   )

