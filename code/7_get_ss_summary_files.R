# figure out directories
mydir <- getwd()
outer_folder <- file.path(mydir, "Scenarios")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
thinned_output <- file.path(mydir, "output_thinned", basename(outer_folder))

# get list of all files (112800 total)
files <- dir(outer_folder_output, full.names = TRUE, recursive = TRUE)
# get files to copy (all ss_summary files, and useful files in each r001 dir)
files_good <- files[
  grepl("ss_summary", files) |
    (grepl("run001", files) &
      (
        grepl("starter.ss", files) |
          grepl("forecast.ss", files) |
          grepl("platoons_data.ss", files) |
          grepl("platoons_control.ss", files) |
          grepl("no_platoons_control.ss", files) |
          grepl(".ss_new", files) |
          grepl("Report.sso", files) |
          grepl("CompReport.sso", files) |
          grepl("covar.sso", files) |
          grepl("warning.sso", files)
      )
    )
]

# copy chosen files
for (ifile in 1:length(files_good)) {
  file <- files_good[ifile]
  newfile <- gsub("/output/", "/output_thinned/", file, fixed = TRUE)
  dir.create(dirname(newfile), recursive = TRUE)
  file.copy(file, newfile)
  if (ifile %% 100 == 0) print(ifile)
}

# final step: manualy delete 8 extra scenario x date folders that didn't have 
# any results because they were not run on the particular date (Oct vs Dec 2022)