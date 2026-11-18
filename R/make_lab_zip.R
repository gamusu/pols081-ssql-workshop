# ---------------------------------------------------------------------------
# make_lab_zip.R
# Bundles the student lab files into worksheets/pols081-lab.zip, a small
# self-contained RStudio project students download and unzip:
#
#   pols081-lab/
#   ├── pols081-lab.Rproj
#   ├── lab-handout.qmd
#   ├── merging-data.qmd
#   ├── data/   (adaptation_aid.csv, renewables.csv, co2.csv, sids_list.csv)
#   └── bib/    (lab.bib)
#
# Solutions are never included. Run R/build_lab_data.R first.
# Run from the project root:  source("R/make_lab_zip.R")
# ---------------------------------------------------------------------------

files <- c("lab-handout.qmd", "merging-data.qmd",
           file.path("data", c("adaptation_aid.csv", "renewables.csv", "co2.csv", "sids_list.csv")),
           file.path("bib", "lab.bib"))
missing <- files[!file.exists(file.path("worksheets", files))]
if (length(missing) > 0) stop("Missing: ", paste(missing, collapse = ", "))

stage <- file.path(tempdir(), "pols081-lab")
unlink(stage, recursive = TRUE)
dir.create(file.path(stage, "data"), recursive = TRUE)
dir.create(file.path(stage, "bib"))
invisible(file.copy(file.path("worksheets", files), file.path(stage, files)))

writeLines(c(
  "Version: 1.0", "",
  "RestoreWorkspace: No", "SaveWorkspace: No", "AlwaysSaveHistory: Default", "",
  "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8"
), file.path(stage, "pols081-lab.Rproj"))

zip_path <- file.path(normalizePath("worksheets"), "pols081-lab.zip")
unlink(zip_path)
old <- setwd(tempdir())
utils::zip(zip_path, "pols081-lab", flags = "-r9Xq")
setwd(old)
if (!file.exists(zip_path)) stop("Zip was not created: ", zip_path)
message("Wrote ", zip_path)
