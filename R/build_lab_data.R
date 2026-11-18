# ---------------------------------------------------------------------------
# build_lab_data.R
# Downloads and cleans the real, public datasets used in the Part 2 lab
# handout (worksheets/lab-handout.qmd) and the merging exercise
# (worksheets/merging-data.qmd).
#
# Run from the project root:  source("R/build_lab_data.R")
# Requires: tidyverse, jsonlite (installed with tidyverse), readxl not needed.
#
# Raw downloads are cached in data-raw/ (git-ignored). Clean files are
# written to worksheets/data/. Delete data-raw/ to force fresh downloads.
#
# Sources (accessed November 17, 2026):
#   - World Bank, World Development Indicators (WDI), API v2
#       https://api.worldbank.org/v2/
#       Country metadata (region, income group) and indicators:
#         EN.GHG.CO2.MT.CE.AR5  CO2 emissions, total excl. LULUCF (Mt CO2e)
#                               (WDI takes this series from EDGAR, JRC)
#         EN.GHG.CO2.PC.CE.AR5  CO2 emissions per capita excl. LULUCF (t CO2e/person)
#         EG.FEC.RNEW.ZS        Renewable energy consumption (% of final energy)
#         NY.GDP.PCAP.CD        GDP per capita (current US$)
#         SP.POP.TOTL           Population, total
#   - OECD, "RioMarkers: Aid (ODA) activities targeting global environmental
#     objectives" (dataflow OECD.DCD.FSD:DSD_RIOMRKR@DF_RIOMARKERS v1.7),
#     OECD Data Explorer SDMX API. DAC members, bilateral allocable ODA,
#     commitments, constant 2024 USD, climate change adaptation marker,
#     principal + significant.
#   - Notre Dame Global Adaptation Initiative (ND-GAIN) Country Index,
#     2026 release. https://gain.nd.edu/our-work/country-index/download-data/
#     The ND-GAIN site blocks scripted downloads, so download the zip by hand
#     and save it (or the unzipped folder) in data-raw/ as
#     ndgain_countryindex_2026.zip or ndgain_countryindex_2026/.
#   - UN Statistics Division, Standard country or area codes (M49), SIDS flag.
#       https://unstats.un.org/unsd/methodology/m49/overview/
# ---------------------------------------------------------------------------

library(tidyverse)

options(timeout = 600)  # the World Bank API can be slow on the first call

raw_dir <- "data-raw"
out_dir <- "worksheets/data"
dir.create(raw_dir, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Download once, then reuse the cached copy
cached <- function(file, url) {
  path <- file.path(raw_dir, file)
  tries <- 0
  while (!file.exists(path) && tries < 4) {
    tries <- tries + 1
    ok <- tryCatch({ download.file(url, path, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) { unlink(path); Sys.sleep(5 * tries) }
  }
  if (!file.exists(path)) stop("Could not download ", url)
  path
}

# --- World Bank: country metadata ------------------------------------------
wb_countries <- cached("wb_countries.json",
                       "https://api.worldbank.org/v2/country?format=json&per_page=400") |>
  jsonlite::fromJSON()
countries <- tibble(
  iso3c        = wb_countries[[2]]$id,
  # Nauru's official short name is now "Naoero" in both World Bank and UN lists;
  # keep it, but add the familiar name so students recognize it.
  country      = str_replace(wb_countries[[2]]$name, "^Naoero$", "Naoero (Nauru)"),
  region       = wb_countries[[2]]$region$value,
  income_group = wb_countries[[2]]$incomeLevel$value
) |>
  filter(region != "Aggregates") |>
  mutate(income_group = na_if(income_group, "Not classified"))

# --- World Bank: indicators -------------------------------------------------
wb_indicator <- function(code, years = "2018:2023") {
  x <- cached(paste0("wb_", code, ".json"),
              sprintf("https://api.worldbank.org/v2/country/all/indicator/%s?format=json&date=%s&per_page=20000",
                      code, years)) |>
    jsonlite::fromJSON()
  tibble(iso3c = x[[2]]$countryiso3code,
         year  = as.integer(x[[2]]$date),
         value = x[[2]]$value)
}
wb_year <- function(code, yr) {
  wb_indicator(code) |> filter(year == yr) |> select(iso3c, value)
}

# --- UN M49: SIDS list ------------------------------------------------------
# Turn HTML character references such as "&#231;" into the characters themselves
decode_entities <- function(x) {
  x <- str_replace_all(x, "&#(\\d+);", \(m) intToUtf8(as.integer(str_extract(m, "\\d+"))))
  str_replace_all(x, c("&amp;" = "&", "&quot;" = "\"", "&#39;" = "'"))
}
m49_html <- cached("m49_overview.html", "https://unstats.un.org/unsd/methodology/m49/overview/") |>
  read_file()
m49_table <- str_extract(m49_html, regex('<table id = "downloadTableEN".*?</table>', dotall = TRUE))
m49_rows  <- str_match_all(m49_table, regex("<tr.*?</tr>", dotall = TRUE))[[1]][, 1]
m49_cells <- map(m49_rows, \(r) {
  str_match_all(r, regex("<t[dh][^>]*>(.*?)</t[dh]>", dotall = TRUE))[[1]][, 2] |>
    str_remove_all("<[^>]+>") |> str_trim() |> decode_entities()
})
m49 <- m49_cells[-1] |>
  map(\(x) set_names(x, m49_cells[[1]])) |>
  bind_rows()
pop_2023 <- wb_year("SP.POP.TOTL", 2023) |> rename(population = value)

sids <- m49 |>
  filter(`Small Island Developing States (SIDS)` != "") |>
  transmute(iso3c = `ISO-alpha3 Code`,
            sids_name = str_replace(`Country or Area`, "^Naoero$", "Naoero (Nauru)"))
stopifnot(nrow(sids) > 50)

# ===========================================================================
# 2. Example 2: renewable energy and income  -> renewables.csv
#    2021 is the most recent year with near-complete renewables coverage.
# ===========================================================================
renewables <- countries |>
  left_join(wb_year("EG.FEC.RNEW.ZS", 2021) |> rename(renew_share = value), by = "iso3c") |>
  left_join(wb_year("NY.GDP.PCAP.CD", 2021) |> rename(gdp_pc = value), by = "iso3c") |>
  left_join(wb_year("SP.POP.TOTL", 2021) |> rename(population = value), by = "iso3c") |>
  filter(!is.na(renew_share), !is.na(gdp_pc), !is.na(income_group)) |>
  mutate(renew_share = round(renew_share, 1), gdp_pc = round(gdp_pc)) |>
  select(country, iso3c, region, income_group, population, gdp_pc, renew_share) |>
  arrange(country)

write_csv(renewables, file.path(out_dir, "renewables.csv"))
message("renewables.csv: ", nrow(renewables), " rows")

# ===========================================================================
# 3. Workshop questions: total vs. per capita CO2  -> co2.csv
# ===========================================================================
co2 <- countries |>
  left_join(wb_year("EN.GHG.CO2.MT.CE.AR5", 2023) |> rename(co2_mt = value), by = "iso3c") |>
  left_join(wb_year("EN.GHG.CO2.PC.CE.AR5", 2023) |> rename(co2_pc = value), by = "iso3c") |>
  left_join(pop_2023, by = "iso3c") |>
  filter(!is.na(co2_mt), !is.na(co2_pc), !is.na(income_group)) |>
  # EDGAR doesn't estimate some small territories separately; the WDI reports
  # them as 0 or a few tonnes (e.g., Guam 0.0003 Mt). Drop totals < 0.01 Mt.
  filter(co2_mt >= 0.01) |>
  mutate(co2_mt = round(co2_mt, 2), co2_pc = round(co2_pc, 2)) |>
  select(country, iso3c, region, income_group, population, co2_mt, co2_pc) |>
  arrange(country)

write_csv(co2, file.path(out_dir, "co2.csv"))
message("co2.csv: ", nrow(co2), " rows")

# Same series for 2018-2023, for the "change over time" example in the
# data visualization guide  -> co2_trend.csv
co2_trend <- co2 |>
  select(country, iso3c, region, income_group) |>
  inner_join(wb_indicator("EN.GHG.CO2.MT.CE.AR5") |> rename(co2_mt = value), by = "iso3c") |>
  inner_join(wb_indicator("EN.GHG.CO2.PC.CE.AR5") |> rename(co2_pc = value), by = c("iso3c", "year")) |>
  filter(!is.na(co2_mt), !is.na(co2_pc)) |>
  mutate(co2_mt = round(co2_mt, 2), co2_pc = round(co2_pc, 2)) |>
  arrange(country, year)

write_csv(co2_trend, file.path(out_dir, "co2_trend.csv"))
message("co2_trend.csv: ", nrow(co2_trend), " rows")

# ===========================================================================
# 4. Merging exercise: two files that share a country code
# ===========================================================================
write_csv(sids |> transmute(iso3c, sids_name, sids = "SIDS"),
          file.path(out_dir, "sids_list.csv"))
message("sids_list.csv: ", nrow(sids), " rows")

# ===========================================================================
# 5. Example 1 (last, because it needs the hand-downloaded ND-GAIN file): adaptation aid and vulnerability  -> adaptation_aid.csv
# ===========================================================================
rio_url <- paste0(
  "https://sdmx.oecd.org/dcd-public/rest/data/",
  "OECD.DCD.FSD,DSD_RIOMRKR@DF_RIOMARKERS,1.7/",
  "DAC_EC..1000.100.2.30.1+2.C.Q._T..USD",
  "?startPeriod=2020&endPeriod=2024&format=csvfilewithlabels"
)
rio <- read_csv(cached("oecd_riomarkers_adaptation_2020_2024.csv", rio_url),
                show_col_types = FALSE)

# Sum principal + significant within each recipient-year, then average over
# 2020-2024. Years with no reported commitments count as zero.
adapt <- rio |>
  filter(RECIPIENT %in% countries$iso3c) |>
  group_by(iso3c = RECIPIENT, year = TIME_PERIOD) |>
  summarize(aid = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") |>
  group_by(iso3c) |>
  summarize(adapt_aid_musd = sum(aid) / 5)

ndgain_zip <- file.path(raw_dir, "ndgain_countryindex_2026.zip")
ndgain_dir <- file.path(raw_dir, "ndgain_countryindex_2026")
if (!dir.exists(ndgain_dir)) {
  if (!file.exists(ndgain_zip)) {
    stop("Download the ND-GAIN Country Index zip from\n",
         "https://gain.nd.edu/our-work/country-index/download-data/\n",
         "and save it (or the unzipped folder) as ", ndgain_zip)
  }
  unzip(ndgain_zip, exdir = ndgain_dir)
}
vuln_file <- list.files(ndgain_dir, pattern = "^vulnerability\\.csv$",
                        recursive = TRUE, full.names = TRUE)
vuln_file <- vuln_file[basename(dirname(vuln_file)) == "vulnerability"][1]  # not trends/
vuln_wide <- read_csv(vuln_file, show_col_types = FALSE)
vuln_year <- max(as.integer(str_subset(names(vuln_wide), "^\\d{4}$")))
vuln <- vuln_wide |>
  select(iso3c = ISO3, vulnerability = all_of(as.character(vuln_year)))

adaptation_aid <- adapt |>
  inner_join(countries, by = "iso3c") |>
  left_join(vuln, by = "iso3c") |>
  left_join(pop_2023, by = "iso3c") |>
  mutate(sids = if_else(iso3c %in% sids$iso3c, "SIDS", "Other"),
         adapt_aid_musd = round(adapt_aid_musd, 3),
         vulnerability = round(vulnerability, 3)) |>
  filter(!is.na(vulnerability), !is.na(population)) |>
  select(country, iso3c, region, income_group, sids, population,
         vulnerability, adapt_aid_musd) |>
  arrange(country)

write_csv(adaptation_aid, file.path(out_dir, "adaptation_aid.csv"))
message("adaptation_aid.csv: ", nrow(adaptation_aid), " rows; ND-GAIN year ", vuln_year)
