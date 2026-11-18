# POLS 081 · SSQL Data Workshop

Materials for the Social Science Quantitative Lab (SSQL) session in POLS 081, *Global Environmental Governance* (Swarthmore College, Fall 2026), Class Meeting #11.

## Opening the project

1. Download or clone this repository.
2. Double-click `pols081-ssql-workshop.Rproj` to open it in RStudio.
3. To rebuild the slides and site: **Build → Render Website**, or run `quarto render` in the Terminal.

## Contents

| Path | What it is |
|---|---|
| `index.qmd` | Landing page for the course website |
| `lab.qmd` | Part 2 lab page: downloads for students |
| `data-sources.qmd` | Data sources for the data paper, by primary focus, plus the data behind the course readings |
| `help.qmd` | Where to get help (SSQL office hours) |
| `slides/part1-lecture.qmd` | Part 1 slides: How numbers become evidence (Quarto reveal.js) |
| `slides/part2-lab.qmd` | Part 2 slides: the lab (runs R on the lab data when rendered) |
| `slides/ssql.scss` | Slide theme |
| `slides/figs/` | Pre-rendered figures and diagrams used in the Part 1 slides |
| `R/make_figures.R` | Rebuilds every figure in `slides/figs/` (requires `ggplot2`) |
| `R/build_lab_data.R` | Downloads and cleans the real, public lab datasets into `worksheets/data/` |
| `R/make_lab_zip.R` | Bundles the student lab files into `worksheets/pols081-lab.zip` |
| `R/make_handout_versions.R` | Instructor tool: regenerates the student and guide handouts from the (git-ignored) solutions files |
| `worksheets/lab-handout.qmd` | Student lab handout (Example 1 worked; Example 2 and Workshop Questions blank) |
| `worksheets/lab-handout-guide.qmd` / `.pdf` | Guide: the handout with Example 2's code filled in |
| `worksheets/merging-data.qmd` | Extra practice: merging two datasets |
| `worksheets/question-ladder.qmd` | Question ladder worksheet (Part 1) |
| `worksheets/paper-discussion.qmd` | Discussion prompts for the three papers (Part 1) |
| `worksheets/data/`, `worksheets/bib/` | Lab datasets and references |
| `guides/` | Setup guides: install R/RStudio (Mac, Windows), Quarto and PDF rendering |
| `reference/` | Example handout from another SSQL workshop (template for the lab handout) |

All figures in the Part 1 slides use **simulated data** for teaching purposes, labelled as such on each plot. The lab uses **real data** (World Bank, OECD, ND-GAIN, UN M49).

## Rebuilding the lab materials

1. Download the ND-GAIN Country Index zip from <https://gain.nd.edu/our-work/country-index/download-data/> and save it in `data-raw/` as `ndgain_countryindex_2026.zip` or as the unzipped folder `ndgain_countryindex_2026/` (the site blocks scripted downloads).
2. `Rscript R/build_lab_data.R` (downloads everything else and writes `worksheets/data/`).
3. `quarto render worksheets/lab-handout-guide.qmd` (the guide PDF).
4. `Rscript R/make_lab_zip.R` (the student download).
5. `quarto render` (the site).

Instructor-only solution files (`*-solutions.qmd`/`.pdf`) are git-ignored and never published.

## Requirements

- R (4.x) with the `tidyverse` (and `jsonlite`, installed with it)
- Quarto 1.4 or later (bundled with recent RStudio)

## Publishing

The site renders to `_site/`. To publish on GitHub Pages, run `quarto publish gh-pages` from the project root.
