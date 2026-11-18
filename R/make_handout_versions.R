# ---------------------------------------------------------------------------
# make_handout_versions.R
# Instructor tool. Builds the student handout and the guide from the full
# solutions file, so the three versions never drift apart:
#
#   worksheets/lab-handout-solutions.qmd  (source; git-ignored, instructor only)
#     -> worksheets/lab-handout.qmd        Example 2 + Workshop Questions blank
#     -> worksheets/lab-handout-guide.qmd  Workshop Questions blank
#   worksheets/merging-data-solutions.qmd (source; git-ignored)
#     -> worksheets/merging-data.qmd       Practice questions blank
#
# Edit the solutions files, then run from the project root:
#   Rscript R/make_handout_versions.R
# ---------------------------------------------------------------------------

# Keep each {r ...} / {asis ...} chunk header and fence, drop the body
blank_chunks <- function(lines) {
  out <- character(); inside <- FALSE
  for (l in lines) {
    if (!inside && grepl("^```\\{(r|asis)", l)) { out <- c(out, l, ""); inside <- TRUE; next }
    if (inside && grepl("^```\\s*$", l)) { out <- c(out, l); inside <- FALSE; next }
    if (!inside) out <- c(out, l)
  }
  out
}

split_at <- function(lines, pattern) {
  i <- grep(pattern, lines)[1]
  if (is.na(i)) stop("Heading not found: ", pattern)
  list(before = lines[seq_len(i - 1)], after = lines[i:length(lines)])
}

set_subtitle <- function(lines, subtitle) {
  i <- grep("^subtitle:", lines)[1]
  lines[i] <- sprintf('subtitle: "%s"', subtitle)
  lines
}

# --- Lab handout -------------------------------------------------------------
sol <- readLines("worksheets/lab-handout-solutions.qmd")
p1 <- split_at(sol, "^# Example 2:")
p2 <- split_at(p1$after, "^# Workshop Questions")
head <- p1$before; ex2 <- p2$before; qs <- p2$after

ex2_blank <- blank_chunks(ex2)
ex2_blank <- sub("^(\\*\\*Write a one-sentence takeaway for your figure here:\\*\\*).*$", "\\1", ex2_blank)

writeLines(c(set_subtitle(head, "POLS 081 · SSQL Workshop, Part 2"), ex2_blank, blank_chunks(qs)),
           "worksheets/lab-handout.qmd")
writeLines(c(set_subtitle(head, "POLS 081 · SSQL Workshop, Part 2 · Guide: Example 2 code filled in"),
             ex2, blank_chunks(qs)),
           "worksheets/lab-handout-guide.qmd")

# --- Merging exercise --------------------------------------------------------
msol <- readLines("worksheets/merging-data-solutions.qmd")
m <- split_at(msol, "^# Practice:")
writeLines(c(set_subtitle(m$before, "POLS 081 · SSQL Workshop"), blank_chunks(m$after)),
           "worksheets/merging-data.qmd")

message("Wrote lab-handout.qmd, lab-handout-guide.qmd and merging-data.qmd")
