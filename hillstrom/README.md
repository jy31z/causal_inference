# Hillstrom Randomized Email Experiment

This project analyzes a three-arm randomized email marketing experiment to estimate how email campaigns affect purchases and how treatment effects vary across customers. It checks covariate balance, estimates average treatment effects, and compares S-, T-, X-, and DR-learners using held-out uplift and Qini curves.

The main analysis uses `grf`, compares womenswear email with no email, and creates an 80/20 training/test split stratified by treatment arm. The DR implementation does not use cross-fitting.

## Files

- `data/hillstrom.csv`: experiment data.
- `R/01_data.R`: data loading, sample splitting, descriptive statistics, SMDs, and feature encoding.
- `R/02_model.R`: ATE estimates, S/T/X/DR learners, and uplift/Qini curves.
- `report.Rmd` / `report.html`: analysis report. The Rmd reads code directly from the scripts by section, avoiding a duplicate analysis implementation.
- `outputs/`: generated figures.

## Run the Analysis

Use `hillstrom/` as the working directory. Run both scripts in the same R session:

```r
# Install dependencies before the first run:
# install.packages(c("grf", "dplyr", "tibble", "tidyr", "ggplot2", "knitr", "rmarkdown"))
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
source("R/01_data.R")
source("R/02_model.R")
```

Generate the HTML report (requires Pandoc, which is typically bundled with RStudio):

```r
rmarkdown::render("report.Rmd", output_format = "html_document")
```

Figures are written to `outputs/`. The report references line ranges in the scripts; update the corresponding ranges in the Rmd if script lines are added or removed.
