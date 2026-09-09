# Kenai Juvenile Salmon Growth Analyses — Project Memory

Last updated: 2026-09-08

## Project Overview

This repo supports Meyer et al. 2023 (TAFS): "Landscape characteristics influence projected growth rates of stream-resident juvenile salmon in the face of climate change in the Kenai River watershed, south-central Alaska." Data archived at https://knb.ecoinformatics.org/view/doi:10.5063/F1Q52MZF. The repo is being refactored for reproducibility and to support deliverables under an AKSSF grant (PI: Erin Larson, UAA; KWF/Meyer is a sub-recipient).

## Three Goals (Priority Order)

### Goal 1 (Priority + time-sensitive, grant ends 11/30/26): AKSSF Deliverable
- KWF's outstanding minimum deliverable: contribute diet composition data/analysis to Objective 1 of the AKSSF grant (freshwater prey availability database)
- No format yet specified by ACCS/Larson; plan is to prepare draft summary figures/tables, then ask Erin Larson for format/submission requirements
- Primary chapter: `diet-composition.qmd` — pipeline is complete but the main stacked bar visualization (`diet-plot-main` chunk) needs `preycat` levels confirmed and color palette finalized before use as a deliverable
- Goals 1 and 3 are tightly coupled — completing Goal 3 fulfills Goal 1

### Goal 2: Quarto Book Conversion — STRUCTURALLY COMPLETE
- All 7 Rmd files and all Bookdown-specific files have been deleted
- 8-chapter Quarto Book renders cleanly (exit 0)
- Bioenergetics input chapters (4, 5) have file-writing code marked `eval: false` since inputs are already committed; they read from committed CSVs for all active code
- Simulation results chapter (6) actively runs Tables S3/S5/Table 4 and Figures 6/8 from committed CSVs; Google Sheets-dependent exploratory code preserved as `eval: false`
- Remaining TODO: the landscape characteristic correlations in simulation-results.qmd need `sensitivity_values.csv` committed to the repo (currently only existed in Google Sheets)

### Goal 3 (tied to Goal 1): Diet Composition Summaries
- Summarize how proportions of terrestrial vs. aquatic (and marine) subsidies vary across space and time from 2015-2016 data
- Primary chapter: `diet-composition.qmd`
- Pipeline is built: data import → dry→wet mass conversion → per-fish proportions → grouped summaries → stacked bar visualization
- Next action: run the chapter interactively, inspect `preycat` levels from actual data, finalize `subsidy_map` in the `subsidy-classification` chunk, and refine the plot

## Key Architectural Decision: FB4 Integration

**Approach: "commit outputs"**

FB4 is a Shiny app whose model functions are not independently callable. Therefore:

1. R code in the Quarto Book generates all FB4 input files and design files from raw data (fully reproducible — Chapters 4 and 5)
2. Callout blocks in Chapters 4 and 5 document how to run FB4 manually with the design file
3. FB4 output CSVs are committed to the repo
4. All downstream code in Chapter 6 reads from the committed outputs

Do NOT modify or extract code from FB4.

**FB4 versions:**
- **v1.1.3** — used in the original Meyer et al. 2023 analysis. Committed output files reflect this version and should not be regenerated without a specific reason.
- **v1.1.8** — current version as of 2026-08-01 (https://github.com/jim-breck/FB4). Use this for any new or rerun analyses going forward. Release notes confirm no change to model results between v1.1.3 and v1.1.8 (intervening releases added Design File options, a thermoregulation feature, and minor bug fixes only). Results are directly comparable to committed v1.1.3 outputs.

**FB4 committed output file locations (from original v1.1.3 runs):**
- `other/FB4_1.1.3_2015_2016.1/FB4_2015_2016_models_old_parameters.csv` (Stewart & Ibarra)
- `other/FB4_1.1.3_2015_2016.1/FB4_2015_2016_models_new_parameters.csv` (Plumb & Moffitt)
- `other/FB4_1.1.3_projected.1/FB4_projected_models_old_parameters.csv`
- `other/FB4_1.1.3_projected.1/FB4_projected_models_new_parameters.csv`

## Repo Structure (current)

```
/                               # Quarto Book root
  _quarto.yml                   # book config (cosmo theme, code-fold, toc-depth 3)
  index.qmd                     # Introduction
  study-area-methods.qmd        # Ch 1: study area, field methods
  diet-literature-review.qmd    # Ch 2: lit review on juvenile salmon diet
  diet-composition.qmd          # Ch 3: PRIMARY AKSSF deliverable chapter
  bioenergetics-inputs-observed.qmd  # Ch 4: FB4 inputs from 2015-2016 data
  bioenergetics-inputs-projected.qmd # Ch 5: FB4 inputs for projected scenarios
  simulation-results.qmd        # Ch 6: FB4 outputs, Tables S3/S5/4, Figs 6/8
  references.qmd                # References placeholder
  AGENTS.md                     # this file
  kenai_juv_salmon_growth_analyses.Rproj

other/
  agent_context.qmd             # project context narrative
  documents/
    AKSSF_Proposal_Form_final.docx   # AKSSF grant proposal (also .md)
    Meyer et al TAFS Resubmission June 29 2022/  # manuscript materials
      TAFS_Meyer.md             # manuscript converted to markdown
  inputs/
    diet_size_other/            # PRIMARY RAW DIET DATA
      2015 EPSCoR SCTC.xlsx
      2016_EPSCoR_Aquatic_Ecology_Database.xlsx
      EPSCoR_SCTC_Prey_Energy_Densities.xlsx
      sim_size_inputs_v2.xlsx
    temperature/
      observed_water_temp/daily_temp_metrics.csv
      projected_temps.csv       # downscaled projected water temperatures
  outputs/
    diet/
      diet_v1.csv               # processed diet dry mass (original version)
      wet_mass_diet_proportions.csv  # pooled diet props by river/spp/age (wet mass)
      mass_conversion/prey_types_manual_edit.xlsx  # wet/dry ratios
    all_fish_age_size.csv
    all_simulations.csv
    observed_diet_proportions.csv   # diet props joined to temporal extents
    fish_size_distribution.csv
    obs_sim_results.csv         # Table S3 output
    pct_chg_tbl.csv             # Table S5 output
    pct_chg_summ_tbl.csv        # Table 4 output
  FB4_1.1.3_2015_2016.1/       # FB4 v1.1.3 observed data runs (inputs + outputs)
  FB4_1.1.3_projected.1/       # FB4 v1.1.3 projected scenario runs
  FB4.1_2015_2016/              # Legacy broken custom FB4 app (do not use)
```

## Author's Role in Related Literature

- Co-author on **Wipfli et al. 2019** (J. Freshwater Ecology) — invertebrate prey contributions to juvenile Coho diet in the Kenai watershed (Beaver Creek, Ptarmigan Creek, Russian River). Direct overlap in rivers and time period with the 2015-2016 data in this repo.
- Field technician for **Rine et al. 2016** (CJFAS) — trophic pathways of juvenile Chinook and Coho in the Susitna River, Alaska. Stomach content + stable isotope approach.

## Diet Data Schema

**Raw Excel workbooks:**
- `other/inputs/diet_size_other/2015 EPSCoR SCTC.xlsx`
- `other/inputs/diet_size_other/2016_EPSCoR_Aquatic_Ecology_Database.xlsx`

**Key sheets:**
- "2015/2016 Diet Contents Data": prey items per fish (gastric lavage)
  - Columns: Sample.ID, Fish_Species, Sample.Event, sample.event.num, River, Reach, Site, Sample_Date, Prey_Type_Used, PreyCategory, Quantity, Total_Prey_Dry_Mass_mg
- "C 2015/2016 Diet & LW Data": fish length, weight, age (Age_manual2)
- "B Fishing Data": trap deployment/collection dates per site

**Cleaned column names used in qmd code:**
`sample.id, spp, sample.event, event.num, river, reach, site, date, year, prey, preycat, quantity, dm_mg`

**Prey categories (5 bioenergetics slots):**

| Slot  | PreyCategory                | Label in figures         |
|-------|-----------------------------|--------------------------|
| diet1 | FishEggs                    | Fish Eggs                |
| diet2 | InvertAquatic_AqOrigin      | Immature Aquatic         |
| diet3 | InvertTerrestrial           | Terrestrial              |
| diet4 | InvertTerrestrial_AqOrigin  | Adult Aquatic            |
| diet5 | SalmonEggs                  | Salmon Eggs              |

Species: Coho and Chinook salmon only.
Ages: 0 and 1 (age-2 Coho and age-1 Chinook excluded from primary analyses).
Rivers: Beaver Creek, Russian River, Ptarmigan Creek, Kenai River.
Seasons: Early Summer (1), Mid-Summer (2), Late Summer (3), Fall (4; 2016 only).

## Dry mass → wet mass conversion

Prey dry masses → wet mass via ratios from McCarthy 2009 + other sources.
Mapping file: `other/outputs/diet/mass_conversion/prey_types_manual_edit.xlsx`
Sheets: "prey_types" (maps each unique prey taxon to a category), "data_validation" (wet/dry ratios and energy densities per category)
Formula: `wm_mg = dm_mg / wet_dry_ratio`

## Conventions
- No AI tropes (avoid em-dashes, excessive hedging)
- Minimize token usage (convert PDFs/docx to .md before reading)
- Use base R pipe |>; not magrittr %>%
- Brief code comments only; no narrating comments
- ggplot2 for visualization; no coord_flip(); no dual encoding
- bslib for any Shiny UI
- All coding decisions explained before executing
