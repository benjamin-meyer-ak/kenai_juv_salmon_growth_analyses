# Kenai Juvenile Salmon Growth Analyses — Project Memory

Last updated: 2026-09-08

## Project Overview

This repo supports Meyer et al. 2023 (TAFS): "Landscape characteristics influence projected growth rates of stream-resident juvenile salmon in the face of climate change in the Kenai River watershed, south-central Alaska." Data archived at https://knb.ecoinformatics.org/view/doi:10.5063/F1Q52MZF. The repo is being refactored for reproducibility and to support deliverables under an AKSSF grant (PI: Erin Larson, UAA; KWF/Meyer is a sub-recipient).

## Three Goals (Priority Order)

### Goal 1 (Priority + time-sensitive, grant ends 11/30/26): AKSSF Deliverable
- KWF's outstanding minimum deliverable: contribute diet composition data/analysis to Objective 1 of the AKSSF grant (freshwater prey availability database)
- No format yet specified by ACCS/Larson; plan is to prepare draft summary figures/tables, then ask Erin Larson for format/submission requirements
- The diet composition data in this repo (2015-2016) is the basis for this deliverable
- Goals 1 and 3 are tightly coupled — completing Goal 3 fulfills Goal 1

### Goal 2: Quarto Book Conversion
- Convert Bookdown (Rmd + _bookdown.yml) to a fully reproducible Quarto Book
- Scope: raw data → all outputs via R chunks; all coding decisions explained before executing
- All calculations in R chunks; no magrittr pipe; base R pipe |> preferred
- FB4 bioenergetics step: "commit outputs" approach (see below)
- GitHub issue requesting scriptable FB4 interface: lowest priority / deferred

### Goal 3 (tied to Goal 1): Diet Composition Summaries
- Summarize how proportions of terrestrial vs. aquatic (and marine) subsidies vary across space and time from 2015-2016 data
- Review literature for presentation examples
- Primary raw data: other/inputs/diet_size_other/2015 EPSCoR SCTC.xlsx and 2016_EPSCoR_Aquatic_Ecology_Database.xlsx

## Key Architectural Decision: FB4 Integration

**Approach: "commit outputs"**

FB4 v1.1.8 (https://github.com/jim-breck/FB4) uses Shiny and sources its model functions with `local = TRUE` inside `shinyServer()`. The functions are not independently callable without reproducing the Shiny reactive environment. Therefore:

1. R code in the Quarto Book generates all FB4 input files and the design file from raw data (fully reproducible)
2. A chapter callout documents how to download FB4 v1.1.8 and run it with the design file
3. FB4 output CSVs are committed to the repo (e.g., `fb4_outputs/`)
4. All downstream chapters read from the committed outputs — always runnable without FB4

Do NOT modify or extract code from FB4.

## Repo Structure

```
/                          # Bookdown root (to be converted to Quarto Book)
  index.Rmd
  01-2015-2016-model-input-generation.Rmd   # diet + temp inputs, FB4 input generation
  02-projected-model-input-generation.Rmd
  03-simulation-results-analyses.Rmd
  04/05/06-chapterxx.Rmd                    # incomplete chapters

other/
  agent_context.qmd                         # project context doc
  documents/
    AKSSF_Proposal_Form_final.docx          # AKSSF grant proposal (also .md version)
  inputs/
    diet_size_other/                        # PRIMARY RAW DIET DATA
      2015 EPSCoR SCTC.xlsx
      2016_EPSCoR_Aquatic_Ecology_Database.xlsx
      EPSCoR_SCTC_Prey_Energy_Densities.xlsx
      sim_size_inputs_v2.xlsx
    temperature/observed_water_temp/daily_temp_metrics.csv
  outputs/
    diet/                                   # processed diet outputs
      diet_v1.csv
      wet_mass_diet_proportions.csv
      mass_conversion/prey_types_manual_edit.xlsx
    all_fish_age_size.csv
    all_simulations.csv
    observed_diet_proportions.csv
    fish_size_distribution.csv
  FB4_1.1.3_2015_2016.1/                   # OLD: FB4 v1.1.3 app + input/output files
  FB4_1.1.3_projected.1/                   # OLD: FB4 v1.1.3 projected scenarios
  FB4.1_2015_2016/                         # OLD: custom FB4 app (broken)
```

## Diet Data Schema (from 01-Rmd)

Key sheets:
- "2015 Diet Contents Data" / "2016 Diet Contents Data": prey items per fish (gastric lavage)
  - Columns: Sample.ID, Fish_Species, Sample.Event, sample.event.num, River, Reach, Site, Sample_Date, Prey_Type_Used, PreyCategory, Quantity, Total_Prey_Dry_Mass_mg
- "C 2015 Diet & LW Data" / "C 2016 Diet & LW Data": fish length, weight, age
- "B Fishing Data": sampling event dates and locations

Prey categories used in bioenergetics (diet1–diet5):
- diet1: FishEggs
- diet2: InvertAquatic_AqOrigin
- diet3: InvertTerrestrial
- diet4: InvertTerrestrial_AqOrigin
- diet5: SalmonEggs

Species: Coho and Chinook salmon only. Ages: primarily age 0 and age 1.
Rivers: Beaver Creek, Russian River, Ptarmigan Creek, Kenai River.
Seasons: Early Summer (event 1), Mid-Summer (2), Late Summer (3), Fall (4; 2016 only).

## Dry mass → wet mass conversion
Prey dry masses are converted to wet mass using wet/dry ratios from McCarthy 2009 and other sources, stored in: other/outputs/diet/mass_conversion/prey_types_manual_edit.xlsx

## Conventions
- No AI tropes (avoid em-dashes, excessive hedging)
- Minimize token usage (convert PDFs/docx to .md before reading)
- Use base R pipe |>; not magrittr %>%
- Brief code comments only; no narrating comments
- ggplot2 for visualization; no coord_flip(); no dual encoding
- bslib for any Shiny UI
- All coding decisions explained before executing (Goal 2)
