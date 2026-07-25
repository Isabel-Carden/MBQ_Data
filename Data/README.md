# Data

Local copies of the analysis datasets used by the Ecology paper scripts in
`../scripts/`. Prefer these files over live Google Sheets so analyses can rerun
offline from this repository.

## Paper analysis snapshots (2026-07-25)

| File | Contents | Rows | Source Google Sheet |
|------|----------|------|---------------------|
| `traits_Clean_Data_Traits_snapshot.csv` | Algal functional traits (`Clean_Data_Traits` tab) | 278 | [Trait Data Sheet](https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit) |
| `herbivory_assay_snapshot.csv` | Herbivory assay (`Sheet1`) | 60 | [Herbivory Assay](https://docs.google.com/spreadsheets/d/169_iAfJME3vntApKnjd_MLyABwFc-VSpz4YJqbX4QUw/edit) |
| `nutrient_bioassay_snapshot.csv` | Grow-out / nutrient bioassay (`5/7 Data (most recent)`) | 72 | [Grow-Out Assay](https://docs.google.com/spreadsheets/d/1rsR_sxOwSnuQDyrUtvjgICQtOJ8asLwo0p5N4bFV3zY/edit) |

Note: the nutrient bioassay sheet has two columns both named `Time`; `readr`
renames them to `Time...3` and `Time...6` on import. Paper scripts use
`Site_Name` and `Percent_Change_Weight`, so this does not affect analyses.

These CSVs were exported with `googlesheets4` on **2026-07-25** (Pacific/Tahiti).
Empty trailing columns were dropped from the herbivory export.

## Older local files (pre-existing)

| File | Notes |
|------|--------|
| `HerbivoryAssay.csv` | Earlier local herbivory copy |
| `Herbivory_Assay.numbers` | Apple Numbers workbook |
| `Grow_Out_Assay.numbers` | Apple Numbers workbook |

Paper scripts use the `*_snapshot.csv` files above, not the `.numbers` workbooks.

## How scripts find these files

Run R with the working directory set to the **project root** (`MBQ_Data/`), e.g.
open `MBQ_Project.Rproj`, then:

```r
source("scripts/Herbivory_Assay_Code.R")
```

Paths like `Data/herbivory_assay_snapshot.csv` are relative to that root.
