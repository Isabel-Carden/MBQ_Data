# MBQ_Data

Source code and related files for the Moorea benthic / algal trait Ecology
manuscript analyses.

## Paper analysis scripts (`scripts/`)

These six scripts match the Ecology initial-submission source code document
(`Ecology_Source_Code.docx`). They are the source of record for the paper.

| Analysis | Script |
|----------|--------|
| Site maps | `scripts/Map_Code.R` |
| Herbivory assay | `scripts/Herbivory_Assay_Code.R` |
| Nutrient bioassay (grow-out) | `scripts/Nutrient_Bioassay_Code.R` |
| Site characterization panels | `scripts/Site_Characterization_Panel_Code.R` |
| Trait PCA and PERMANOVA | `scripts/PCA_and_Permanova_Code.R` |
| Trait univariate analyses | `scripts/Univariate_Analysis_Code.R` |

Most scripts read from public Google Sheets via `googlesheets4` (not only local
files in `Data/`).

## Folder structure

```
MBQ_Data/
├── scripts/     # Paper source-of-record R scripts
├── archive/     # Older drafts, duplicates, and tutorials (not deleted)
├── Data/        # Local data files (e.g. herbivory CSV / Numbers)
├── logs/        # Change record
└── MBQ_Project.Rproj
```

## Archive policy

Nothing important was deleted. Drafts and near-duplicate scripts were **moved**
to `archive/` so the paper scripts are easy to find. See
`logs/CHANGELOG.md` for what was archived and why.

## GitHub

https://github.com/Isabel-Carden/MBQ_Data
