# Changelog — MBQ_Data

## 2026-07-25 (Pacific/Tahiti)

### Organized repository around Ecology source-of-record scripts

**Why:** The repo root contained many overlapping drafts. The Ecology
initial-submission document `Ecology_Source_Code.docx` was used to identify the
six paper scripts (all `*_Code.R` files except `Dunns_Code.R`).

**Kept as paper scripts → `scripts/`**
- `Map_Code.R`
- `Herbivory_Assay_Code.R`
- `Nutrient_Bioassay_Code.R`
- `Site_Characterization_Panel_Code.R`
- `PCA_and_Permanova_Code.R`
- `Univariate_Analysis_Code.R`

**Archived (moved, not deleted) → `archive/`**
- `Dunns_Code.R` — near-duplicate of herbivory assay; not a separate paper section
- `GrowOut.R` — earlier / alternate nutrient bioassay draft
- `HerbivoryAssay.R` — earlier herbivory draft
- `HOWTOPCAPERMANOVA.R` — tutorial / plug-and-chug notes
- `Map_of_Moorea.R`, `Site_Maps.R` — map drafts (paper maps = `Map_Code.R`)
- `PCA_and_Permanova_Updated.R`, `Permanova_and_PCA.R` — earlier PCA/PERMANOVA drafts
- `site_characterization_panel.R` — near-duplicate of panel Code script
- `Univariate_Analyses_Plot.R`, `Univariate_Analyses_with_GLM.R`,
  `Univariate_Analysis_On_Traits.R`, `Univariate_Analysis_On_Traits_Updated.R` —
  earlier univariate drafts
- `MBQ_Project/` — nested leftover project folder

**Unchanged**
- `Data/` left in place
- `MBQ_Project.Rproj` left at project root
