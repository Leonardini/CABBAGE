# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

**CABBAGE** (Comprehensive Assessment of Bacterial-Based AMR prediction from GEnotypes) collects, extracts, and curates all publicly available data pairing bacterial genotypes with AMR phenotypes (R/I/S or MIC values). It covers both major databases (CDC, COMPARE-ML, NARMS, NDARO, PathogenWatch, PATRIC, PubMLST) and individual papers from a literature search restricted to the WHO Priority Pathogen List (2017).

## Running the Code

All scripts are R, intended to be run interactively from RStudio (project file: `AMRDataTables.Rproj`) or sourced into an R session. There are no automated build, lint, or test commands. Scripts are semi-manual: they define functions at the top, and the bottom of each file contains calls that require manual data entry (URLs, file paths) before executing.

**Load order** — source these files into a single R session in order:
1. `Utilities.R` — shared functions and constants; must be loaded first
2. `ExtractInfo.R` — genotype extraction functions
3. `PrepareTables.R` — merging raw tables from papers
4. `PreprocessAllPapers.R` — first-wave literature search processing
5. `PreprocessNewPapers.R` — second-wave literature search processing
6. `PreprocessDatabases.R` — database-specific processing
7. `ParseTables.R` — AMR package antibiotic table utilities

## Key Global Constants (defined in `Utilities.R`)

- `WORKING_DIR` — absolute base path; currently hardcoded to `~/Downloads/AMR/AMRDataTables/`. All file I/O is relative to this.
- `GOOD_SPECIES` / `GOOD_GENERA` — the target organism list (WHO Priority Pathogens). Any filtering by organism traces back to these vectors.
- `ACCESSIONS` — named regex vector mapping accession type names (`RUN`, `ASSEMBLY`, `BIOSAMPLE`, etc.) to their ENA/NCBI patterns.
- `FIELDS` — defines the canonical schema: `GENOTYPE` (Database, Accession, Type), `PHENOTYPE` (Status, Antimicrobial, Class, MICValue, …), `METADATA` (Species, PubMedID, …).

## Data Pipeline Architecture

Raw source files live in per-database or per-paper subdirectories. The pipeline produces files with standardized suffixes:

| Suffix | Meaning |
|---|---|
| `_Processed.csv` | Raw data cleaned and reshaped into a standard wide format |
| `_Merged_Processed.csv` | Multiple processed files concatenated |
| `_SplitRes_Processed.csv` | Resistance profile column expanded into per-drug columns |
| `_WidenedRes_Processed.csv` | Long-format resistance pivoted to wide (one column per drug) |
| `_Mapped.csv` | Accession IDs resolved to run-level ENA accessions |
| `_Extracted_Genotypes.csv` | Genotype accessions extracted; written to `AllExtractedGenotypes/` |

**Core functions in `Utilities.R`:**
- `findAccessions()` — scans all character columns of a table for ENA/NCBI accession patterns; returns the highest-priority match per row.
- `getGenotypes()` — wraps `findAccessions`, adds `Database` column, writes `*_Extracted_Genotypes.csv`.
- `getAllGenotypes(Dir)` — batch-runs `getGenotypes` over all `*_Processed.csv` files in a directory.
- `joinGenoPheno()` — joins a genotype file and a phenotype file on a shared ID column.
- `concatenateFiles()` — row-binds multiple processed CSVs with type coercion.
- `widenResistance()` / `splitResistance()` — reshape resistance columns.
- `convertAccessions()` — calls EBI API to resolve sample-level accessions to run-level.
- `downloadStudyDetails()` — fetches ENA `filereport` for a study/sample accession.

## File Naming Conventions

- Papers: `PMID_<id>_<supplement_filename>_<role>_Processed.csv` (role = `Genotype` or `Phenotype`)
- Preprints: `DOI_<doi_with_colons_as_is>_…_Processed.csv`
- Databases: `<DatabaseName>_<species_or_tag>_…_Processed.csv`
- PathogenWatch collections: `pathogenwatch-<species>-<collection-id>-<author-year>_…`
- PubMLST collections: `<Species>_BIGSdb_<collection-id>_…`

## Directory Layout

- `SelectedTables/` — processed tables for the first-wave literature search
- `RemainingTables/` — processed tables for lower-priority papers
- `AllExtractedGenotypes/` — one `*_Extracted_Genotypes.csv` per source, result of `getAllGenotypes`
- `Mappings/` — EBI-resolved accession mapping files (`*_Mapped.csv`) and consolidated `AllMappings*.csv`
- `IntermediateFiles/` — scratch files produced mid-pipeline
- `CABBAGEdata/` — final curated outputs including `FinalDatasets.csv`, `FoundPapers.csv`, `SelectedPapers.csv`
- `NDARO_Unprocessed/` — raw NDARO download files before processing
- `PATRIC/` — raw PATRIC genome metadata and AMR files
- `CARD Database/` — CARD reference data
- `NewTables/` — supplementary files for new/additional papers
