This is the repository for the *CABBAGE* project, which is an acronym for:

Comprehensive Assessment of Bacterial-Based AMR prediction from GEnotypes

----------

It aims at collecting, extracting and curating all publicly available data containing both

genotypes and matched AMR phenotypes (categorical R/I/S or quantitative values like MICs).

Unlike most existing efforts, it includes both *databases* as well as *individual papers*.

Of the former we process CDC, COMPARE-ML, NARMS, NDARO, PathogenWatch, PATRIC, and PubMLST.

Of the latter we process any paper found in a literature search with at least 100 isolates

belonging to a bacterial pathogen included in the WHO Priority Pathogen List, 2017 version.

----------

The repository contains the R scripts used to extract the initial version of the database.

*PreprocessAllPapers.R* contains the URLs to the genotypes and the phenotypes in papers.

*PreprocessNewPapers.R* contains the URLs to the papers found in a 2nd literature search.

*PreprocessDatabases.R* contains the scripts required to extract and parse the databases.

*PrepareTables.R* contains semi-manual scripts for creating merged datatables for papers.

*ExtractInfo.R* focuses on extracting genotypic information from the individual databases.

*ParseTables.R* contains scripts used to preprocess useful tables from the *AMR* package.

*Utilities.R* contains general-purpose functions that are used by the remaining scripts.

*LegacyCode.R* contains full or partial functions that are not currently used (obsolete).

*Replication.R* is a self-contained walkthrough of the pipeline on a small representative subset of sources (two papers, the PATRIC database, and the ENA antibiogram search), intended to help reviewers reproduce key outputs from scratch.

----------

The repository also contains the following data folders:

*SelectedTables/* — processed and merged tables for papers from the first literature search.

*RemainingTables/* — processed and merged tables for lower-priority papers.

*NewTables/* — supplementary input files for papers from the second literature search.

*AllExtractedGenotypes/* — one `*_Extracted_Genotypes.csv` per source, produced by `getAllGenotypes`.

*Mappings/* — EBI-resolved accession mapping files (`*_Mapped.csv`) and consolidated `AllMappings*.csv` files.

*ExtractionRecords/* — logs recording which accessions were extracted per source.

*Antibiogram search/* — raw and processed files from the ENA SRA antibiogram search (BioProject PRJNA756559).
