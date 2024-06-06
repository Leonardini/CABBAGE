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
