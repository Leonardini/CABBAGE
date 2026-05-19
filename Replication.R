### This file demonstrates the CABBAGE data curation pipeline end-to-end on a small,
### representative subset of sources. It covers:
###   - One paper from the initial literature search  (PMID 34547028, SelectedTables)
###   - One paper from the new literature search       (PMID 35876577, NewTables)
###   - The PATRIC genome-AMR database
###   - The ENA/SRA antibiogram search (BioProject PRJNA756559)
###
### Running these steps in a fresh R session (after sourcing the helper scripts below)
### reproduces the corresponding files already present in IntermediateFiles/,
### SelectedTables/, PATRIC/, and "Antibiogram search/".
###
### Source order before running this file:
###   source("Utilities.R")
###   source("ExtractInfo.R")
###   source("PrepareTables.R")
###   source("PreprocessAllPapers.R")
###   source("PreprocessNewPapers.R")
###   source("PreprocessDatabases.R")

library(magrittr)
library(tidyverse)
library(openxlsx)


# ==============================================================================
# PART 1: Initial-search paper — PMID 34547028
# ==============================================================================
# Vagdatli et al. (2021), PLOS ONE 16(9):e0249617.
# Species: Salmonella enterica subsp. enterica serovar Dublin.
# Discovered in the first literature search (AllFoundPapers, row 27 in miniRes).
# Genotype source : NCBI BioProject PRJNA736314 (ENA Portal run table).
# Phenotype source: PMC supplementary Table S1 — pone.0249617.s001.xlsx
#   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8454963/bin/pone.0249617.s001.xlsx
# Expected output: IntermediateFiles/PMID_34547028_Merged_Processed.csv (123 rows)

setwd(paste0(WORKING_DIR, "IntermediateFiles"))

### Step 1a. Download genotype data from the EBI ENA Portal API.
### downloadStudyDetails() queries the EBI read-run endpoint with all EBI_FIELDS
### and writes PMID_34547028_filereport_read_run_PRJNA736314_Details.csv.
### Equivalent manual route: visit https://www.ebi.ac.uk/ena/browser/view/PRJNA736314,
### select the Reads tab, and export the run table as a TSV file.
geno_raw_34547028 <- downloadStudyDetails(
  "PRJNA736314",
  prefix      = "PMID_34547028_filereport_read_run_",
  short       = FALSE,
  writeResponse = TRUE
)

### Step 1b. Save under the project's _Genotype_Processed.csv naming convention
### so that joinGenoPheno() can read it as the genotype side of the join.
write_csv(geno_raw_34547028,
          "PMID_34547028_filereport_read_run_PRJNA736314_Genotype_Processed.csv")

### Step 1c. Download the phenotype supplementary file manually (URL above), save as
### PMID_34547028_pone.0249617.s001.xlsx in IntermediateFiles/, then read and write
### as a CSV. The sheet holds isolate metadata and MIC values; the "accession number"
### column carries BioSample IDs (SAMN...) used as the join key with the genotype file.
pheno_raw_34547028 <- read.xlsx(
  "PMID_34547028_pone.0249617.s001.xlsx"
) %>% as_tibble()
write_csv(pheno_raw_34547028,
          "PMID_34547028_pone.0249617.s001_Phenotype_Processed.csv")

### Step 1d. Extract run-level genotype accessions from the ENA filereport.
### getGenotypes() scans all character columns for ENA/NCBI accession patterns
### (preferring RUN > ASSEMBLY > CONTIG) and writes the extracted accessions to
### AllExtractedGenotypes/PMID_34547028_filereport_read_run_PRJNA736314_Extracted_Genotypes.csv
getGenotypes(
  "PMID_34547028_filereport_read_run_PRJNA736314_Genotype_Processed.csv"
)

### Step 1e. Join genotype (run-level) and phenotype (isolate-level) tables.
### Both share the BioSample accession: "sample_accession" on the genotype side
### and "accession number" on the phenotype side. joinGenoPheno() performs an
### inner join and writes the result to PMID_34547028_Merged_Processed.csv.
Q_34547028 <- joinGenoPheno(
  "PMID_34547028_filereport_read_run_PRJNA736314_Genotype_Processed.csv",
  "PMID_34547028_pone.0249617.s001_Phenotype_Processed.csv",
  genoID  = "sample_accession",
  phenoID = "accession number"
)
### Console output:  "[n] IDs are common out of [n] genotypes and [n] phenotypes"


# ==============================================================================
# PART 2: New-search paper — PMID 35876577
# ==============================================================================
# Gebre et al. (2022), Frontiers in Microbiology.
# Species: Klebsiella pneumoniae, clinical isolates from Ethiopia.
# Discovered in the second literature search (NewlyFoundPapers).
# Genotype source : NCBI BioProject PRJNA787062 (SRA Run Selector).
# Phenotype source: supplementary Table 2 (to be downloaded manually).
# Raw files are kept in NewTables/; processing follows exactly the same
# steps as Part 1 once the phenotype file has been obtained.

setwd(paste0(WORKING_DIR, "NewTables"))

### Step 2a. The SRA Run Table was downloaded from NCBI's SRA Run Selector:
###   https://www.ncbi.nlm.nih.gov/Traces/study/?acc=PRJNA787062
### and saved as PMID_35876577_SraRunTable.txt. Read it and write it under the
### _Genotype_Processed.csv naming convention.
geno_raw_35876577 <- read_csv(
  "PMID_35876577_SraRunTable.txt",
  guess_max = Inf,
  show_col_types = FALSE
)
write_csv(geno_raw_35876577,
          "PMID_35876577_SraRunTable_Genotype_Processed.csv")

### Step 2b. Download the phenotype supplementary file manually from the journal
### (URL recorded in NewlyFoundPapers.csv for this entry), save as
### PMID_35876577_<supplement_name>.xlsx in NewTables/, then preprocess:
###
###   pheno_raw_35876577 <- read.xlsx("PMID_35876577_<supplement_name>.xlsx") %>%
###     as_tibble()
###   write_csv(pheno_raw_35876577, "PMID_35876577_<supplement_name>_Phenotype_Processed.csv")
###
### Steps 2c–2d (getGenotypes, joinGenoPheno) are then identical to Part 1,
### Steps 1d–1e, substituting the correct column names for the join key.
### The SRA run table column "BioSample" or "SRA Study" typically serves as the
### shared identifier between genotype and phenotype files.


# ==============================================================================
# PART 3: PATRIC database
# ==============================================================================
# PATRIC (now BV-BRC) is a curated repository of bacterial genome and AMR data.
# Raw files are already present in PATRIC/:
#   PATRIC_genomes_AMR.tsv  — per-genome AMR test results (measurement sign/value)
#   genome_metadata.tsv     — genome-level metadata (biosample, assembly accessions)
# The PATRIC download page is https://www.bv-brc.org/view/DataType/amr

setwd(paste0(WORKING_DIR, "Databases"))

### Step 3a. Filter to WHO Priority Pathogens and join AMR table with metadata.
### processPATRIC() keeps rows whose genus is in GOOD_GENERA or whose
### genus+species pair is in GOOD_SPECIES, inner-joins with genome_metadata.tsv
### on genome_id, and writes PATRIC/PATRIC_genomes_AMR_Reduced.csv.
processPATRIC()

### Step 3b. Extract assembly accessions for EBI resolution.
### processPATRICFull() parses the last space-delimited token of the genome_name
### column as an assembly accession (GCA_/GCF_...) and writes PATRICAccessions.csv.
processPATRICFull()

### Step 3c. Resolve assembly accessions to run-level accessions via the EBI API.
### convertAccessions() queries the EBI ENA Portal for each unique accession,
### maps it to ENA run-level IDs, writes Mappings/PATRICMappings.csv alongside the
### original file, and returns the augmented table.
patric_mapped <- convertAccessions(
  "PATRIC/PATRIC_genomes_AMR_Reduced.csv",
  cName = "assembly_accession",
  short = FALSE
)

### Step 3d. Join run-level accessions with the AMR phenotype data and consolidate.
### The logic in joinAllGenoPheno() (PrepareTables.R, variables Qb–Qf) handles
### three sub-populations of PATRIC rows:
###   Part A — rows with a BioSample match in PATRICMappings.csv (biosample_accession)
###   Part B — rows with no assembly/GenBank/RefSeq accession but a BioSample ID
###   Part C — rows with assembly, GenBank, or RefSeq accessions (used as-is)
### The three parts are concatenated and deduplicated to PATRIC/PATRIC_Merged_Processed.csv.
### Run joinAllGenoPheno() from PrepareTables.R to execute all PATRIC joins together
### with the paper joins:
###   joinAllGenoPheno()


# ==============================================================================
# PART 4: Antibiogram search (ENA SRA analyses, BioProject PRJNA756559)
# ==============================================================================
# Some studies deposit antibiogram results as ENA analysis objects (accession type
# ERZ...) of type AMR_ANTIBIOGRAM alongside the corresponding sequencing runs.
# The ENA Portal search for analyses of type AMR_ANTIBIOGRAM
# (https://www.ebi.ac.uk/ena/browser/search — filter: Analysis type = AMR_ANTIBIOGRAM)
# was downloaded on 2022-11-19 and saved as
#   "Antibiogram search/Antibiograms_ena_sra-analysis_20221119-2217.csv"
# This file lists ERZ accessions with a description of the corresponding sample.
# From those studies, BioProject PRJNA756559 (Canadian NML Antimicrobial Resistance
# Surveillance Program) was selected as a representative large dataset.

setwd(paste0(WORKING_DIR, "Antibiogram search"))

### Step 4a. Inspect the ENA antibiogram analysis list.
### Each row corresponds to one ENA analysis object (ERZ...) that contains the
### antibiogram for a single isolate belonging to a study on NCBI/ENA.
antibiogram_enas <- read_csv(
  "Antibiograms_ena_sra-analysis_20221119-2217.csv",
  guess_max = Inf,
  show_col_types = FALSE
)
### Columns: accession (ERZ...), description ("Antibiogram for study X, sample Y")

### Step 4b. Load the SRA Run Table for PRJNA756559.
### This table was downloaded from the NCBI SRA Run Selector and saved as
### PRJNA756559_SraRunTable.csv, then passed through getGenotypes() to produce
### PRJNA756559_SraRunTable_Genotype_Processed.csv.
### Unlike the paper workflows above, this source embeds the AMR phenotype directly
### in the SRA metadata: the "Antibiogram" column contains a hyphen-separated list
### of drug abbreviations for which the isolate was resistant
### (e.g. "AMC-CRO-DOX-GEN-TGC-TOB" = resistant to all six drugs).
antibiogram_runs <- read_csv(
  "PRJNA756559_SraRunTable_Genotype_Processed.csv",
  guess_max = Inf,
  show_col_types = FALSE
)

### Step 4c. Expand the Antibiogram column into per-drug binary R/S columns.
### The separator is a hyphen rather than a comma, so the column is split
### accordingly before applying the same R/S logic used by splitResistance():
antibiogram_long <- antibiogram_runs %>%
  mutate(drug_list = str_split(Antibiogram, "-"))
all_drugs <- antibiogram_long$drug_list %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  setdiff(NA_character_)
drug_matrix <- sapply(all_drugs, function(drug) {
  ifelse(sapply(antibiogram_long$drug_list, function(lst) drug %in% lst), "R", "S")
})
antibiogram_expanded <- bind_cols(
  antibiogram_runs %>% select(-Antibiogram),
  as_tibble(drug_matrix)
)
write_csv(antibiogram_expanded,
          "PRJNA756559_SraRunTable_SplitRes_Processed.csv")
### The Run column (SRR...) serves as the genotype accession.
### The per-drug columns encode the phenotype.
### getGenotypes("PRJNA756559_SraRunTable_Genotype_Processed.csv") extracts
### the run accessions for inclusion in AllExtractedGenotypes/.
getGenotypes("PRJNA756559_SraRunTable_Genotype_Processed.csv")
