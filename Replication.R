### This file demonstrates the CABBAGE data curation pipeline end-to-end on a small,
### representative subset of sources. It covers:
###   - One paper from the initial literature search  (PMID 34547028, SelectedTables)
###   - One paper from RemainingTables              (PMID 34519526, RemainingTables)
###   - The PATRIC genome-AMR database
###   - The ENA/SRA antibiogram search (BioProject PRJNA756559)
###
### Running these steps in a fresh R session (after sourcing the helper scripts below)
### reproduces the corresponding files already present in IntermediateFiles/,
### RemainingTables/, PATRIC/, and "Antibiogram search/".
###

### Source order before running the rest of this file:
source("Utilities.R")
source("ExtractInfo.R")
source("PrepareTables.R")
source("PreprocessAllPapers.R")
source("PreprocessNewPapers.R")
source("PreprocessDatabases.R")

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
# Phenotype source: PLOS ONE supplementary Table S1 — pone.0249617.s001.xlsx
#   Direct download (open-access, no auth gate):
#     https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0249617.s001&type=supplementary
#   The PMC mirror (pmc.ncbi.nlm.nih.gov/articles/instance/8454963/bin/...) now
#   requires a JavaScript proof-of-work and cannot be fetched programmatically.
# Expected output: IntermediateFiles/PMID_34547028_Merged_Processed.csv (123 rows)

setwd(paste0(WORKING_DIR, "IntermediateFiles"))

BIOPROJ_34547028    <- "PRJNA736314"
ENA_PREFIX_34547028 <- "PMID_34547028_filereport_read_run_"
GENO_FILE_34547028  <- paste0(ENA_PREFIX_34547028, BIOPROJ_34547028, "_Genotype_Processed.csv")
PHENO_URL_34547028  <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0249617.s001&type=supplementary"
PHENO_XLSX_34547028 <- "PMID_34547028_pone.0249617.s001.xlsx"
PHENO_FILE_34547028 <- "PMID_34547028_pone.0249617.s001_Phenotype_Processed.csv"

### Step 1a. Download genotype data from the EBI ENA Portal API.
### downloadStudyDetails() queries the EBI read-run endpoint with all EBI_FIELDS
### and writes PMID_34547028_filereport_read_run_PRJNA736314_Details.csv.
### Equivalent manual route: visit https://www.ebi.ac.uk/ena/browser/view/PRJNA736314,
### select the Reads tab, and export the run table as a TSV file.
geno_raw_34547028 <- downloadStudyDetails(
  BIOPROJ_34547028,
  prefix        = ENA_PREFIX_34547028,
  short         = FALSE,
  writeResponse = TRUE
)

### Step 1b. Select the relevant columns, order by run accession, then
### save under the project's _Genotype_Processed.csv naming convention
### so that joinGenoPheno() can read it as the genotype side of the join.
geno_processed_34547028 <- geno_raw_34547028 %>%
  select(run_accession, study_accession, sample_accession, tax_id, scientific_name, submitted_ftp) %>%
  arrange(run_accession)
write_csv(geno_processed_34547028, GENO_FILE_34547028)

### Step 1c. Download the phenotype supplementary file and read it.
### The sheet holds isolate metadata and MIC values; the "accession number"
### column carries BioSample IDs (SAMN...) used as the join key with the genotype file.
### The PMC URL (pmc.ncbi.nlm.nih.gov/articles/instance/...) now requires a
### JavaScript proof-of-work and cannot be fetched programmatically. Use the
### PLOS ONE direct-download URL instead (no auth gate for open-access articles).
download.file(PHENO_URL_34547028, destfile = PHENO_XLSX_34547028, method = "curl", extra = "-L")
pheno_raw_34547028 <- read.xlsx(PHENO_XLSX_34547028, startRow = 3, sep.names = " ")
### Remove the unnamed columns; they contain R/S calls which we do de novo later
pheno_processed_34547028 <- pheno_raw_34547028 %>%
  as_tibble() %>%
  select(-starts_with('X')) %>%
  mutate_all(str_trim)
write_csv(pheno_processed_34547028, PHENO_FILE_34547028)

### Step 1d. Join genotype (run-level) and phenotype (isolate-level) tables.
### Both share the BioSample accession: "sample_accession" on the genotype side
### and "accession number" on the phenotype side. joinGenoPheno() performs an
### inner join and writes the result to PMID_34547028_Merged_Processed.csv.
Q_34547028 <- joinGenoPheno(
  GENO_FILE_34547028,
  PHENO_FILE_34547028,
  genoID  = "sample_accession",
  phenoID = "accession number"
)
### Console output:  "[n] IDs are common out of [n] genotypes and [n] phenotypes"


# ==============================================================================
# PART 2: RemainingTables paper — PMID 34519526
# ==============================================================================
# Carbapenem-resistant Klebsiella pneumoniae, Northwestern University (mSystems 2021).
# Genotype source : NCBI BioProject PRJNA395086 (SRA Run Selector).
# Phenotype source: mSystems supplementary Table S1 — msystems.00194-21-st001.xlsx
#   The published supplement is a PDF (PMC: pmc.ncbi.nlm.nih.gov/articles/instance/8547452/bin/
#   msystems.00194-21-st001.pdf). The Excel version in IntermediateFiles/ was obtained
#   by contacting the corresponding author directly.
# Expected output: RemainingTables/PMID_34519526_Merged_Processed.csv
#   (166 rows)
# Note: the NCBI SRA Run Table is used here instead of the EBI filereport because
# the EBI filereport does not carry the STRAIN field, which is the join key for
# this paper (STRAIN on the genotype side = "Isolate ID" on the phenotype side).

setwd(paste0(WORKING_DIR, "IntermediateFiles"))

BIOPROJ_34519526    <- "PRJNA395086"
GENO_SRA_34519526   <- "PMID_34519526_SraRunTable.txt"
GENO_FILE_34519526  <- "PMID_34519526_SraRunTable_Genotype_Processed.csv"
PHENO_XLSX_34519526 <- "PMID_34519526_msystems.00194-21-st001.xlsx"
PHENO_FILE_34519526 <- "PMID_34519526_msystems.00194-21-st001_Phenotype_Processed.csv"

### Step 2a. Load the NCBI SRA Run Table.
### This table was downloaded from the NCBI SRA Run Selector:
###   https://www.ncbi.nlm.nih.gov/Traces/study/?acc=PRJNA395086
### and saved as PMID_34519526_SraRunTable.txt. Unlike the EBI filereport used in
### Part 1, the NCBI table includes the STRAIN field needed as the join key.
### The STRAIN values use the lab prefix "NU-CRE" (Northwestern University CRE)
### while the phenotype uses the bare "CRE-" form, so a transformation is required.
geno_raw_34519526 <- read_csv(GENO_SRA_34519526, guess_max = Inf, show_col_types = FALSE)
geno_processed_34519526 <- geno_raw_34519526 %>%
  mutate(STRAIN = str_replace(STRAIN, "NU-CRE", "CRE-"))
write_csv(geno_processed_34519526, GENO_FILE_34519526)

### Step 2b. Read the phenotype supplementary file (manually obtained from mSystems).
### Column names contain spaces and units, so sep.names = " " is required.
pheno_raw_34519526 <- read.xlsx(PHENO_XLSX_34519526, sep.names = " ") %>% 
  as_tibble()
pheno_processed_34519526 <- pheno_raw_34519526 %>%
  mutate("Fold Coverage" = as.integer(round(`Fold Coverage`)))
write_csv(pheno_processed_34519526, PHENO_FILE_34519526)

### Step 2c. Join genotype and phenotype tables.
### The shared identifier is STRAIN (SRA Run Table) = "Isolate ID" (phenotype).
Q_34519526 <- joinGenoPheno(
  GENO_FILE_34519526,
  PHENO_FILE_34519526,
  genoID  = "STRAIN",
  phenoID = "Isolate ID"
)
### Console output:  "[n] IDs are common out of [n] genotypes and [n] phenotypes"


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

### Step 3b. Resolve BioSample accessions to run-level accessions via the EBI API.
### convertAccessions() queries the EBI ENA Portal for each unique biosample_accession,
### maps it to ENA run-level IDs, writes Mappings/PATRIC_genomes_AMR_Reduced_Mapped.csv
### (the accession mapping table) and PATRIC/PATRIC_genomes_AMR_Reduced_Mapped_Processed.csv
### (the full augmented table), and returns the augmented table. NOTE: This is a slow function!
patric_mapped <- convertAccessions(
  "PATRIC/PATRIC_genomes_AMR_Reduced.csv",
  cName = "biosample_accession",
  short = FALSE
)

### Step 3c. Join run-level accessions with the AMR phenotype data and consolidate.
### The logic in joinPATRIC() (PrepareTables.R, variables Qb–Qf) handles
### three sub-populations of PATRIC rows:
###   Part A — rows with a BioSample match in PATRIC_genomes_AMR_Reduced_Mapped.csv (sample_accession)
###   Part B — rows with no assembly/GenBank/RefSeq accession but a BioSample ID
###   Part C — rows with assembly, GenBank, or RefSeq accessions (used as-is)
### The three parts are concatenated and deduplicated to PATRIC/PATRIC_Merged_Processed.csv.
### Run joinPATRIC() from PrepareTables.R to execute all PATRIC joins together.
joinPATRIC()


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
  setdiff(c(NA_character_, ""))
drug_matrix <- sapply(all_drugs, function(drug) {
  ifelse(sapply(antibiogram_long$drug_list, function(lst) drug %in% lst), "R", "S")
})
antibiogram_expanded <- bind_cols(
  antibiogram_runs %>% select(-Antibiogram),
  as_tibble(drug_matrix)
)
write_csv(antibiogram_expanded,
          "PRJNA756559_SraRunTable_SplitRes_Processed.csv")
