DATABASE_DIR = paste0(WORKING_DIR, "Databases")
setwd(DATABASE_DIR)

### Traverses the two-level COMPARE_ML_AMR folder hierarchy (top-level study folders,
### each containing one subfolder per isolate with a single TSV file), reads the first
### MAX_COLS columns of each TSV, and writes one concatenated *_Processed.csv per study.
processCompareML = function(MAX_COLS = 11) {
  initDir = getwd()
  setwd("COMPARE_ML_AMR/")
  LF = list.files()
  for (folder in LF) {
    print(folder)
    Tab = tibble()
    setwd(folder)
    curLF = list.files()
    print(paste("There are", length(curLF), "sub-directories to process"))
    count = 0
    for (subfolder in curLF) {
      setwd(subfolder)
      miniLF = list.files()
      stopifnot(length(miniLF) == 1)
      curTab = read_tsv(miniLF[1], guess_max = Inf, show_col_types = FALSE)[1:MAX_COLS]
      Tab %<>%
        bind_rows(curTab)
      setwd("../")
      count %<>%
        add(1)
      if (count %% 10 == 0) { print(count) }
    }
    write_csv(Tab, paste0(str_replace(folder, "AnalysisFiles", "../AllAntibiograms"), "_Processed.csv"))
    setwd("../")
  }
  setwd(initDir)
}

### Post-processes the per-study CSVs produced by processCompareML: normalises antibiotic
### names to title case, removes any drug with conflicting (duplicate) records per isolate
### and writes a _Corrected version, widens each file to per-drug columns via
### widenResistance, and concatenates all widened files into AllAntibiograms_WidenedRes_Processed.csv.
processCompareMLFull = function() {
  LF = list.files(pattern = '.csv')
  for (fname in LF) {
    print(fname)
    Tab = read_csv(fname, guess_max = Inf, show_col_types = FALSE) %>%
      mutate_at("antibiotic_name", ~{str_to_title(.)})
    write_csv(Tab, fname)
    if (!n_distinct(Tab %>% select(bioSample_ID, antibiotic_name)) == nrow(Tab)) {
      problemAntibiotics = Tab %>%
        dplyr::group_by(bioSample_ID, antibiotic_name) %>%
        dplyr::summarise(N = dplyr::n(), .groups = "keep") %>%
        dplyr::filter(N > 1) %>%
        select(antibiotic_name) %>%
        pull() %>%
        unique()
      Tab = Tab %>%
        filter(!(antibiotic_name %in% problemAntibiotics))
      modFname = str_replace(fname, "_Processed.csv", "_Corrected_Processed.csv")
      write_csv(Tab, modFname)
    } else {
      modFname = fname
    }
    altTab = widenResistance(modFname, c(1, 2, 8), 3, c(4, 5, 6, 7, 9, 10, 11))
  }
  LFNew = list.files(pattern = 'WidenedRes')
  fullTab = tibble()
  for (fname in LFNew) {
    fullTab %<>%
      bind_rows(read_csv(fname, guess_max = Inf) %>% 
                  mutate_all(~{as.character(.)}) %>%
                  mutate(Study = str_extract(fname, 'PRJ[A-Z0-9]+\\_') %>% str_sub(end = -2))
                )
  }
  write_csv(fullTab, "AllAntibiograms_WidenedRes_Processed.csv")
  fullTab
}

### Reads each NARMS Excel file, filters to rows where both WGS.ID and
### NCBI.Accession.Number are present, and writes a CSV alongside the original Excel file.
processNARMS = function() {
  initDir = getwd()
  setwd("NARMS/")
  LF = list.files(pattern = "xlsx")
  for (ind in 1:length(LF)) {
    inFilename = LF[ind]
    Tab = read.xlsx(inFilename) %>%
      as_tibble()
    Tab = Tab %>%
      filter(!is.na(WGS.ID) & !is.na(NCBI.Accession.Number))
    write_csv(Tab, str_replace(inFilename, "xlsx", "csv"))
  }
  setwd(initDir)
}

### Collects NCBI accessions from all NARMS CSV files, adds a species column derived
### from the filename prefix, writes NARMSAccessions.csv, and returns per-species
### distinct accession counts.
processNARMSFull = function() {
  U = getAccessions("NARMS/", cName = "NCBI.Accession.Number")
  U1 = U %>% 
    mutate(species = str_split_fixed(fname, "_", n = 2)[,1])
  write_csv(U1, file = "../NARMSAccessions.csv")
  U2 = U1 %>% 
    group_by(species) %>% 
    mutate(N = n_distinct(accession)) %>% 
    slice(1) %>% 
    ungroup
  U2
}

### Reads NDARO TSV files and writes filtered CSVs, keeping only rows with both a
### Run accession and AST phenotype data. As a special step, the combined
### EscherichiaColiShigella file is first split into separate E. coli and Shigella files
### (the original combined file should then be excluded from further processing).
### Returns a named integer vector of retained row counts per species.
processNDARO = function() {
  initDir = getwd()
  setwd("NDARO")
  # Special preprocessing: separating E. coli from Shigella
  miniTab = read_tsv("EscherichiaColiShigella.3416.metadata.tsv", guess_max = Inf)
  subTab1 = miniTab %>% filter(str_starts(scientific_name, "Escherichia coli"))
  subTab2 = miniTab %>% filter(str_starts(scientific_name, "Shigella"))
  write_tsv(subTab1, "EscherichiaColi.3416.metadata.tsv")
  write_tsv(subTab2, "Shigella.3416.metadata.tsv")
  # Then either remove the file or do not consider it for further processing! 
  LF = list.files(pattern = ".tsv")
  Cnts = rep(NA, length(LF))
  for (ind in 1:length(LF)) {
    fname = LF[ind]
    shortName = fname %>% 
      str_split("\\.") %>% 
      unlist %>% 
      extract2(1) %>% 
      unlist()
    shortFname = fname %>%
      str_replace(".tsv", ".filtered.csv")
    Tab = read_tsv(fname)
    redTab = Tab %>%
      filter(AST_phenotypes != "NULL" & Run != "NULL")
    write_csv(redTab, shortFname)
    names(Cnts)[ind] = shortName
    Cnts[ind] = nrow(redTab)
  }
  setwd(initDir)
  Cnts
}

### Collects accessions from all filtered NDARO CSVs, adds a species column derived
### from the filename prefix, writes NDAROAccessions.csv, and returns per-species
### distinct accession counts.
processNDAROFull = function() {
  W = getAccessions("NDARO/")
  W1 = W %>% 
    mutate(species = str_split_fixed(fname, "\\.", n = 2)[,1])
  write_csv(W1, file = "../NDAROAccessions.csv")
  W2 = W1 %>% 
    group_by(species) %>% 
    mutate(N = n_distinct(accession)) %>% 
    slice(1) %>% 
    ungroup
  W2
}

### Joins the PATRIC AMR table (PATRIC_genomes_AMR.tsv) with the genome metadata table
### (genome_metadata.tsv), filters to organisms in GOOD_GENERA or GOOD_SPECIES, and
### writes the reduced combined file to PATRIC/PATRIC_genomes_AMR_Reduced.csv.
processPATRIC = function() {
  Tab = read_tsv("PATRIC/PATRIC_genomes_AMR.tsv", guess_max = Inf)
  TabR = Tab %>% 
    separate(genome_name, into = c("Genus", "Species", "Rest"), sep = " ", extra = "merge", remove = FALSE) %>%
    select(-Rest)
  TabR1 = TabR %>%
    filter(Genus %in% GOOD_GENERA)
  TabR2 = TabR %>%
    filter(Genus %in% str_split_fixed(GOOD_SPECIES, " ", 2)[,1] & Species %in% str_split_fixed(GOOD_SPECIES, " ", 2)[,2])
  TabR = bind_rows(TabR1, TabR2)
  TabS = read_tsv("PATRIC/genome_metadata.tsv", guess_max = Inf)
  TabS = TabS %>%
    filter(!is.na(bioproject_accession) | !is.na(biosample_accession) | !is.na(assembly_accession)
           | !is.na(genbank_accessions) | !is.na(refseq_accessions)) %>%
    select(c(1:5, 14:20, 35:51))
  TabT = inner_join(TabR, TabS)
  TabCnts = TabT %>% 
    group_by(Genus, Species) %>% 
    mutate(N = n_distinct(genome_id)) %>% 
    slice(1) %>% 
    ungroup %>%
    select(Genus, Species, N)
  write_csv(TabT, file = "PATRIC/PATRIC_genomes_AMR_Reduced.csv")
}

### Extracts assembly accessions from PATRIC_genomes_AMR_Reduced.csv (taken as the last
### space-separated token of the Rest column), deduplicates, and writes PATRICAccessions.csv.
processPATRICFull = function() {
  miniTab = read_csv("PATRIC/PATRIC_genomes_AMR_Reduced.csv")
  miniTab = miniTab %>% 
    select(1:(ncol(miniTab) - 1))
  Tab = miniTab %>% 
    mutate(Accession = str_split(Rest, " ") %>% sapply(., function(x) {x[length(x)]})) %>%
    select(Genus, Species, Accession) %>%
    distinct()
  write_csv(Tab, "PATRICAccessions.csv")
}

### For each PathogenWatch collection, joins the AMR-profile CSV and the metadata CSV
### on the NAME column and writes a *_Merged_Processed.csv. Asserts that NAME is a
### unique key in both files and that every AMR-profile name is present in the metadata.
processPWatch = function() {
  initDir = getwd()
  setwd("PathogenWatch/")
  LF = list.files()
  LFa = str_subset(LF, "amr-profile")
  LFb = str_subset(LF, "metadata")
  for (ind in 1:length(LFa)) {
    curA = LFa[ind]
    curB = LFb[ind]
    print(paste("Processing", curA))
    TabA = read_csv(curA)
    TabB = read_csv(curB)
    stopifnot(all(!duplicated(TabA$NAME)))
    stopifnot(all(!duplicated(TabB$NAME)))
    stopifnot(all(TabA$NAME %in% TabB$NAME))
    TabC = inner_join(TabA, TabB, by = "NAME")
    write_csv(TabC, str_replace(curB, "-metadata.csv", "_Merged_Processed.csv"))
  }
  setwd(initDir)
}

### Collects accessions from all PathogenWatch merged CSVs, adds a species column
### derived from the second dash-delimited token of the filename, writes
### PathogenWatchAccessions.csv, and returns per-species distinct accession counts.
processPWatchFull = function() {
  Q = getAccessions("PathogenWatch/")
  Q1 = Q %>% 
    mutate(species = str_split_fixed(fname, "-",n = 3)[,2])
  write_csv(Q1, file = "../PathogenWatchAccessions.csv")
  Q2 = Q1 %>% 
    group_by(species) %>% 
    mutate(N = n_distinct(accession)) %>% 
    slice(1) %>% 
    ungroup
  Q2
}

### Reads each PubMLST TSV file and writes a filtered CSV, retaining only rows for
### which findAccessions can identify at least one valid accession number.
processPubMLST = function() {
  initDir = getwd()
  setwd("pubMLST/")
  LF = list.files()
  for (ind in 1:length(LF)) {
    fname = LF[ind]
    Tab = read_tsv(fname, guess_max = Inf)
    acc = findAccessions(Tab)
    TabR = Tab %>%
      filter(!is.na(acc))
    write_csv(TabR, str_replace(fname, ".txt", "_filtered.csv"))
  }
  setwd(initDir)
}

### Collects accessions from all filtered PubMLST CSVs, adds a species column derived
### from the filename prefix, writes pubMLSTAccessions.csv, and returns per-species
### distinct accession counts.
processPubMLSTFull = function() {
  Z = getAccessions("pubMLST/")
  Z1 = Z %>% 
    mutate(species = str_split_fixed(fname, "_", n = 2)[,1])
  write_csv(Z1, file = "../pubMLSTAccessions.csv")
  Z2 = Z1 %>% 
    group_by(species) %>% 
    mutate(N = n_distinct(accession)) %>% 
    slice(1) %>% 
    ungroup
  Z2
}
