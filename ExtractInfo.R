### Batch-runs getGenotypes on every *_Processed.csv file in Dir, writing one
### *_Extracted_Genotypes.csv per file into AllExtractedGenotypes/. Files originating
### from a database directory (not starting with PMID or DOI) receive a directory-name
### prefix in the output filename to avoid collisions across sources.
getAllGenotypes = function(Dir) {
  initDir = getwd()
  setwd(Dir)
  LF = list.files(pattern = "Processed.csv")
  for (fname in LF) {
    print(fname)
    cName = NULL
    PMID = str_split_fixed(fname, "_", n = 3)[,2]
    if (PMID == "32903472") { ### A special case
      cName = "pubMLST id"
    }
    prefix = ifelse(str_starts(fname, "PMID") || str_starts(fname, "DOI"), "", 
                    paste0(str_sub(str_extract(getwd(), "/[A-Za-z]*$"), 2), "_"))
    outFilename = paste0(WORKING_DIR, "AllExtractedGenotypes/",  prefix, 
                         str_replace(fname, "Processed.csv", "Extracted_Genotypes.csv"))
    curTab = getGenotypes(fname, cName = cName, types = c("RUN", "CONTIG", "ASSEMBLY"), full = FALSE, 
                          badTypes = c("SAMPLE","BIOSAMPLE"), writeResult = TRUE, outFilename = outFilename)
  }
  setwd(initDir)
}

### Reads all extracted genotype files from AllExtractedGenotypes/ (produced by
### getAllGenotypes) and additionally processes the three large mixed-species database
### files (PATRIC, CDC, COMPARE-ML), filtering those to GOOD_GENERA and GOOD_SPECIES.
### Returns a named list: fullList (accession IDs with source file) for the per-species
### files, and extraList (accession IDs with source file and species label) for the
### database files.
countGenotypes = function() {
  setwd(paste0(WORKING_DIR, "AllExtractedGenotypes/"))
  LF = list.files()
  fullList = tibble()
  for (fname in LF) {
    print(fname)
    curTab = read_csv(fname, guess_max = Inf, show_col_types = FALSE) %>%
      mutate_all(as.character)
    if (!("Accession" %in% colnames(curTab))) {
      print(paste("Warning:", fname, "does not have the appropriate column!"))
    } else {
      fullList %<>%
        bind_rows(tibble(ID = curTab$Accession, file = fname))
    }
  }
  setwd(paste0(WORKING_DIR, "Databases"))
  extraList = tibble()
  mixedFiles = tibble(fn = c("PATRIC_Merged_Processed.csv", "CDC_Mapped_Processed.csv", "COMPARE_ML_AMR_WidenedRes_Merged_Processed.csv"),
                      relCol = c("Genus", "OrganismName", "species"), genCol = c("biosample_accession", NA, NA))
  for (ind in 1:nrow(mixedFiles)) {
    curRow = mixedFiles %>% slice(ind)
    curTab = read_csv(curRow$fn, guess_max = Inf, show_col_types = FALSE)
    relevantColumn = curTab %>%
      select(one_of(curRow$relCol)) %>%
      as_vector()
    firstWord = tolower(str_split_fixed(relevantColumn, " ", n = 2)[,1])
    if (ind == 1) {
      useVector = (!is.na(match(firstWord, tolower(GOOD_GENERA))) | !is.na(match(firstWord, tolower(str_split_fixed(GOOD_SPECIES, " ", n = 2)[,1]))))
      curIDs = getGenotypes(curRow$fn, cName = curRow$genCol, types = c("RUN", "CONTIG", "ASSEMBLY"), full = FALSE, badTypes = c("SAMPLE","BIOSAMPLE"), writeResult = FALSE)
    } else {
      firstTwoWords = tolower(apply(str_split_fixed(relevantColumn, " ", n = 3)[, 1:2], 1, function(x) {paste(x, collapse = " ")}))
      useVector = (!is.na(match(firstWord, tolower(GOOD_GENERA))) | !is.na(match(firstTwoWords, tolower(GOOD_SPECIES))))
      curIDs = getGenotypes(curRow$fn, cName = NULL, types = c("RUN", "CONTIG", "ASSEMBLY"), full = FALSE, badTypes = c("SAMPLE","BIOSAMPLE"), writeResult = FALSE)
    }
    extraList %<>%
      bind_rows(tibble(ID = curIDs$Accession[useVector], file = fname, Species = relevantColumn[useVector]))
  }
  extraList %<>%
    filter(!is.na(ID))
  setwd(WORKING_DIR)
  output = list(fullList = fullList, extraList = extraList)
  output
}

### Infers a species label for each accession in fullList by matching its source
### filename against GOOD_GENERA and GOOD_SPECIES (both full genus and abbreviated
### genus+species forms). For accessions that remain unlabelled, falls back to
### FinalDatasets.csv PMID-to-species mappings, but only for PMIDs with a single
### associated species to avoid ambiguous assignments. Writes the labelled table to
### ExtractedGenotypesWithSpecies.csv and returns it.
breakUpGenotypesBySpecies = function(fullList, extraList) {
  fullList %<>%
    mutate(Species = NA)
  for (ind in 1:length(GOOD_GENERA)) {
    curGenus = GOOD_GENERA[ind]
    fullList %<>%
      mutate_at("Species", ~{ifelse(str_detect(file, curGenus), curGenus, .)})
  }
  splitSpecies = GOOD_SPECIES %>%
    str_split_fixed(" ", n = 2) %>%
    set_colnames(c("genus", "species")) %>%
    as_tibble() %>%
    rowid_to_column("index")
  for (ind in 1:nrow(splitSpecies)) {
    curGenus = splitSpecies %>%
      slice(ind) %>%
      pull(genus) %>%
      tolower()
    fullList %<>%
      mutate_at("Species", ~{ifelse(is.na(.) & str_detect(tolower(file), curGenus), GOOD_SPECIES[ind], .)})
  }
  for (ind in 1:nrow(splitSpecies)) {
    curSpeciesShort = splitSpecies %>%
      slice(ind) %>%
      mutate(shortSpecies = paste0(str_sub(genus, 1, 1), species)) %>%
      pull(shortSpecies) %>%
      tolower()
    fullList %<>%
      mutate_at("Species", ~{ifelse(is.na(.) & str_detect(tolower(file), curSpeciesShort), GOOD_SPECIES[ind], .)})
  }
  miniTab = read_csv("FinalDatasets.csv") %>%
    mutate_at("Species", ~{str_remove_all(., "spp.")})
  miniTab %<>%
    mutate(species = GOOD_SPECIES[match(Species, splitSpecies %>% mutate(shortSpecies = paste0(str_sub(genus, 1, 1), ". ", species)) %>% pull(shortSpecies))]) %>%
    mutate_at("Species", ~{case_match(., "S. suis" ~ "Streptococcus suis", "S. typhi" ~ "Salmonella", "C. jejuni" ~ "Campylobacter", "S. enterica" ~ "Salmonella", .default = .)})
  miniTab %<>%
    mutate_at("Species", ~{ifelse(!is.na(species), species, .)}) %>%
    select(-species)
  # NOTE: can only safely impute the species when there is only one for a PMID!
  miniMap = miniTab %>%
    select(PMID, Species) %>%
    group_by(PMID) %>%
    mutate(N = n()) %>%
    filter(N == 1) %>%
    ungroup()
  for (index in 1:nrow(miniMap)) {
    curRow = miniMap %>% slice(index)
    fullList %<>%
      mutate_at("Species", ~{ifelse(is.na(.) & (str_detect(file, paste0("PMID_", curRow$PMID)) | str_detect(file, paste0("DOI_", str_replace(curRow$PMID, "/", ":")))), curRow$Species, .)})
  }
  fullList %<>%
    mutate_at("Species", ~{ifelse(is.na(.) & str_detect(file, "styphi"), "Salmonella", .)}) %>%
    mutate_at("Species", ~{ifelse(is.na(.) & str_detect(file, "klepn"), "Klebsiella pneumoniae", .)})
  write_csv(fullList, file = "ExtractedGenotypesWithSpecies.csv")
  fullList
}

### Orchestrates the full genotype-counting pipeline: calls countGenotypes, passes the
### result to breakUpGenotypesBySpecies, combines fullList and extraList, normalises
### species names to sentence case and canonical GOOD_GENERA / GOOD_SPECIES values,
### and writes the final combined table to AllGenotypesAndSpecies.csv.
tabulateGenotypesAndSpecies = function() {
  output = countGenotypes()
  fullList = output[[1]]
  extraList = output[[2]]
  fullList = breakUpGenotypesBySpecies(fullList)
  finalList = bind_rows(fullList, extraList)
  finalList %<>% mutate_at("Species", str_to_sentence)
  for (ind in 1:length(GOOD_GENERA)) {
    finalList %<>% 
      mutate_at("Species", ~{ifelse(str_starts(., GOOD_GENERA[ind]), GOOD_GENERA[ind], .)})
  }
  splitSpecies = str_split_fixed(GOOD_SPECIES, " ", n = 2)
  for (ind in 1:length(GOOD_SPECIES)) {
    finalList %<>% 
      mutate_at("Species", ~{ifelse(str_starts(., splitSpecies[ind, 1]), GOOD_SPECIES[ind], .)})
  }
  write_csv(finalList, file = "AllGenotypesAndSpecies.csv")
  finalList
}

### Reads AllGenotypesAndSpecies.csv and generates a stacked bar chart (Figure 2)
### of unique sample counts per species, coloured by source: "DB" (databases only),
### "Paper" (literature only), or "Both" (present in both). Total counts are
### annotated above each bar. The chart is written to fname as a PDF.
makeFigure = function(fname = "Figure2.pdf") {
  Tab1 = read_csv("AllGenotypesAndSpecies.csv", show_col_types = FALSE) %>%
    mutate(DB = !(str_detect(file, "PMID") | str_detect(file, "doi"))) %>%
    group_by(ID) %>% 
    distinct(ID, DB, .keep_all = TRUE) %>% 
    ungroup() %>%
    group_by(ID) %>% 
    mutate(type = ifelse(all(DB), "DB", ifelse(all(!DB), "Paper", "Both"))) %>%
    ungroup()
  Tab2 = Tab1 %>% 
    group_by(Species, type) %>% 
    mutate(N = n_distinct(ID)) %>% 
    slice(1) %>% 
    ungroup() %>%
    mutate(species = ifelse(str_detect(Species, " "), paste0(str_sub(Species, 1, 1), ". ", str_split_fixed(Species, " ", n = 2)[,2]), Species)) %>%
    rename(source = type)
  totals = Tab2 %>%
    group_by(species) %>%
    summarise(total = sum(N))
  pdf(fname)
  ggplot(data = Tab2, aes(x = species, y = N, fill = source)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
    geom_text(aes(species, total + 1000, label = total, fill = NULL), data = totals, size = 2) +
    ggtitle("Number of samples by species and source") + 
    theme(plot.title = element_text(hjust = 0.5))
  dev.off()
  Tab1
}

### Like getAllGenotypes, but extracts RUN, SAMPLE, and BIOSAMPLE accessions
### simultaneously (full = TRUE, badTypes = NULL) for every *_Processed.csv in Dir.
### All per-file results are combined into a single mapping table and written to
### Mappings/AllMappings<DirName>.csv, where DirName is the last component of Dir.
getAllMappings = function(Dir) {
  initDir = getwd()
  setwd(Dir)
  LF = list.files(pattern = "Processed.csv")
  mapTab = tibble() 
  for (fname in LF) {
    print(fname)
    cName = NULL
    curTab = getGenotypes(fname, cName = NULL, types = c("RUN", "SAMPLE", "BIOSAMPLE"), full = TRUE, 
                        badTypes = NULL, writeResult = FALSE) %>%
      mutate(file = fname) %>%
      mutate_all(~{as.character(.)})
    mapTab %<>%
      bind_rows(curTab)
  }
  outFile = paste0(WORKING_DIR, "Mappings/AllMappings", str_sub(str_extract(getwd(), "/[A-Za-z]*$"), 2), ".csv")
  write_csv(mapTab, outFile)
  setwd(initDir)
  mapTab
}
