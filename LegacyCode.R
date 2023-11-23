parsePWFile = function(inFilename) {
  Tab = read_csv2(inFilename, col_names = TRUE, col_types = cols(.default = "c"))
  Tab %<>% 
    select(-ends_with("colour"))
  Tab %<>%
    select(-starts_with("wgs"))
  cnames = colnames(Tab) %>%
    str_to_lower()
  results = ab_from_text(cnames, collapse = NULL, type = "drug", thorough_search = FALSE)
  goodPos = which(sapply(results, function(x) { !is.na(x)} ))
  goodNames = unlist(results[goodPos])
}

parseOneFile = function(inFilename) {
  if (str_sub(inFilename, -4, -1) == ".csv") {
    Tab = read_csv(inFilename, col_names = TRUE, col_types = cols(.default = "c"))
  } else {
    Tab = read.xlsx(inFilename)
  }
  cnames = colnames(Tab) %>%
    str_to_lower()
  results = ab_from_text(cnames, collapse = NULL, type = "drug", thorough_search = FALSE)
  goodPos = which(sapply(results, function(x) { !is.na(x)} ))
  goodNames = unlist(results[goodPos])
  accName = str_locate(cnames, "accession") %>%
    as_tibble() %>%
    mutate(N = 1:length(cnames)) %>%
    filter(!is.na(start) & !is.na(end)) %>%
    pull(N)
  stopifnot(length(accName) == 1)
  subTab = Tab %>%
    select(all_of(c(accName, goodPos))) %>%
    set_colnames(c("Accession", goodNames))
  outFilename = paste0(str_sub(inFilename, 1, 14), "Processed.csv")
  write_csv(subTab, outFilename)
  subTab
}

simplifyOneFile = function(inFilename, defaultSpecies) {
  Tab = read_csv(inFilename, col_names = TRUE, col_types = cols(.default = "c")) %>%
    select(-ends_with("colour")) %>%
    select(-starts_with("wgs")) %>%
    select(-contains("silico")) %>%
    select(-contains("predict")) %>%
    select(-contains("presence"))
  cnames = colnames(Tab) %>%
    str_to_lower()
  colnames(Tab) = cnames
  candidates = cnames[str_count(cnames, " ") == 0 | str_detect(cnames, "resist") | str_detect(cnames, "mic")]
  results = ab_from_text(cnames, collapse = NULL, type = "drug", translate_ab = TRUE, thorough_search = FALSE)
  goodPos = which(sapply(results, function(x) { !is.na(x)} ))
  goodNames = unlist(results[goodPos])
  accName = str_locate(cnames, "accession") %>%
    as_tibble() %>%
    mutate(N = 1:length(cnames)) %>%
    filter(!is.na(start) & !is.na(end)) %>%
    pull(N)
  stopifnot(length(accName) == 1)
  subTab = Tab %>%
    select(all_of(c(accName, goodPos))) %>%
    set_colnames(c("Accession", goodNames))
  outFilename = paste0(str_sub(inFilename, 1, 14), "Processed.csv")
  write_csv(subTab, outFilename)
  subTab
}

preprocessTBData = function(fname = "PMID_26116186_mmc2_Processed.csv", cName = "Isolate accession number") {
  TabU = read_csv(fname, guess_max = Inf)
  altTab = read_tsv("~/Downloads/LowPriority/Pre-2020\ documents/GitHub\ repositories/DrugResistance/genotyping/metadata/reseq_phenotype_full.tsv") %>%
    select(c(2,5)) %>%
    distinct()
  joinVector = "USUBJID" %>%
    set_names(cName)
  TabU %<>% 
    left_join(altTab, by = joinVector)
  colnames(TabU)[ncol(TabU)] = "Run_accession"
  leftIDs = TabU %>% 
    filter(is.na(Run_accession)) %>%
    select(one_of(cName)) %>%
    pull()
  leftIDs  = leftIDs[!str_detect(leftIDs, ACCESSIONS["RUN"])]
  rightIDs = convertAccessionsToRuns(leftIDs)
}

CONVERSION_CMD_EBI = 'https://www.ebi.ac.uk/ena/portal/api/search?result=read_run&format=tsv&query=accession='

convertAccessionToRunEBI = function(accession = "SAMEA1317920") {
  response = read_html(paste0(CONVERSION_CMD_EBI, accession)) %>%
    xml_text() %>%
    read_tsv(guess_max = Inf, show_col_types = FALSE) %>%
    pull(run_accession)
  response
}

### This function extracts the projects corresponding to a given list of samples
### Note that this function may only work correctly for projects in SRA, not ENA
findProjectsFromSamples = function(accessionList) {
  if (!exists("SampDB")) {
    library(DBI)
    con = dbConnect(RSQLite::SQLite(), paste0(WORKING_DIR, "SRAmetadb.sqlite"))
    SampDB = dbReadTable(con, "sample")
    dbDisconnect(con)
  }
  projects = SampDB %>% 
    filter(sample_accession %in% accessionList) %>% 
    pull(xref_link) %>% 
    unique() %>%
    sort() %>%
    str_extract(pattern = ACCESSIONS["PROJECT"])
  projects
}

convertAccessionsToRuns = function(accessionList) {
  sra_con = dbConnect(dbDriver("SQLite"), paste0(WORKING_DIR, "SRAmetadb.sqlite"))
  Tab = sraConvert(in_acc = accessionList, out_type = "run", sra_con = sra_con)
  dbDisconnect(sra_con)
  Tab
}