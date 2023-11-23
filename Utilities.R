library(magrittr)
library(tidyverse)
library(openxlsx)
library(AMR)
library(ggplot2)
library(xml2)
library(rvest)
library(SRAdb)

PMID_prefix = "www.ncbi.nlm.nih.gov/pubmed/?term="
NCBI_prefix = "https://www.ncbi.nlm.nih.gov/bioproject/?term="

### Regular expressions based on https://ena-docs.readthedocs.io/en/latest/submit/general-guide/accessions.html
ACCESSIONS = c(
  PROJECT      = "^PRJ(E|D|N)[A-Z][0-9]+",
  STUDY        = "^(E|D|S)RP[0-9]{6,}",
  BIOSAMPLE    = "^SAM(D|N|EA)[0-9]+",
  SAMPLE       = "^(E|D|S)RS[0-9]{6,}",
  EXPERIMENT   = "^(E|D|S)RX[0-9]{6,}",
  RUN          = "^(E|D|S)RR[0-9]{6,}",
  ANALYSIS     = "^(E|D|S)RZ[0-9]{6,}",
  ASSEMBLY     = "^GC(A|F)[\\_][0-9]{9}[\\.]?[0-9]*",
  PROTEIN      = "^[A-Z]{3}[0-9]{5,7}[\\.][0-9]+",
  SCAFFOLD     = "^[A-Z]{1,2}[0-9]{5,6}[\\.]?[0-9]*",
  CONTIG       = "^[A-Z]{4,6}[0-9]{2}[S]?[0-9]{6,9}"
)

MAP_DB         = c(D = "DDBJ", E = "ENA", N = "NCBI", S = "NCBI")
GOOD_GENERA    = c("Campylobacter", "Enterobacter", "Morganella", "Proteus", 
                 "Providencia", "Serratia", "Salmonella", "Shigella")
GOOD_SPECIES   = c("Acinetobacter baumannii", "Clostridioides difficile", "Enterococcus faecium", 
                 "Escherichia coli", "Haemophilus influenzae", "Helicobacter pylori", 
                 "Klebsiella pneumoniae", "Mycobacterium tuberculosis", "Neisseria gonorrhoeae", 
                 "Pseudomonas aeruginosa", "Staphylococcus aureus", "Streptococcus pneumoniae")
MIN_FRAC = 0.05 # Fraction of non-missing entries that are accessions for a column to be an accession column

EBI_FIELDS = c("study_accession", "sample_accession", "secondary_sample_accession", "experiment_accession", 
                    "run_accession", "tax_id", "scientific_name", "fastq_ftp", "submitted_ftp", "sra_ftp")
EBI_FIELD = EBI_FIELDS[5]
QUERY_EBI            = 'https://www.ebi.ac.uk/ena/portal/api/filereport?accession='
EBI_PRE_FIELDS       = '&result=read_run&fields='
EBI_POST_FIELDS      = '&format=tsv&download=false&limit=0'
CONVERSION_EBI_SHORT = paste0(EBI_PRE_FIELDS, EBI_FIELD, EBI_POST_FIELDS)
CONVERSION_EBI_LONG  = paste0(EBI_PRE_FIELDS, paste0(EBI_FIELDS, collapse=","), EBI_POST_FIELDS)

### Information to be captured; in each category, elements 1-2 are required, element 3 is derived from element 2
### and is also required, and the remaining elements are optional and should only be included if available.
FIELDS = c(
  GENOTYPE  = list(c("Database", "Accession"    , "Type")),
  PHENOTYPE = list(c("Status"  , "Antimicrobial", "Class", "MICValue", "MICUnit", "MICSign", "Method", "Platform")),
  METADATA  = list(c("Species" , "PubMedID"     , "Title", "Source", "Location", "CollectionDate", "Host", "Comments"))
)

WORKING_DIR = "~/Downloads/AMR/AMRDataTables/"

str_reverse = function(x) {
  str_split(x, "") %>% 
    unlist %>% 
    rev %>% 
    paste0(collapse = "")
}

### This function tries to extract the genotype information from a formatted csv file
### If writeResult is TRUE the results are written out to a file whose name is given by outFilename
### See the findAccessions function below for full details on the meaning of the other parameters
getGenotypes = function(fname, cName = NULL, types = c("RUN", "CONTIG", "ASSEMBLY"), full = FALSE, 
                        badTypes = c("SAMPLE","BIOSAMPLE"), writeResult = TRUE, outFilename = NULL) {
  Tab = read_csv(fname, guess_max = Inf, show_col_types = FALSE)
  TabG = findAccessions(Tab, cName = cName, types = types, full = full, badTypes = badTypes) %>%
    filter(!is.na(Accession))
  print(paste("Found accessions for", nrow(TabG), "out of", nrow(Tab), "entries"))
  if (!is.null(cName)) {
    TabG %<>%
      mutate(Database =  str_trim(str_remove(cName, "id")))
  } else {
    TabG %<>%
      mutate(Database = as.vector(MAP_DB[str_sub(Accession,1,1)]))
  }
  if (writeResult) {
    if (is.null(outFilename)) { outFilename = str_replace(fname, "Processed", "Extracted_Genotypes") }
    write_csv(TabG %>% select(N, Database, Accession, Type), outFilename)
  }
  TabG
}

### This function tries to extract the accession numbers from a formatted table;
### types is a list specifying the accession types that are allowed to be matched.
### This list is a sub-list of the names of the ACCESSIONS variable defined above;
### its order specifies how the found accession identifiers will be prioritised.
### The function extracts all potential accessions for each row, and then returns
### the highest-priority one for each row, along with the type that matches it.
### minFraction specifies the minimum fraction of column content (non-NA values) 
### that needs to match one of the accession patterns in order to be considered.
### If cName is specified, the named column is extracted as accession; its name
### is then used as a type. NB: row indices are also returned for compatibility.
### If full is true, the complete set of accessions is returned for each entry!
findAccessions = function(Table, cName = NULL, types = "RUN", minFrac = MIN_FRAC, full = FALSE, badTypes = NULL) {
  cNames = colnames(Table)
  M = nrow(Table)
  if (!is.null(cName)) {
    position = match(cName, cNames)
    stopifnot(!is.na(position))
    matchedCol = Table %>%
      pull(position)
    output = tibble(N = 1:M, Accession = matchedCol, Type = cName)
    return(output)
  }
  L = length(types)
  goodCols = which(sapply(Table, class) == "character")
  foundAccessions = matrix(NA, M, L) %>%
    set_colnames(types)
  for (ind in goodCols) {
    curCol = Table %>% 
      pull(var = ind)
    curDetect = rep(0, M)
    for (index in 1:M) {
      curVal = curCol[index]
      if (!is.na(curVal) && !any(str_detect(curVal, ACCESSIONS[badTypes]))) {
        curFound = str_detect(curVal, ACCESSIONS[types])
        if (any(!is.na(curFound)) && any(curFound)) {
          curDetect[index] = which(curFound)[1]
        }
      }
    }
    if (sum(curDetect > 0) >= minFrac * sum(!is.na(curCol))) {
      for (pos in 1:L) {
        curType = types[pos]
        curBest = which(curDetect == pos)
        if (length(curBest) > 0) {
          foundAccessions[curBest, pos] = str_extract(curCol[curBest], ACCESSIONS[[curType]])
        }
      }
    }
  }
  if (full) {
    output = foundAccessions %>%
      as_tibble() %>%
      mutate(N = 1:M) ### %>% pivot_longer() - later on, ensure that each non-missing accession has its own row!
    colnames(output) = c("Accession", colnames(output)[-1])
  } else {
    output = tibble(pos = apply(foundAccessions, 1, function(x) { which(!is.na(x))[1] })) %>%
      mutate(N = 1:M, Accession = foundAccessions[cbind(1:M, pos)], Type = types[pos]) %>%
      select(-pos)
  }
  output
}

getAccessions = function(Dir, cName = NULL) {
  initDir = getwd()
  setwd(Dir)
  LF = list.files(pattern = ".csv")
  fullTab = tibble(fname = character(), accession = character())
  for (ind in 1:length(LF)) {
    curFname = LF[ind]
    Tab = read_csv(curFname, guess_max = Inf)
    curAccessions = findAccessions(Tab, cName = cName)
    fullTab %<>% 
      bind_rows(tibble(fname = curFname, accession = curAccessions))
  }
  setwd(initDir)
  fullTab
}

joinAllAccessions = function() {
  X = getAccessions(DATABASE_DIR, cName = "accession")
  X1 = X %>% 
    mutate(DB = str_remove_all(fname, "Accessions.csv"))
  write_csv(X1, file = "AllAccessions.csv")
  X2 = n_distinct(X1$accession)
  X2
}

concatenateFiles = function(inFilenames, outFilename) {
  Tab = read_csv(inFilenames[1], guess_max = Inf)
  cnames = colnames(Tab)
  ctypes = sapply(Tab, class)
  for (fname in inFilenames[-1]) {
    nextTab = read_csv(fname, guess_max = Inf)
    nextNames = colnames(nextTab)
    nextTypes = sapply(nextTab, class)
    for (index in 1:ncol(nextTab)) {
      cname = nextNames[index]
      curMatch = match(cname, cnames)
      if (!is.na(curMatch) && nextTypes[[index]] != ctypes[[curMatch]]) {
        curFun = paste0("as.", ctypes[curMatch])
        nextTab %<>%
          mutate_at(cname, ~{exec(curFun, .)})
      }
    }
    Tab %<>%
      bind_rows(nextTab)
  }
  write_csv(Tab, outFilename)
  Tab
}

joinGenoPheno = function(genoFilename, phenoFilename, genoID, phenoID, defaultPheno = NULL, outFilename = NULL) {
  gList = c("c") %>%
    set_names(genoID)
  TabG = read_csv(genoFilename, guess_max = Inf, col_types = gList)
  pList = c("c") %>%
    set_names(phenoID)
  TabP = read_csv(phenoFilename, guess_max = Inf, col_types = pList)
  GID = TabG %>%
    select(any_of(genoID)) %>%
    pull(1)
  PID = TabP %>%
    select(any_of(phenoID)) %>%
    pull(1)
  common = intersect(GID, PID)
  print(paste(length(common), "IDs are common out of", length(GID), "genotypes and", length(PID), "phenotypes"))
  colnG = colnames(TabG)
  colnG[colnG == genoID] = phenoID
  colnames(TabG) = colnG
  if (is.null(defaultPheno)) {
    fullTab = inner_join(TabG, TabP, by = phenoID)
  } else {
    fullTab = left_join(TabG, TabP, by = phenoID)
    fillIDs = setdiff(GID, common)
    fillPos = fullTab %>%
      select(any_of(phenoID)) %>%
      pull()
    fillPos = match(fillIDs, fillPos)
    fullTab[fillPos, ncol(fullTab)] = defaultPheno
  }
  if (is.null(outFilename)) {
    outFilename = paste0(str_sub(genoFilename, 1, 14), "Merged_Processed.csv")
  }
  write_csv(fullTab, outFilename)
  fullTab
}

splitResistance = function(fname, Col = "Resistance profile", default = "susceptible", extraSymbol = NULL) {
  Tab = read_csv(fname, guess_max = Inf)
  splitCol = Tab %>%
    select(one_of(Col)) %>%
    pull()
  if (!is.null(extraSymbol)) {
    splitCol %<>%
      str_remove_all(extraSymbol)
  }
  separatedCol = splitCol %>%
    str_split(",") %>%
    lapply(function(x) {str_trim(x)})
  uniqueEntries = separatedCol %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    setdiff(default)
  Tab %<>%
    select(-one_of(Col))
  extraTab = matrix("S", nrow(Tab), length(uniqueEntries), dimnames = list(c(), uniqueEntries))
  for (ind in 1:nrow(Tab)) {
    extraTab[ind, intersect(separatedCol[[ind]], uniqueEntries)] = "R"
  }
  extraTab %<>%
    as_tibble()
  Tab %<>% 
    bind_cols(extraTab)
  write_csv(Tab, str_replace(fname, "_Processed.csv", "_SplitRes_Processed.csv"))
  Tab
}

widenResistance = function(fname, repColumns = c(1,2), pivotColumn = 3, splitColumns = 4:8, dropColumns = TRUE) {
  Tab = read_csv(fname, guess_max = Inf)
  cnames = colnames(Tab)
  repCnames = cnames[repColumns]
  pivotCname = cnames[pivotColumn]
  splitCnames = cnames[splitColumns]
  if (dropColumns) {
    Tab = Tab %>%
      select(all_of(c(repCnames, pivotCname, splitCnames)))
  }
  Tab = Tab %>%
    pivot_wider(id_cols = all_of(repCnames), names_from = one_of(pivotCname), values_from = all_of(splitCnames),
                names_vary = "slowest", values_fill = NA)
  write_csv(Tab, str_replace(fname, "_Processed.csv", "_WidenedRes_Processed.csv"))
  Tab
}

downloadStudyDetails = function(studyID, prefix = "", short = FALSE, writeResponse = TRUE) {
  URL = paste0(QUERY_EBI, studyID, ifelse(short, CONVERSION_EBI_SHORT, CONVERSION_EBI_LONG))
  result = try(read_html(URL) %>% 
                 xml_text() %>%
                 read_tsv(guess_max = Inf, show_col_types = FALSE), 
               silent = TRUE)
  if (length(result) == 1 && class(result) == "try-error") {
    print(paste("Warning: unsuccessful at extracting information on", studyID))
    return(NULL)
  } else {
    if (short) {
      result = bind_cols(ID = studyID, result)
    }
    if (writeResponse) {
      write_csv(result, paste0(prefix, studyID, "_Details.csv"))
    }
  }
  result
}

extractRunsFromSamples = function(accessionList, short = TRUE) {
  L = length(accessionList)
  Tab = tibble()
  print(paste("There are", L, "entries to process"))
  startInd = 1
  for (ind in startInd:L) {
    if (ind %% 10 == 0) { print(ind) }
    acc = accessionList[ind]
    Tab %<>%
      bind_rows(downloadStudyDetails(acc, short = short, writeResponse = FALSE))
  }
  Tab
}

convertAccessions = function(fname, cName = "sample_name", short = TRUE, previousMappingsFile = NULL) {
  initTab = read_csv(fname, guess_max = Inf)
  accessions = initTab %>%
    select(one_of(cName)) %>%
    pull()
  if (!is.null(previousMappingsFile)) {
    stopifnot(short)
    oldMap = read_csv(previousMappingsFile, guess_max = Inf) %>%
      filter(ID %in% accessions)
    accessions %<>%
      setdiff(oldMap$ID)
  }
  mappedAccessions = extractRunsFromSamples(accessions, short = short)
  if (!is.null(previousMappingsFile)) {
    mappedAccessions %<>%
      bind_rows(oldMap)
  }
  namedList = ifelse(short, "ID", "sample_accession") %>%
    set_names(cName)
  augTab = initTab %>%
    inner_join(mappedAccessions, by = namedList)
  if (!short) {
    extraNamedList = "secondary_sample_accession" %>%
      set_names(cName)
    extraAugTab = initTab %>%
      inner_join(mappedAccessions, by = extraNamedList)
    augTab %<>%
      bind_rows(extraAugTab)
  }
  write_csv(mappedAccessions, paste0(WORKING_DIR, "Mappings/", str_replace(fname, "Processed.csv", "Mapped.csv")))
  write_csv(augTab, str_replace(fname, "Processed.csv", "Mapped_Processed.csv"))
  augTab
}
