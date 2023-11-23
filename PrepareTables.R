setwd(WORKING_DIR)

concatenateAllFiles = function() {
  initDir = getwd()
  setwd(paste0(WORKING_DIR, "SelectedTables"))
  LF0 = str_subset(list.files(), "PMID_30643125")
  Tab0 = read_csv(LF0[1])
  cnames0 = colnames(Tab0) %>%
    str_replace_all("Penicillin", "PEN")
  colnames(Tab0) = cnames0
  write_csv(Tab0, LF0[1])
  C0  = concatenateFiles(LF0, "PMID_30643125_Processed.csv")
  setwd(paste0(WORKING_DIR, "RemainingTables"))
  LF1 = str_subset(list.files(), "MTuberculosis_Processed.csv")
  LF1 = LF1[c(2:4,1)]
  C1  = concatenateFiles(LF1, "PMID_26686880_ncomms10063_MTuberculosis_Processed.csv")
  LF2 = str_subset(list.files(), "StaphAureus_Processed.csv")
  C2  = concatenateFiles(LF2, "PMID_26686880_ncomms10063_StaphAureus_Processed.csv")
  LF3 = str_subset(list.files(), "Part[1|2]_Genotype")
  C3  = concatenateFiles(LF3, "PMID_23299977_Genotype_Processed.csv")
  LF4 = str_subset(list.files(), "Part3")
  C4  = joinGenoPheno(LF4[2], LF4[1], "Strain", "Database_ID", 
                      outFilename = "PMID_34554083_Merged_Part3_Processed.csv")
  C4 %<>% 
    rename(run_accession = WGS, sample = Database_ID, sample_accession = BioSample)
  write_csv(C4, "PMID_34554083_Merged_Part3_Processed.csv")
  LF5 = str_subset(list.files(), "Part[1|2]_Processed.csv")
  C5  = concatenateFiles(LF5, "PMID_34554083_mgen-7-0610-s002_Parts12_Processed.csv")
  LF6 = str_subset(list.files(), "Part")
  C6  = concatenateFiles(LF6, "PMID_34554083_mgen-7-0610-s002_Processed.csv")
  LF7 = str_subset(list.files(), "PMID_31086182")
  C7  = read_csv("PMID_31086182_41467_2019_10110_MOESM4_ESM_Processed.csv", guess_max = Inf)
  C7 %<>%
    rename(`Isolate ID` = `WGSAccessionNumber OR Run Accession`, Country = `Country of Isolation`, 
           Source = `Source Lab`, Lab = `DST/MIC testing Lab (Tables S1,S2)`)
  write_csv(C7, "PMID_31086182_41467_2019_10110_MOESM4_ESM_Processed.csv")
  D7  = concatenateFiles(LF7, "PMID_31086182_41467_2019_10110_MOESM4_MOESM7_ESM_Processed.csv")
  LF8 = str_subset(list.files(), "PMID_31736907")
  C8  = concatenateFiles(LF8, "PMID_31736907_S1_S2_Processed.csv")
  LF9 = str_subset(list.files(), "PMID_24462211")
  C9  = concatenateFiles(LF9, "PMID_24462211_Genotype_Processed.csv")
  file.copy("PMID_24462211_Genotype_Processed.csv", "PMID_27638945_Genotype_Processed.csv")
  LF10 = str_subset(list.files(), "PMID_35308374")
  C10 = concatenateFiles(LF10[1:2], "PMID_35308374_Genotype_Processed.csv")
  setwd(initDir)
}

joinAllGenoPheno = function() {
  initDir = getwd()
  setwd(paste0(WORKING_DIR, "SelectedTables"))
  Q0 = joinGenoPheno("PMID_34867917_filereport_read_run_PRJNA600010_Genotype_Processed.csv", 
                     "PMID_34867917_Table_1_Systematic Evaluation of Whole-Genome Sequencing Based Prediction of Antimicrobial Resistance in Campylobacter jejuni and C. coli_Phenotype_Processed.csv",
                     "sample_accession", "Biosample #")
  Q1 = joinGenoPheno("PMID_34694881_SraRunTable_Genotype_Processed.csv", 
                     "PMID_34694881_aac.01196-21-s0002_Phenotype_Processed.csv",
                     "Library Name", "Sample ID")
  Q2 = joinGenoPheno("PMID_34493269_PRJNA628943_AssemblyDetails_Genotype_Processed.csv", 
                     "PMID_34493269_12915_2021_1094_MOESM1_ESM_Phenotype_Processed.csv",
                     "Strain", "Strain")
  Q3 = joinGenoPheno("PMID_33897640_Table_1_Genotype_Processed.csv",
                     "PMID_33897640_Table_1_Phenotype_Processed.csv",
                     "Isolates", "Isolate")
  Q4 = joinGenoPheno("PMID_33875431_aac.02696-20-s0001_TableS6_Genotype_Processed.csv", 
                     "PMID_33875431_aac.02696-20-s0001_TableS7_Phenotype_Processed.csv", 
                     "Isolate_IDs", "Clinical_isolate_IDs")
  Q5 = joinGenoPheno("PMID_33659219_Table_1_Genotype_Processed.csv", 
                     "PMID_33659219_Table_1_Phenotype_Processed.csv",
                     "isolate_id", "isolate_id")
  Q6 = joinGenoPheno("PMID_33021437_Supp_TableS1_Genotype_Processed.csv",
                     "PMID_33021437_Table1_Phenotype_Processed.csv",
                     "Isolate name", "Isolate name")
  Q7 = joinGenoPheno("PMID_32562543_dkaa221_supplementary_data_TableS1_Genotype_Processed.csv",
                     "PMID_32562543_dkaa221_supplementary_data_TableS2_Phenotype_Processed.csv",
                     "Sample", "Strain", defaultPheno = "MET")
  Q8 = joinGenoPheno("PMID_32427945_41598_2020_64934_MOESM1_ESM_Genotype_Processed.csv",
                     "PMID_32427945_41598_2020_64934_MOESM1_ESM_Phenotype_Processed.csv",
                     "Isolate", "Isolate")
  Q9 = joinGenoPheno("PMID_32205351_SraRunTable_Genotype_Processed.csv",
                     "PMID_32205351_bd_phoenix_mics_Phenotype_Processed.csv",
                     "Isolate", "isolate")
  Q10 = joinGenoPheno("PMID_25367914_PRJNA264310_AssemblyDetails_Genotype_Processed.csv",
                      "PMID_25367914_AAC.03954-14_zac001153606sd2_Phenotype_Processed.csv",
                      "Strain", "Isolate")
  TabA = read_csv("PMID_29776807_SraRunTable_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(ShortName = str_remove_all(Sample_Name, '-sc-[0-9]*'))
  write_csv(TabA, "PMID_29776807_SraRunTable_Genotype_Processed.csv")
  TabB = read_csv("PMID_29776807_pathogenwatch-ngonorrhoeae-eurogasp2013-amr-profile_Phenotype_Processed.csv",
                  guess_max = Inf) %>%
    mutate(ShortName = str_remove_all(NAME, 'ECDC_'))
  write_csv(TabB, "PMID_29776807_pathogenwatch-ngonorrhoeae-eurogasp2013-amr-profile_Phenotype_Processed.csv")
  Q11 = joinGenoPheno("PMID_29776807_SraRunTable_Genotype_Processed.csv",
                      "PMID_29776807_pathogenwatch-ngonorrhoeae-eurogasp2013-amr-profile_Phenotype_Processed.csv",
                      "ShortName", "ShortName")
  TabC = read_csv("PMID_27638945_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(GSGC_Number = str_remove_all(STRAIN, "GCGS") %>% as.integer())
  write_csv(TabC, "PMID_27638945_Genotype_Processed.csv")
  TabD = read_csv("PMID_27638945_supp_jiw420_jiw420supp_Phenotype_Processed.csv", guess_max = Inf) %>%
    mutate(GSGC_Number = str_remove_all(Identifier, "GCGS") %>% as.integer())
  write_csv(TabD, "PMID_27638945_supp_jiw420_jiw420supp_Phenotype_Processed.csv")
  Q12 = joinGenoPheno("PMID_27638945_Genotype_Processed.csv",
                      "PMID_27638945_supp_jiw420_jiw420supp_Phenotype_Processed.csv",
                      "GSGC_Number", "GSGC_Number")
  TabE = read_csv("PMID_32025709_filereport_read_run_PRJEB32286_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(ID = str_extract(submitted_ftp, "HICF[A-Za-z0-9]+") %>% str_remove_all("[r|R][p|P][t|T][0-9]*"))
  write_csv(TabE, "PMID_32025709_filereport_read_run_PRJEB32286_Genotype_Processed.csv")
  Q13 = joinGenoPheno("PMID_32025709_filereport_read_run_PRJEB32286_Genotype_Processed.csv",
                      "PMID_32025709_JAC-2019-1726_SupplementaryTables_Phenotype_Processed.csv",
                      "ID", "StrainID")
  TabF = readLines("PMID_32048461_Details_PRJNA526797")
  TabF %<>%
    subset(mod(1:length(.), 7) == 2) %>%
    str_remove_all("Identifiers:\t") %>%
    str_split_fixed("\\;", n = 3) %>%
    subset(, 1:2)
  SampleIDs = TabF[,1] %>%
    str_trim() %>%
    str_remove_all("BioSample: ")
  StrainIDs = TabF[,2] %>%
    str_trim() %>%
    str_remove_all("Sample name: ")
  write_csv(tibble(Sample = SampleIDs, Strain = StrainIDs), "PMID_32048461_Details_PRJNA526797_Processed.csv")
  Q135 = joinGenoPheno("PMID_32048461_Details_PRJNA526797_Processed.csv",
                       "PMID_32048461_EMMM-12-e10264-s003_Phenotype_Processed.csv",
                       "Strain", "Isolate",  outFilename = "PMID_32048461_Merged_Phenotype_Processed.csv")
  Q14 = joinGenoPheno("PMID_32048461_filereport_read_run_PRJNA526797_Genotype_Processed.csv",
                      "PMID_32048461_Merged_Phenotype_Processed.csv",
                      "sample_accession", "Sample")
  Q15 = joinGenoPheno("PMID_32311302_filereport_read_run_PRJNA186035_Genotype_Processed.csv",
                      "PMID_32311302_Suppl_TableS2_Processed.csv", 
                      "sample_accession", "BioSample")
  Q16 = joinGenoPheno("PMID_32903472_Genotype_Processed.csv", 
                      "PMID_32903472_Table_2_Phenotype_Processed.csv",
                      "isolate", "Strain name")
  C17 = concatenateFiles(c("PMID_32974772_filereport_read_run_PRJEB31119_Genotype_Processed.csv", 
                     "PMID_32974772_filereport_read_run_PRJEB40238_Genotype_Processed.csv"),
                     "PMID_32974772_Genotype_Processed.csv")
  Q17 = joinGenoPheno("PMID_32974772_Genotype_Processed.csv",
                      "PMID_32974772_10096_2020_4043_MOESM1_ESM_Processed.csv",
                      "sample_accession", "ENA Number")
  C18 = concatenateFiles(c("PMID_33875431_PRJNA325248_SraRunTable_Genotype_Processed.csv",
                           "PMID_33875431_PRJNA649313_SraRunTable_Genotype_Processed.csv"),
                         "PMID_33875431_RunTables_Merged_Genotype_Processed.csv")
  Q18 = joinGenoPheno("PMID_33875431_RunTables_Merged_Genotype_Processed.csv",
                      "PMID_33875431_Merged_Phenotype_Processed.csv",
                      "Isolate", "Clinical_isolate_IDs", outFilename = "PMID_33875431_Merged_Part1_Processed.csv")
  Q19 = joinGenoPheno("PMID_33875431_Auxiliary_Genotype_Processed.csv",
                      "PMID_33875431_Merged_Phenotype_Processed.csv",
                      "ID", "Clinical_isolate_IDs", outFilename = "PMID_33875431_Merged_Part2_Processed.csv")
  C19 = concatenateFiles(c("PMID_33875431_Merged_Part1_Processed.csv", "PMID_33875431_Merged_Part2_Processed.csv"),
                         "PMID_33875431_Merged_Processed.csv")
  TabG = read_csv("PMID_33875431_Merged_Processed.csv") %>%
    mutate_at("Host", ~{.[is.na(.)] = Host.y[is.na(.)]; .}) %>%
    mutate_at("Run", ~{.[is.na(.)] = Accession[is.na(.)]; .}) %>%
    select(-Isolate_country, -`*BioProject`, -Host.x, -Host.y, -Accession)
  write_csv(TabG, "PMID_33875431_Merged_Processed.csv")
  Q20 = joinGenoPheno("PMID_33897640_PRJNA675435_AssemblyDetails_Genotype_Processed.csv",
                      "PMID_33897640_Merged_Phenotype_Processed.csv",
                      "Strain", "Isolate")
  TabH = read_csv("PMID_34166472_PRJNA576513_SraRunTable_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(Index = str_remove_all(`Sample Name`, "SKS_"))
  write_csv(TabH, "PMID_34166472_PRJNA576513_SraRunTable_Genotype_Processed.csv")
  TabI = read_csv("PMID_34166472_pone.0253797.s003_Phenotype_Processed.csv", guess_max = Inf) %>%
    mutate(Index = str_remove_all(ID, "Camp_"))
  write_csv(TabI, "PMID_34166472_pone.0253797.s003_Phenotype_Processed.csv")
  Q21 = joinGenoPheno("PMID_34166472_PRJNA576513_SraRunTable_Genotype_Processed.csv",
                      "PMID_34166472_pone.0253797.s003_Phenotype_Processed.csv",
                      "Index", "Index")
  TabJ = read_csv("PMID_34414415_Tables_20042018-revised_for_submission_Phenotype_Processed.csv", guess_max = Inf)
  TabK = TabJ %>%
    filter(str_detect(SRA, ACCESSIONS["RUN"]))
  write_csv(TabK, "PMID_34414415_PartA_Phenotype_Processed.csv")
  TabL = TabJ %>%
    filter(!(str_detect(SRA, ACCESSIONS["RUN"])) & !(Bioproject == "PRJNA413593"))
  write_csv(TabL, "PMID_34414415_PartB_Phenotype_Processed.csv")
  TabM = TabJ %>%
    filter(Bioproject == "PRJNA413593")
  write_csv(TabM, "PMID_34414415_PartC_Phenotype_Processed.csv")
  TabJ = TabJ %>%
    filter(str_sub(SRA, 1, 3) == "SAM")
  write_csv(TabJ, "PMID_34414415_PartD_Phenotype_Processed.csv")
  C22 = concatenateFiles(c("PMID_34414415_filereport_read_run_PRJNA302362_Genotype_Processed.csv",
                           "PMID_34414415_filereport_read_run_PRJNA390471_Genotype_Processed.csv",
                           "PMID_34414415_filereport_read_run_PRJNA401515_Genotype_Processed.csv"),
                         "PMID_34414415_Merged_Genotype_Processed.csv")
  Q22 = joinGenoPheno("PMID_34414415_Merged_Genotype_Processed.csv",
                      "PMID_34414415_PartD_Phenotype_Processed.csv",
                      "sample_accession", "SRA", outFilename = "PMID_34414415_PartD_Merged_Processed.csv")
  Q23 = joinGenoPheno("PMID_34414415_filereport_read_run_PRJNA283583_Genotype_Processed.csv", 
                      "PMID_34414415_PartB_Phenotype_Processed.csv",
                      "secondary_sample_accession", "SRA", outFilename = "PMID_34414415_PartB_Merged_Processed.csv")
  TabN = read_csv("PMID_34414415_SraRunTable_PRJNA413593_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(IsolateRecoded = str_remove_all(Isolate, "BC") %>% str_remove_all("Mtb") %>%
             str_replace("-", "s") %>% str_replace("^0", "A"))
  write_csv(TabN, "PMID_34414415_SraRunTable_PRJNA413593_Genotype_Processed.csv")
  Q24 = joinGenoPheno("PMID_34414415_SraRunTable_PRJNA413593_Genotype_Processed.csv", 
                      "PMID_34414415_PartC_Phenotype_Processed.csv",
                      "IsolateRecoded", "labnumber", outFilename = "PMID_34414415_PartC_Merged_Processed.csv")
  C24 = concatenateFiles(c("PMID_34414415_PartA_Phenotype_Processed.csv", 
                           "PMID_34414415_PartB_Merged_Processed.csv", 
                           "PMID_34414415_PartC_Merged_Processed.csv",
                           "PMID_34414415_PartD_Merged_Processed.csv"),
                         "PMID_34414415_Merged_Processed.csv")
  Q25 = joinGenoPheno("PMID_34547028_filereport_read_run_PRJNA736314_Genotype_Processed.csv", 
                      "PMID_34547028_pone.0249617.s001_Phenotype_Processed.csv", 
                      "sample_accession", "accession number")
  TabP = read_csv("PMID_33068113_genomes_info_Processed.csv", guess_max = Inf)
  TabQ = TabP %>%
    filter(Accession == "PRJNA597427") %>%
    mutate_at("id", ~{as.integer(.)})
  write_csv(TabQ, "PMID_33068113_PartA_Phenotype_Processed.csv")
  TabR = TabP %>%
    filter(Accession == "PRJNA449293") %>%
    mutate_at("id", ~{as.integer(.)})
  write_csv(TabR, "PMID_33068113_PartB_Phenotype_Processed.csv")
  Q26 = joinGenoPheno("PMID_33068113_PRJNA597427_AssemblyDetails_Genotype_Processed.csv",
                      "PMID_33068113_PartA_Phenotype_Processed.csv",
                      "Strain", "id", outFilename = "PMID_33068113_PartA_Merged_Processed.csv")
  Q27 = joinGenoPheno("PMID_33068113_PRJNA449293_SraRunTable_Genotype_Processed.csv",
                      "PMID_33068113_PartB_Phenotype_Processed.csv",
                      "STRAIN", "id", outFilename = "PMID_33068113_PartB_Merged_Processed.csv")
  C27 = concatenateFiles(c("PMID_33068113_PartA_Merged_Processed.csv", 
                           "PMID_33068113_PartB_Merged_Processed.csv"),
                         "PMID_33068113_Merged_Processed.csv")
  LF = list.files(pattern = "PMID_35780211") %>%
    setdiff(c("PMID_35780211_master_table_resistance_Processed.csv", 
              "PMID_35780211_PRJNA343736_AssemblyDetails_Genotype_Processed.csv"))
  C28 = concatenateFiles(LF, "PMID_35780211_Merged_Genotype_Processed.csv")
  TabS = read_csv("PMID_35780211_master_table_resistance_Processed.csv", guess_max = Inf)
  TabT = TabS %>%
    filter(!str_detect(accessions, ACCESSIONS["RUN"])) %>%
    distinct(accessions, .keep_all = TRUE)
  TabS = TabS %>%
    filter(str_detect(accessions, ACCESSIONS["RUN"]))
  write_csv(TabS, "PMID_35780211_PartA_Phenotype_Processed.csv")
  write_csv(TabT, "PMID_35780211_PartB_Phenotype_Processed.csv")
  Q28 = joinGenoPheno("PMID_35780211_Merged_Genotype_Processed.csv",
                      "PMID_35780211_PartB_Phenotype_Processed.csv",
                      "sample_accession", "accessions", 
                      outFilename = "PMID_35780211_MatchA_Merged_Processed.csv")
  Q29 = joinGenoPheno("PMID_35780211_Merged_Genotype_Processed.csv",
                      "PMID_35780211_PartB_Phenotype_Processed.csv",
                      "secondary_sample_accession", "accessions", 
                      outFilename = "PMID_35780211_MatchB_Merged_Processed.csv")
  Q30 = joinGenoPheno("PMID_35780211_PRJNA343736_AssemblyDetails_Genotype_Processed.csv",
                      "PMID_35780211_PartB_Phenotype_Processed.csv",
                      "BioSample", "accessions",
                      outFilename = "PMID_35780211_MatchC_Merged_Processed.csv")
  C30 = concatenateFiles(c(paste0("PMID_35780211_Match", LETTERS[1:3], "_Merged_Processed.csv"), 
                           "PMID_35780211_PartA_Phenotype_Processed.csv"),
                         "PMID_35780211_Merged_Processed.csv")
  stopifnot(all(!is.na(C30$run_accession) | !is.na(C30$run) | !is.na(C30$run_combined) | !is.na(C30$WGS)))
  Q31 = joinGenoPheno("DOI_10.1101:2021.09.14.460274_Genotype_Merged_Processed.csv",
                      "DOI_10.1101:2021.09.14.460274_CRyPTIC_reuse_table_20221019_Processed.csv",
                      "secondary_sample_accession", "ENA_SAMPLE")
  TabU = read_csv("PMID_30550564_pcbi.1006258.s010_Processed.csv", guess_max = Inf)
  TabV = TabU %>%
    filter(!str_detect(Lane.accession, ACCESSIONS[["RUN"]]))
  write_csv(TabV, "PMID_30550564_PartA_Phenotypes_Processed.csv")
  TabU = TabU %>%
    filter(str_detect(Lane.accession, ACCESSIONS[["RUN"]]))
  write_csv(TabU, "PMID_30550564_PartB_Phenotypes_Processed.csv")
  Q32 = joinGenoPheno("PMID_30550564_filereport_read_run_PRJEB23294_Genotype_Processed.csv", 
                      "PMID_30550564_PartA_Phenotypes_Processed.csv",
                      "secondary_sample_accession", "Lane.accession", 
                      outFilename = "PMID_30550564_PartA_Merged_Processed.csv")
  C32 = concatenateFiles(c("PMID_30550564_PartA_Merged_Processed.csv", 
                           "PMID_30550564_PartB_Phenotypes_Processed.csv"),
                         "PMID_30550564_Merged_Processed.csv")
  C33 = concatenateFiles(list.files(pattern = "PMID_33659219_[A-Za-z0-9\\_]*_Genotype"),
                         "PMID_33659219_Merged_Genotype_Processed.csv")
  Q33 = joinGenoPheno("PMID_33659219_Merged_Genotype_Processed.csv", 
                      "PMID_33659219_Merged_EColi_Processed.csv", 
                      "sample_accession", "sample_name",
                      outFilename = "PMID_33659219_Merged_EColi_PartA_Processed.csv")
  Q34 = joinGenoPheno("PMID_32427945_PRJNA564922_AssemblyDetails_Genotype_Processed.csv", 
                      "PMID_32427945_Merged_Phenotype_Processed.csv",
                      "Strain", "Isolate")
  TabV = read_csv("PMID_30643125_Processed.csv", guess_max = Inf) %>%
    mutate(Accession = str_split_fixed(Accession.number, ":", n = 2)[,2])
  TabW = TabV %>%
    filter(str_detect(Accession, ACCESSIONS["BIOSAMPLE"]))
  write_csv(TabW, "PMID_30643125_PartA_Phenotype_Processed.csv")
  TabV = TabV %>%
    filter(str_detect(Accession, ACCESSIONS["RUN"]))
  write_csv(TabV, "PMID_30643125_PartB_Phenotype_Processed.csv")
  Q35 = joinGenoPheno("PMID_30643125_SraRunTable_Genotype_Processed.csv", 
                      "PMID_30643125_PartA_Phenotype_Processed.csv",
                      "BioSample", "Accession", outFilename = "PMID_30643125_PartA_Merged_Processed.csv")
  C35 = concatenateFiles(c("PMID_30643125_PartA_Merged_Processed.csv", 
                           "PMID_30643125_PartB_Phenotype_Processed.csv"),
                         "PMID_30643125_Merged_Processed.csv")
  setwd(paste0(WORKING_DIR, "RemainingTables"))
  Tab0 = read_csv("PMID_23299977_Genotype_Processed.csv") %>%
    mutate(short_ftp = submitted_ftp %>%
             str_sub(-13,-5) %>%
             str_remove_all("/") %>%
             str_replace_all("\\#", "_"))
  write_csv(Tab0, "PMID_23299977_Genotype_Processed.csv")
  R1 = joinGenoPheno('PMID_23299977_Genotype_Processed.csv',
                     'PMID_23299977_Supp_TableS1_Phenotype_Processed.csv',
                     'short_ftp', 'Sanger ID')
  R2 = joinGenoPheno('PMID_24464101_Genotype_Processed.csv',
                     'PMID_24464101_NIHMS56217-supplement-3_Phenotype_Processed.csv',
                     'Sample', 'Isolate')
  R3 = joinGenoPheno('PMID_25373147_Genotype_Processed.csv',
                     'PMID_25373147_Dataset_S1_Phenotype_Processed.csv',
                     'Sample Name', 'Sample ID')
  R4 = joinGenoPheno('PMID_25960343_ncomms8119-s2_Genotype_Processed.csv',
                     'PMID_25960343_ncomms8119-s5_Phenotype_Processed.csv',
                     'Isolate name', 'Strain')
  R5 = joinGenoPheno('PMID_26142410_dkv186supp_Genotype_Processed.csv',
                     'PMID_26142410_dkv186supp_Phenotype_Processed.csv',
                     'Isolate', 'Isolate')
  Tab1 = read_csv("PMID_26418737_Genotype_Processed.csv") %>%
    mutate_at('Sample Name', ~{str_remove_all(., "Mycobacterium tuberculosis ")})
  write_csv(Tab1, "PMID_26418737_Genotype_Processed.csv")
  R6 = joinGenoPheno('PMID_26418737_Genotype_Processed.csv',
                     'PMID_26418737_pmed.1001880.s006_Phenotype_Processed.csv',
                     'Sample Name', 'Specimen ID')
  R7 = joinGenoPheno('PMID_26669893_Genotype_Processed.csv',
                     'PMID_26669893_Table 6_Phenotype_Processed.csv',
                     'Isolate', 'Sample ID')
  R8 = joinGenoPheno('PMID_26935729_Genotype_Processed.csv',
                     'PMID_26935729_JCM.03195-15_zjm999094941so1_Phenotype_Processed.csv',
                     'Sample Name', 'IsolateID')
  R9 = joinGenoPheno('PMID_29182725_dkx405_supplementary_data1_Genotype_Processed.csv',
                     'PMID_29182725_dkx405_supplementary_data2_Phenotype_Processed.csv',
                     'Code using in manuscript', 'isolate')
  Tab2 = read_csv("PMID_30346947_PRJEB11776_AssemblyDetails_CDifficile_Genotype_Processed.csv") %>%
    mutate_at('WGS', ~{str_replace_all(., "00000000", "01000001")})
  write_csv(Tab2, "PMID_30346947_PRJEB11776_AssemblyDetails_CDifficile_Genotype_Processed.csv")
  R10 = joinGenoPheno('PMID_30346947_PRJEB11776_AssemblyDetails_CDifficile_Genotype_Processed.csv',
                      'PMID_30346947_pcbi.1006434.s005_CDifficile_Phenotype_Processed.csv',
                      'WGS', 'Accession number', outFilename = 'PMID_30346947_CDifficile_Merged_Processed.csv')
  R11 = joinGenoPheno('PMID_30346947_PRJNA244279_AssemblyDetails_PAeruginosa_Genotype_Processed.csv',
                      'PMID_30346947_pcbi.1006434.s005_PAeruginosa_Phenotype_Processed.csv',
                      'BioSample', 'Accession number', outFilename = 'PMID_30346947_PAeruginosa_Merged_Processed.csv')
  R12 = joinGenoPheno('PMID_31266463_12866_2019_1520_MOESM1_ESM_Genotype_Processed.csv',
                      'PMID_31266463_12866_2019_1520_MOESM1_ESM_Phenotype_Processed.csv',
                      'Sample', 'sample')
  Tab3 = read_csv("PMID_31307955_Genotype_Processed.csv") %>%
    mutate_at('Sample_Name', ~{str_remove_all(., "Escherichia coli-")})
  write_csv(Tab3, "PMID_31307955_Genotype_Processed.csv")
  R13 = joinGenoPheno('PMID_31307955_Genotype_Processed.csv',
                      'PMID_31307955_mmc2_Phenotype_Processed.csv',
                      'Sample_Name', 'Isolate')
  R14 = joinGenoPheno('PMID_31915233_Genotype_Processed.csv',
                      'PMID_31915233_Phenotype_Processed.csv',
                      'Isolate', 'Isolate')
  R15 = joinGenoPheno('PMID_33055186_Genotype_Processed.csv',
                      'PMID_33055186_JCM.00583-20-s0006_Phenotype_Processed.csv',
                      'Sample Name', 'Sample')
  R16 = joinGenoPheno('PMID_33656437_mgen-7-0531-s001_Genotype_Processed.csv',
                      'PMID_33656437_mgen-7-0531-s001_Phenotype_Processed.csv',
                      'Sample', 'Antibiotic')
  Tab4 = read_csv("PMID_34519526_SraRunTable_Genotype_Processed.csv") %>%
    mutate_at('STRAIN', ~{str_replace_all(., "NU-CRE", "CRE-")})
  write_csv(Tab4, "PMID_34519526_SraRunTable_Genotype_Processed.csv")
  R17 = joinGenoPheno('PMID_34519526_SraRunTable_Genotype_Processed.csv',
                      'PMID_34519526_msystems.00194-21-st001_Phenotype_Processed.csv',
                      'STRAIN', 'Isolate ID')
  R18 = joinGenoPheno('PMID_35442078_SraRunTable_Genotype_Processed.csv',
                      'PMID_35442078_spectrum.01594-21-s002_Phenotype_Processed.csv',
                      'Sample Name', 'ISOLATE-ID')
  Tab5 = read_csv('PMID_35756053_Table_2_EColi_Phenotype_Processed.csv', col_types = cols(ID = "c")) %>%
    filter(!is.na(ID))
  write_csv(Tab5, 'PMID_35756053_Table_2_EColi_Phenotype_Processed.csv')
  R19 = joinGenoPheno('PMID_35756053_Table_1_Genotype_Processed.csv',
                      'PMID_35756053_Table_2_EColi_Phenotype_Processed.csv',
                      'ID(1)', 'ID', outFilename = 'PMID_35756053_EColi_Merged_Processed.csv')
  Tab6 = read_csv('PMID_35756053_Table_2_SAureus_Phenotype_Processed.csv', col_types = cols(ID = "c")) %>%
    filter(!is.na(ID))
  write_csv(Tab6, 'PMID_35756053_Table_2_SAureus_Phenotype_Processed.csv')
  R20 = joinGenoPheno('PMID_35756053_Table_1_Genotype_Processed.csv',
                      'PMID_35756053_Table_2_SAureus_Phenotype_Processed.csv',
                      'ID(1)', 'ID', outFilename = 'PMID_35756053_SAureus_Merged_Processed.csv')
  R21 = joinGenoPheno('PMID_24462211_Genotype_Processed.csv', 
                      'PMID_24462211_mmc1_Phenotype_Processed.csv', 
                      'STRAIN', 'Sample')
  R22 = joinGenoPheno('PMID_25378573_SraRunTable_PRJNA266539_Genotype_Processed.csv',
                      'PMID_25378573_JCM.02589-14_zjm999093959so3_Phenotype_Processed.csv',
                      'Isolate', 'Strain Id')
  R23 = joinGenoPheno('PMID_35308374_Genotype_Processed.csv', 
                      'PMID_35308374_Table_2_Phenotype_Processed.csv', 
                      'sample_accession', 'SRA_BioSample')
  Tab7 = read_csv("PMID_27216077_SraRunTable_Genotype_Processed.csv") %>%
    mutate_at("Isolate", ~{str_remove_all(., "Isolate ") %>% toupper()})
  write_csv(Tab7, "PMID_27216077_SraRunTable_Genotype_Processed.csv")
  R24 = joinGenoPheno('PMID_27216077_SraRunTable_Genotype_Processed.csv',
                      'PMID_27216077_AAC.00075-16_zac008165389so1_Phenotype_Processed.csv',
                      'Isolate', 'Isolate')
  Tab8 = read_csv("PMID_32929164_meta_Genotype_Processed.csv", guess_max = Inf) %>%
    mutate(shortFTP = str_extract(submitted_ftp, "/SAM[A-Za-z0-9\\-\\.]+\\_") %>% str_sub(2)) %>%
    mutate(Sample = str_extract(shortFTP, "SAM[A-Za-z0-9]*")) %>%
    mutate(Run = str_extract(fastq_ftp, "[A-Z0-9]*\\_1.fastq.gz") %>% str_sub(1, -12))
  write_csv(Tab8, "PMID_32929164_meta_Genotype_Processed.csv")
  R25 = joinGenoPheno("PMID_32929164_meta_Genotype_Processed.csv", 
                      "PMID_32929164_supplementary_meta_table_Phenotype_Processed.csv",
                      "Sample", "sample_accession")
  Tab9 = read_csv("PMID_32943401_filereport_read_run_PRJEB25999_Genotype_Processed.csv") %>%
    mutate(ID = str_split_fixed(library_name, ". ", n = 4)[,3])
  write_csv(Tab9, "PMID_32943401_filereport_read_run_PRJEB25999_Genotype_Processed.csv")
  R26a = joinGenoPheno("PMID_32943401_filereport_read_run_PRJEB25999_Genotype_Processed.csv",
                       "PMID_32943401_ERJ-02338-2020.Tables_Phenotype_Processed.csv",
                       "ID", "Sample", outFilename = "PMID_32943401_PartA_Merged_Processed.csv")
  Tab10 = read_csv("PMID_32943401_filereport_read_run_PRJEB31023_Genotype_Processed.csv") %>%
    mutate(index = str_remove_all(sample_alias, "TDR-")) %>%
    mutate(ID = paste0("TB-TDR-", strrep("0", 4 - nchar(index)), index)) %>%
    select(-index)
  write_csv(Tab10, "PMID_32943401_filereport_read_run_PRJEB31023_Genotype_Processed.csv")
  R26b = joinGenoPheno("PMID_32943401_filereport_read_run_PRJEB31023_Genotype_Processed.csv",
                       "PMID_32943401_ERJ-02338-2020.Tables_Phenotype_Processed.csv",
                       "ID", "Sample", outFilename = "PMID_32943401_PartB_Merged_Processed.csv")
  R26 = concatenateFiles(c("PMID_32943401_PartA_Merged_Processed.csv", "PMID_32943401_PartB_Merged_Processed.csv"),
                         "PMID_32943401_Merged_Processed.csv")
  Tab11 = read_csv("PMID_33945455_mgen-7-0573-s001_Phenotype_Processed.csv") %>%
    mutate_all(~{str_trim(.)})
  write_csv(Tab11, "PMID_33945455_mgen-7-0573-s001_Phenotype_Processed.csv")
  R27 = joinGenoPheno("PMID_33945455_filereport_read_run_PRJNA639216_Genotype_Processed.csv",
                      "PMID_33945455_mgen-7-0573-s001_Phenotype_Processed.csv",
                      "sample_accession", "Accession No")
  R28 = concatenateFiles(list.files(pattern = "PMID_34161790")[-1], "PMID_34161790_Genotype_Merged_Processed.csv")
  R29 = joinGenoPheno("PMID_34161790_Genotype_Merged_Processed.csv",
                      "PMID_34161790_1-s2.0-S0924857921001448-mmc2_Processed.csv",
                      "sample_accession", "Accession No")
  R30 = joinGenoPheno("PMID_34733267_filereport_read_run_PRJNA756552_Genotype_Processed.csv",
                      "PMID_34733267_Table_1_Phenotype_Processed.csv",
                      "sample_accession", "NCBI BioSample Accession")
  C31 = concatenateFiles(list.files(pattern = "PMID_26116186")[-5], "PMID_26116186_Genotype_Merged_Processed.csv")
  Tab12 = read_csv("PMID_26116186_mmc2_Processed.csv")
  Tab13 = Tab12 %>%
    filter(!str_detect(`Isolate accession number`, ACCESSIONS["RUN"]))
  write_csv(Tab13, "PMID_26116186_Part1_Phenotype_Processed.csv")
  Tab12 = Tab12 %>%
    filter(str_detect(`Isolate accession number`, ACCESSIONS["RUN"]))
  write_csv(Tab12, "PMID_26116186_Part2_Phenotype_Processed.csv")
  R31 = joinGenoPheno("PMID_26116186_Genotype_Merged_Processed.csv", 
                      "PMID_26116186_Part1_Phenotype_Processed.csv",
                      "sample_accession", "Isolate accession number", 
                      outFilename = "PMID_26116186_PartA_Merged.csv")
  R32 = joinGenoPheno("PMID_26116186_Genotype_Merged_Processed.csv", 
                      "PMID_26116186_Part1_Phenotype_Processed.csv",
                      "secondary_sample_accession", "Isolate accession number", 
                      outFilename = "PMID_26116186_PartB_Merged.csv")
  C32 = concatenateFiles(c("PMID_26116186_PartA_Merged.csv", "PMID_26116186_PartB_Merged.csv", 
                           "PMID_26116186_Part2_Phenotype_Processed.csv"), "PMID_26116186_Merged_Processed.csv")
  Tab14 = read_csv("PMID_26686880_ncomms10063_MTuberculosis_Processed.csv", guess_max = Inf)
  Tab15 = Tab14 %>%
    filter(!str_detect(sra_accessions, ACCESSIONS["RUN"]))
  write_csv(Tab15, "PMID_26686880_Part1_Phenotype_Processed.csv")
  Tab14 = Tab14 %>% 
    filter(str_detect(sra_accessions, ACCESSIONS["RUN"]))
  write_csv(Tab14, "PMID_26686880_Part2_Phenotype_Processed.csv")
  write_csv(C31, "PMID_26686880_Genotype_Merged_Processed.csv") # Use the same genotype file as previously!
  R33 = joinGenoPheno("PMID_26686880_Genotype_Merged_Processed.csv", 
                      "PMID_26686880_Part1_Phenotype_Processed.csv",
                      "secondary_sample_accession", "sra_accessions", 
                      outFilename = "PMID_26686880_PartA_Merged.csv")
  C33 = concatenateFiles(c("PMID_26686880_PartA_Merged.csv", "PMID_26686880_Part2_Phenotype_Processed.csv"),
                         "PMID_26686880_MTuberculosis_Merged_Processed.csv")
  R34 = joinGenoPheno("PMID_26686880_filereport_read_run_PRJEB5261_Genotype_Processed.csv",
                      "PMID_26686880_ncomms10063_StaphAureus_Processed.csv",
                      "secondary_sample_accession", "sra_accessions")
  Tab16 = read_csv("PMID_26686880_ncomms10063_StaphAureus_Processed.csv", guess_max = Inf) %>%
    filter(str_detect(sra_accessions, ACCESSIONS["RUN"]))
  write_csv(Tab16, "PMID_26686880_PartC_Merged.csv")
  C34 = concatenateFiles(c("PMID_26686880_PartC_Merged.csv", "PMID_26686880_Merged_Processed.csv"),
                         "PMID_26686880_StaphAureus_Merged_Processed.csv")
  Tab17 = read_csv("PMID_27150362_data_Phenotype_Processed.csv", guess_max = Inf) %>%
    filter(id != "57_UK_34" & id != "105_UK_22")  ## duplicate entries with incorrect provenance!
  write_csv(Tab17, "PMID_27150362_data_Phenotype_Processed.csv")
  R35 = joinGenoPheno("PMID_27150362_filereport_read_run_PRJEB2478_Genotype_Processed.csv",
                      "PMID_27150362_data_Phenotype_Processed.csv",
                      "secondary_sample_accession", "ERS Number")
  R36 = joinGenoPheno("PMID_31358980_filereport_read_run_PRJEB4024_Genotype_Processed.csv",
                      "PMID_31358980_EMS83238-supplement-Supplementary_Table_1_Processed.csv",
                      "secondary_sample_accession", "ENA accession")
  R37 = joinGenoPheno("PMID_35907429_filereport_read_run_PRJEB48275_Genotype_Processed.csv",
                      "PMID_35907429_1-s2.0-S2666524722001161-mmc2_Processed.csv",
                      "secondary_sample_accession", "Acc_no")
  file.rename("PMID_35780211_filereport_read_run_PRJEB31023_Genotype_Processed.csv", 
              "PMID_31086182_filereport_read_run_PRJEB31023_Genotype_Processed.csv")
  Tab18 = read_csv("PMID_31086182_41467_2019_10110_MOESM4_MOESM7_ESM_Processed.csv")
  Tab19 = Tab18 %>%
    filter(BioProject == "PRJEB31023")
  write_csv(Tab19, "PMID_31086182_PartA_Phenotype_Processed.csv")
  Tab18 = Tab18 %>%
    filter(is.na(BioProject) | BioProject != "PRJEB31023")
  write_csv(Tab18, "PMID_31086182_PartB_Phenotype_Processed.csv")
  R38 = joinGenoPheno("PMID_31086182_filereport_read_run_PRJEB31023_Genotype_Processed.csv",
                      "PMID_31086182_PartA_Phenotype_Processed.csv",
                      "secondary_sample_accession", "BioSample", 
                      outFilename = "PMID_31086182_PartA_Merged_Processed.csv")
  C38 = concatenateFiles(c("PMID_31086182_PartB_Phenotype_Processed.csv", 
                           "PMID_31086182_PartA_Merged_Processed.csv"),
                         "PMID_31086182_Merged_Processed.csv")
  setwd(paste0(WORKING_DIR, "Databases/PATRIC"))
  Qa = concatenateFiles(paste0(WORKING_DIR, "Mappings/", c("AllMappingsRemainingTables.csv", 
                                                           "AllMappingsSelectedTables.csv")), 
                        paste0(WORKING_DIR, "Mappings/", "AllMappingsTables.csv"))
  Qb = joinGenoPheno(paste0(WORKING_DIR, "Mappings/PATRICMappings.csv"), 
                            "PATRIC_genomes_AMR_Reduced.csv",
                            "ID", "biosample_accession", outFilename = "PATRIC_PartA_Merged_Processed.csv")
  Qc = read_csv("PATRIC_genomes_AMR_Reduced.csv", guess_max = Inf)
  Qd = Qc %>%
    filter(is.na(assembly_accession) & is.na(genbank_accessions) & is.na(refseq_accessions) & !is.na(biosample_accession))
  write_csv(Qd, "PATRIC_PartB_Phenotype_Processed.csv")
  Qc = Qc %>%
    filter(!(is.na(assembly_accession) & is.na(genbank_accessions) & is.na(refseq_accessions)))
  write_csv(Qc, "PATRIC_PartC_Merged_Processed.csv")
  Qe = joinGenoPheno(paste0(WORKING_DIR, "Mappings/", "AllMappingsTables.csv"),
                     "PATRIC_PartB_Phenotype_Processed.csv", "BIOSAMPLE", "biosample_accession",
                     outFilename = "PATRIC_PartB_Merged_Processed.csv")
  Qf = concatenateFiles(c("PATRIC_PartA_Merged_Processed.csv", "PATRIC_PartB_Merged_Processed.csv",
                          "PATRIC_PartC_Merged_Processed.csv"), "PATRIC_Merged_Processed.csv")
  Qf = read_csv("PATRIC_Merged_Processed.csv", guess_max = Inf) %>%
    select(-N, -Database, - file, -measurement) %>%
    distinct(.keep_all = TRUE)
  badIndices = which(duplicated(Qf %>% select(c(1:8, 11:46))))
  badAccessions = Qf %>% 
    slice(badIndices) %>%
    pull(biosample_accession)
  altAccessions1 = paste0(WORKING_DIR, "Databases/NDARO/StaphylococcusAureus.324.amr.metadata.filtered_Processed.csv") %>%
    read_csv(guess_max = Inf) %>%
    pull(biosample_acc)
  altAccessions2 = paste0(WORKING_DIR, "IntermediateFiles/PMID_35780211_Processed.csv") %>%
    read_csv(guess_max = Inf) %>%
    pull(Accession)
  stopifnot(all(badAccessions %in% altAccessions1 | badAccessions %in% altAccessions2))
  Qf = Qf %>%
    filter(!(biosample_accession %in% badAccessions))
  write_csv(Qf, "PATRIC_Merged_Processed.csv")
  # Qg = widenResistance(fname = "PATRIC_Merged_Processed.csv",
  #                     repColumns = c(1:7, 11:46), pivotColumn = 8, splitColumns = 9:10, dropColumns = FALSE)
  setwd(paste0(WORKING_DIR, "Databases/NARMS"))
  Qh = joinGenoPheno("NARMS_Campylobacter_filereport_read_run_PRJNA239251_Genotype_Processed.csv", 
                     "NARMS_Campylobacter_IsolateData.csv",
                     "sample_accession", "NCBI.Accession.Number", 
                     outFilename = "NARMS_Campylobacter_Merged_Processed.csv")
  Qi = joinGenoPheno("NARMS_Escherichia_and_Shigella_filereport_read_run_PRJNA218110_Genotype_Processed.csv", 
                     "NARMS_EscherichiaColiO157_IsolateData.csv",
                     "sample_accession", "NCBI.Accession.Number", 
                     outFilename = "NARMS_EscherichiaColi_Merged_Processed.csv")
  Qj = joinGenoPheno("NARMS_Escherichia_and_Shigella_filereport_read_run_PRJNA218110_Genotype_Processed.csv", 
                     "NARMS_Shigella_IsolateData.csv",
                     "sample_accession", "NCBI.Accession.Number", 
                     outFilename = "NARMS_Shigella_Merged_Processed.csv")
  Qk = joinGenoPheno("NARMS_All_Salmonella_filereport_read_run_PRJNA230403_Genotype_Processed.csv",
                     "NARMS_NonTyphoidalSalmonella_IsolateData.csv",
                     "sample_accession", "NCBI.Accession.Number", 
                     outFilename = "NARMS_NonTyphoidalSalmonella_Merged_Processed.csv")
  Ql = joinGenoPheno("NARMS_All_Salmonella_filereport_read_run_PRJNA230403_Genotype_Processed.csv",
                     "NARMS_TyphoidalSalmonella_IsolateData.csv",
                     "sample_accession", "NCBI.Accession.Number", 
                     outFilename = "NARMS_TyphoidalSalmonella_Merged_Processed.csv")
  setwd(paste0(WORKING_DIR, "Databases/pubMLST"))
  setwd(paste0(WORKING_DIR, "Databases/CDC"))
  Qn = read_csv("CDC_IsolateBank_MIC Data - All Panels.csv") %>%
    filter(!is.na())
  write_csv(Qn, "CDC_Phenotypes_Processed.csv")
  ### Initially only converted the accession numbers that were not found in other processed files; now do all! 
  Qp = convertAccessions("CDC_Phenotypes_Processed.csv", cName = "BioSampleAccessionNum", short = TRUE, 
                    previousMappingsFile = paste0(WORKING_DIR, "Mappings/CDCMappings.csv"))
  setwd(paste0(WORKING_DIR, "Databases/COMPARE_ML_AMR"))
  Qr = joinGenoPheno(paste0(WORKING_DIR, "Mappings/COMPAREMappings.csv"), 
                    "AllAntibiograms_WidenedRes_Processed.csv",
                    "ID", "bioSample_ID", outFilename = "AllAntibiograms_WidenedRes_PartA_Merged_Processed.csv")
  Qt = joinGenoPheno(paste0(WORKING_DIR, "Mappings/AllMappingsTables.csv"), 
                     "AllAntibiograms_WidenedRes_Processed.csv",
                     "BIOSAMPLE", "bioSample_ID", outFilename = "AllAntibiograms_WidenedRes_PartB_Merged_Processed.csv")
  Qu = concatenateFiles(c("AllAntibiograms_WidenedRes_PartA_Merged_Processed.csv", 
                          "AllAntibiograms_WidenedRes_PartB_Merged_Processed.csv"),
                        "COMPARE_ML_AMR_WidenedRes_Merged_Processed.csv")
  setwd(paste0(WORKING_DIR, "Databases/microreact"))
  Qv = read_csv("PMID_31107206_000269_2_Processed.csv")
  Qw = Qv %>%
    filter(!str_detect(`Strain name`, ACCESSIONS["RUN"]))
  write_csv(Qw, "PMID_31107206_000269_2_PartA_Phenotype_Processed.csv")
  Qv = Qv %>% 
    filter(str_detect(`Strain name`, ACCESSIONS["RUN"]))
  write_csv(Qv, "PMID_31107206_000269_2_PartB_Phenotype_Processed.csv")
  Qx = joinGenoPheno("PMID_31107206_filereport_read_run_PRJNA445436_Genotype_Processed.csv", 
                     "PMID_31107206_000269_2_PartA_Phenotype_Processed.csv",
                     "sample_accession", "Biosample", outFilename = "PMID_31107206_PartA_Merged_Processed.csv")
  Qy = concatenateFiles(c("PMID_31107206_PartA_Merged_Processed.csv", 
                          "PMID_31107206_000269_2_PartB_Phenotype_Processed.csv"),
                        "PMID_31107206_Merged_Processed.csv")
  setwd(paste0(WORKING_DIR, "Databases/PathogenWatch"))
  aTab = read_csv("pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_Merged_Processed.csv")
  bTab = aTab %>% 
    filter(!is.na(`ENA RUN`))
  write_csv(bTab, "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartA_Phenotypes_Processed.csv")
  cTab = aTab %>%
    filter(!is.na(`SAMPLE RUN ACCESSION ERS`))
  write_csv(cTab, "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartB_Phenotypes_Processed.csv")
  S1 = joinGenoPheno("pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_Merged_Mapped.csv", 
                     "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartA_Phenotypes_Processed.csv",
                     "ID", "ENA RUN", 
                     outFilename = "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartA_Mapped_Processed.csv")
  S2 = joinGenoPheno("pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_Merged_Mapped.csv",
                     "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartB_Phenotypes_Processed.csv",
                     "ID", "SAMPLE RUN ACCESSION ERS", 
                     outFilename = "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartB_Mapped_Processed.csv")
  C39 = concatenateFiles(c("pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartA_Mapped_Processed.csv",
                           "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_PartB_Mapped_Processed.csv"),
                         "pathogenwatch-saureus-4o6bnl6rl7bf-tong-et-al-2015_Merged_Mapped_Processed.csv")
  setwd(initDir)
}

preprocessPMID31005733 = function() {
  Tab = read_csv("../SelectedTables/PMID_31005733_SraRunTable.csv", guess_max = Inf) %>% 
    mutate(BioSampleShort = as.integer(str_sub(BioSample, 5))) %>%
    filter(BioSampleShort >= 9400893 & BioSampleShort <= 9401217) %>%
    select(-BioSampleShort)
  Tab1 = Tab %>%
    filter(Organism == "Staphylococcus aureus") %>%
    mutate(Methicillin = "R")
  Tab2 = Tab %>%
    filter(Organism == "Enterococcus faecium") %>%
    mutate(Vancomycin = "R")
  write_csv(Tab1, file = "../SelectedTables/PMID_31005733_SAureus_Processed.csv")
  write_csv(Tab2, file = "../RemainingTables/PMID_31005733_EFaecium_Processed.csv")
}

preprocessPMID24509479 = function() {
  Tab = read_csv("../SelectedTables/PMID_24509479_NIHMS56749-supplement-2_Processed.csv", guess_max = Inf) %>%
    mutate(`B-lactam_susceptibility/MIC(_g/mL)` = na_if(`B-lactam_susceptibility/MIC(_g/mL)`, "N/A")) %>%
    separate(`B-lactam_susceptibility/MIC(_g/mL)`, into = c("B-lactam_susceptibility", "B-lactam MIC (mg/L)"), 
             sep = "/", fill = "right")
  write_csv(Tab, "../SelectedTables/PMID_24509479_NIHMS56749-supplement-2_Processed.csv")
}

getIPCDInfo = function() {
  isolateIDs = read_csv("PMID_33875431_Merged_Phenotype_Processed.csv") %>% 
    pull(Clinical_isolate_IDs) %>%
    magrittr::extract(1:236)
  URLs = paste0("https://ipcd.ibis.ulaval.ca/index/display-detail/id/", isolateIDs, 
                "/criteria/IsolateID/text_search/", isolateIDs, "#tab-2")
  URLs
}

getCrypticInfo = function() {
  allStudies = read_csv("DOI_10.1101:2021.09.14.460274_Component_Projects.csv") %>% ### Found by searching ENA
    pull(id)
  for (study in allStudies) {
    cur = downloadStudyDetails(study, prefix = "DOI_10.1101:2021.09.14.460274_")
  }
  LF = list.files(pattern = "_Details.csv")
  W = concatenateFiles(LF, outFilename = "DOI_10.1101:2021.09.14.460274_Genotype_Merged_Processed.csv")
  W
}


splitFileByColumn = function(fname = "../SelectedTables/PMID_33659219_Merged_Processed.csv", Col = "taxon_name") {
  Tab = read_csv(fname, guess_max = Inf)
  splitVar = Tab %>%
    select(one_of(Col)) %>%
    pull()
  splitTab = Tab %>%
    split(splitVar)
  splitNames = names(splitTab) %>%
    str_split_fixed(" ", n = 2) %>%
    set_colnames(c("Genus", "Species")) %>%
    as_tibble() %>%
    mutate_at("Genus", ~{str_sub(., 1, 1)}) %>%
    mutate_at("Species", ~{str_to_title(.)}) %>%
    mutate(Abbr = paste0(Genus, Species)) %>%
    pull(Abbr)
  splitFNames = str_replace(rep(fname, length(splitNames)), "_Processed", paste0("_", splitNames, "_Processed"))
  for (ind in 1:length(splitNames)) {
    write_csv(splitTab[[ind]], splitFNames[ind])
  }
  splitTab
}

splitResistanceAll = function() {
  initDir = getwd()
  setwd(paste0(WORKING_DIR, "RemainingTables"))
  S1 = splitResistance("PMID_23299977_Merged_Processed.csv", "Resistance profile", "susceptible")
  S2 = widenResistance("PMID_31266463_Merged_Processed.csv", 1:2, 3, 4:8, dropColumns = TRUE)
  setwd(paste0(WORKING_DIR, "SelectedTables"))
  S3 = splitResistance("PMID_33021437_Merged_Processed.csv", "Phenotypic resistance profile", "—", "\\*")
  S4 = splitResistance("PMID_32562543_Merged_Processed.csv", "Resistance profile", "susceptible", "\\.")
  S5 = widenResistance("PMID_30333126_a4ed1601ccb774eaf3b9c5f44b1a6bc3_JCM.01260-18-s0002_Processed.csv", 1:6, 7, 8, dropColumns = TRUE)
  S6 = widenResistance("PMID_29323230_41598_2017_18972_MOESM2_ESM_Processed.csv", 1:3, 4, 5, dropColumns = TRUE)
  setwd(initDir)
}
