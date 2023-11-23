preprocessAllData = function() {
  initDir = getwd()
  setwd(WORKING_DIR)
  Res = readLines("AllFoundPapers")
  Res = Res[Res != '']
  Res %<>% str_remove_all("\t") %>% 
    str_remove_all("https://") %>% 
    str_split_fixed(" - ", 2) %>%
    str_remove_all("sequences") %>% 
    str_trim()
  ResU = matrix(Res, ncol = 2) %>%
    set_colnames(c("URL", "Contents")) %>% 
    as_tibble() %>% 
    separate(Contents, into = c("Size", "Species"), sep = " ", extra = "merge") %>% 
    arrange(URL) %>% 
    mutate_at("Size", as.integer) %>%
    separate("Species", sep = "\\[", into = c("Species", "Context"), fill = "right") %>% 
    mutate_at("Context", ~{str_remove(., "\\]")}) %>%
    mutate_at("Context", ~{replace_na(., "human")}) %>%
    mutate_at("Species", ~{str_remove_all(.,"\\;")}) %>% 
    mutate_at("Species", ~{str_trim(.)})
  ResU %<>% arrange(Species, -Size) %>%
    mutate(GenoData = NA) %>%
    mutate(PhenoData = NA) %>%
    group_by(Species) %>%
    mutate(N = n()) %>%
    ungroup() %>%
    filter(N > 1) %>%
    mutate(UpdatedSize = Size)
  print(paste("Found", nrow(ResU) + 1, "datasets in", n_distinct(ResU$URL) + 1, "papers"))
  write_csv(ResU, "FoundPapers.csv")
  miniRes = ResU %>%
    group_by(Species) %>%
    slice(1:10) %>%
    ungroup()
  miniRes %<>%
    arrange(desc(URL)) %>%
    mutate(N = 1:nrow(miniRes), .before = 1)
  write_csv(miniRes, "SelectedPapers.csv")
  Rest = ResU %>%
    group_by(Species) %>%
    slice(-(1:10)) %>%
    ungroup()
  Rest %<>%    
    arrange(desc(URL)) %>%
    mutate(N = 1:nrow(Rest), .before = 1)
  write_csv(Rest, "RemainingPapers.csv")
  miniRes$GenoData[1]   = "https://www.ncbi.nlm.nih.gov/bioproject/PRJ[EB26000/NA343736/NA526078]"
  miniRes$PhenoData[1]  = "https://github.com/aggreen/MTB-CNN/blob/main/input_data/master_table_resistance.csv"
  miniRes$GenoData[2]   = "https://www.microbiologyresearch.org/deliver/fulltext/mgen/8/6/000837_2.xlsx"
  miniRes$PhenoData[2]  = "https://microreact.org/project/mvgn3EvNgmxAcPjPBnyFMj/a32e0dc6"
  miniRes$GenoData[3]   = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA809954/"
  miniRes$GenoData[4]   = "https://www-microbiologyresearch-org.iclibezp1.cc.ic.ac.uk/deliver/fulltext/jmm/71/6/001551_2.xlsx"
  miniRes$PhenoData[4]  = miniRes$GenoData[4]
  miniRes$UpdatedSize[4] = 50L
  miniRes$GenoData[5]   = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9161033/bin/Table_2.xlsx"
  miniRes$GenoData[6]   = "https://github.com/YunxiaoRen/Multi_Label-Classification/blob/main/data.zip"
  miniRes$GenoData[7]   = "https://ars.els-cdn.com/content/image/1-s2.0-S0924857922000553-mmc1.docx"
  miniRes$GenoData[8]   = "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fhel.12879&file=hel12879-sup-0001-TableS1-S9.docx"
  miniRes$PhenoData[9]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8843102/bin/TEMI_A_2032373_SM2438.zip"
  miniRes$GenoData[10]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8753810/bin/12864_2022_8291_MOESM1_ESM.docx"
  miniRes$GenoData[11:15] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8657983/bin/ijms-22-13049-s001.zip"
  miniRes$PhenoData[11:15] = miniRes$GenoData[11]
  miniRes$GenoData[16]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8635091/bin/Table_1.XLSX"
  miniRes$PhenoData[16] = miniRes$GenoData[16]
  miniRes$GenoData[17]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA690682"
  miniRes$GenoData[18]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8765299/bin/aac.01736-21-s0001.pdf"
  miniRes$GenoData[19]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA771404"
  miniRes$PhenoData[19] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8765321/bin/aac.01196-21-s0002.xlsx"
  miniRes$GenoData[20]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8765234/bin/aac.01370-21-s0003.xlsx"
  miniRes$PhenoData[20] = miniRes$GenoData[20]
  miniRes$UpdatedSize[20] = 1354L
  miniRes$GenoData[21]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8518998/bin/Table_1.XLSX"
  miniRes$PhenoData[21] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8518998/bin/Table_4.XLSX"
  miniRes$GenoData[22]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA556442"
  miniRes$PhenoData[22] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8765267/bin/aac.00714-21-s0002.xlsx" 
  miniRes$UpdatedSize[22] = 61L
  miniRes$GenoData[23]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8627213/bin/mgen-7-0635-s002.xlsx"
  miniRes$PhenoData[23] = miniRes$GenoData[23]
  miniRes$GenoData[24]  = "https://raw.githubusercontent.com/GaryNapier/tb-lineages/main/samples_github.csv"
  miniRes$GenoData[25]  = "https://microreact.org/project/GPS_Argentina"
  miniRes$PhenoData[25] = miniRes$GenoData[25]
  miniRes$GenoData[26]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA691075"
  miniRes$PhenoData[26] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8471365/bin/pathogens-10-01178-s001.zip"
  miniRes$UpdatedSize[26] = 10L
  miniRes$GenoData[27]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA736314"
  miniRes$PhenoData[27] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8454963/bin/pone.0249617.s001.xlsx"
  miniRes$GenoData[28]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8444031/bin/jamanetwopen-e2125203-s001.pdf"
  miniRes$GenoData[29]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB3084"
  miniRes$GenoData[30]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA628943"
  miniRes$PhenoData[30] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8422772/bin/12915_2021_1094_MOESM1_ESM.xlsx"
  miniRes$GenoData[31]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8407037/bin/13073_2021_953_MOESM1_ESM.csv"
  miniRes$PhenoData[31] = miniRes$GenoData[31]
  miniRes$GenoData[32]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB23294"
  miniRes$GenoData[33]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[413593/283583/401515/302362/390471]"
  miniRes$PhenoData[33] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6121966/bin/NEJMoa1800474_Walker_SuppApp.pdf"
  miniRes$UpdatedSize[33] = 10209L
  miniRes$GenoData[34]  = "https://www.gastrojournal.org/article/S0016-5085(21)03246-7/fulltext#supplementaryMaterial"
  miniRes$GenoData[35]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA576513"
  miniRes$PhenoData[35] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8224912/bin/pone.0253797.s003.xlsx"
  miniRes$GenoData[36]  = "https://microreact.org/api/files/raw?56b547d41c6fcf579d6dfba00a45725b64c29bed"
  miniRes$PhenoData[36] = miniRes$GenoData[36]
  miniRes$GenoData[37]  = "https://www.ncbi.nlm.nih.gov/nuccore/MT022086"
  miniRes$GenoData[38]  = "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fzph.12853&file=zph12853-sup-0001-TableS1.xlsx"
  miniRes$UpdatedSize[38] = 118L
  miniRes$GenoData[39]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA648026"
  miniRes$GenoData[40]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8128892/bin/41467_2021_23091_MOESM4_ESM.xlsx"
  miniRes$PhenoData[40] = "https://gitlab.com/cgps/pathogenwatch/publications/-/blob/master/styphi/benchmark_AMR/Table1/AST_data_from_pubs_1316.csv" 
  miniRes$UpdatedSize[40] = 1316L
  miniRes$GenoData[41]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB40974"
  miniRes$PhenoData[41] = "https://www.liebertpub.com/doi/suppl/10.1089/mdr.2020.0603/suppl_file/Supp_TableS3.docx"
  miniRes$UpdatedSize[41] = 37L
  miniRes$GenoData[42]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA675435"
  miniRes$PhenoData[42] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8058101/bin/Table_1.XLSX"
  miniRes$GenoData[43]  = "https://ars.els-cdn.com/content/image/1-s2.0-S0012369221007042-mmc1.pdf"
  miniRes$GenoData[44]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[325248/326244/294726/649313]"
  miniRes$PhenoData[44] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8218647/bin/aac.02696-20-s0001.pdf"
  miniRes$GenoData[45]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8054416/bin/13073_2021_858_MOESM1_ESM.pdf"
  miniRes$GenoData[46]  = "https://db.cngb.org/search/project/CNP0000099/"
  miniRes$GenoData[47:51] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7917081/bin/Table_1.xlsx"
  miniRes$PhenoData[47:51] = miniRes$GenoData[47]
  miniRes$GenoData[52]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7928456/bin/pntd.0009207.s003.xlsx"
  miniRes$GenoData[54]  =  "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA645794"
  miniRes$GenoData[55]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA574097"
  miniRes$GenoData[56]  = "https://www.researchgate.net/profile/Balaji-Veeraraghavan/publications"
  miniRes$GenoData[57:58] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7595632/bin/pcbi.1008319.s002.xlsx"
  miniRes$PhenoData[57:58] = miniRes$GenoData[57]
  miniRes$GenoData[59]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[597427/449293]"
  miniRes$PhenoData[59] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7568433/bin/giaa110_supplemental_files.zip"
  miniRes$UpdatedSize[59] = 113L
  miniRes$GenoData[60]  = "https://www.liebertpub.com/doi/suppl/10.1089/mdr.2020.0184/suppl_file/Supp_TableS1.docx"
  miniRes$PhenoData[60] = "https://www.liebertpub.com/doi/10.1089/mdr.2020.0184#tb1"
  miniRes$UpdatedSize[60] = 95L
  miniRes$GenoData[61]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB[40238/31119]"
  miniRes$PhenoData[61] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7979593/bin/10096_2020_4043_MOESM1_ESM.xlsx"
  miniRes$GenoData[63]  = "https://pubmlst.org/bigsdb?db=pubmlst_campylobacter_isolates"
  miniRes$PhenoData[63] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7438411/bin/Table_2.XLSX"
  miniRes$GenoData[64]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6663919/bin/NIHMS1022822-supplement-Table_1.xlsx"
  miniRes$GenoData[65]  = "https://figshare.com/articles/dataset/Meningitis_bacterial_GWAS_metadata/5915314?file=25587806"
  miniRes$GenoData[66]  = "https://microreact.org/project/5DEFpeck4"
  miniRes$GenoData[67]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7443737/bin/dkaa221_supplementary_data.docx"
  miniRes$PhenoData[67] = miniRes$GenoData[67]
  miniRes$GenoData[68]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7237477/bin/41598_2020_64934_MOESM1_ESM.xlsx"
  miniRes$PhenoData[68] = miniRes$GenoData[68]
  miniRes$GenoData[69]  = miniRes$GenoData[33]
  miniRes$PhenoData[69] = miniRes$PhenoData[33]
  miniRes$UpdatedSize[69] = miniRes$UpdatedSize[33]
  miniRes$GenoData[70]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA186035"
  miniRes$PhenoData[70] = "https://www.liebertpub.com/doi/suppl/10.1089/mdr.2019.0477/suppl_file/Suppl_TableS2.pdf"
  miniRes$PhenoData[71:72] = "https://www.ebi.ac.uk/ena/pathogens/datahubs#dcc_schubert"
  miniRes$GenoData[73]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA540750"
  miniRes$PhenoData[73] = "https://github.com/TimothyJDavies/reconciling_the_potentially_irreconcilable/blob/master/raw_data/bd_phoenix_mics.csv"
  miniRes$GenoData[74]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA521038"
  miniRes$GenoData[75]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7034955/bin/pone.0229416.s002.xlsx"
  miniRes$GenoData[76]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8071603/bin/NIHMS1604968-supplement-sTable_1.xlsx"
  miniRes$GenoData[77]  = "https://www.ncbi.nlm.nih.gov/sra/PRJNA526797"
  miniRes$PhenoData[77] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7059009/bin/EMMM-12-e10264-s003.xlsx"
  miniRes$GenoData[78]  = "https://www.ncbi.nlm.nih.gov/sra/PRJEB32286"
  miniRes$PhenoData[78] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7177496/bin/dkz570_supplementary_data.zip"
  miniRes$GenoData[79]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9341236/bin/NIHMS1823000-supplement-Supplement_2_-_Data.xlsx"
  miniRes$GenoData[80]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7038236/bin/AAC.02005-19-sd004.xlsx"
  miniRes$PhenoData[80] = miniRes$GenoData[80]
  miniRes$UpdatedSize[80] = 431L
  miniRes$GenoData[81]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA528114"
  miniRes$PhenoData[81] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6881056/bin/pntd.0007868.s004.xlsx"
  miniRes$UpdatedSize[81] = 13L
  miniRes$GenoData[82]  = "https://www.frontiersin.org/articles/10.3389/fgene.2019.00922/full#h11"
  miniRes$GenoData[83]  = "https://www.clinicalmicrobiologyandinfection.com/cms/10.1016/j.cmi.2019.09.008/attachment/42fdbb19-fe3e-4be5-a878-23ff641f9dac/mmc2.xlsx"
  miniRes$GenoData[84:86] = "https://doi.org/10.1371/journal.pcbi.1007349.s007"
  miniRes$PhenoData[84:86] = miniRes$GenoData[84]
  miniRes$GenoData[88]  = miniRes$GenoData[36]
  miniRes$PhenoData[88] = miniRes$PhenoData[36]    
  miniRes$GenoData[89]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6591855/#MOESM2"
  miniRes$GenoData[90]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA475751"
  miniRes$PhenoData[90] = "VRE"
  miniRes$GenoData[91]  = "https://doi.org/10.6084/m9.figshare.4822255"
  miniRes$GenoData[92]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6437544/bin/AAC.02602-18-s0002.xlsx"
  miniRes$PhenoData[92] = miniRes$GenoData[92]
  miniRes$GenoData[93]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6331587/bin/41467_2018_7997_MOESM3_ESM.xlsx"
  miniRes$PhenoData[93] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6331587/bin/41467_2018_7997_MOESM4_ESM.xlsx"
  miniRes$GenoData[94]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6351930/bin/jcm-08-00053-s001.docx"
  miniRes$GenoData[95]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6310291/bin/pcbi.1006258.s010.csv"
  miniRes$PhenoData[95] = miniRes$GenoData[95]
  miniRes$GenoData[96]  = miniRes$GenoData[64]
  miniRes$PhenoData[96] = miniRes$GenoData[64]
  miniRes$GenoData[97]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6232905/bin/Table_2.XLSX"
  miniRes$GenoData[98]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6355527/bin/a4ed1601ccb774eaf3b9c5f44b1a6bc3_JCM.01260-18-s0002.xlsx"
  miniRes$PhenoData[98] = miniRes$GenoData[98]
  miniRes$GenoData[99]  = miniRes$GenoData[33]
  miniRes$PhenoData[99] = miniRes$PhenoData[33]
  miniRes$GenoData[101]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA445516"
  miniRes$GenoData[102]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB9227"
  miniRes$PhenoData[102] = "https://pathogen.watch/collection/eurogasp2013"
  miniRes$GenoData[103]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5933809/bin/pntd.0006408.s004.xlsx"
  miniRes$PhenoData[103] = miniRes$GenoData[103]
  miniRes$GenoData[104]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5880904/bin/Table_1.xlsx"
  miniRes$GenoData[105]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5765115/bin/41598_2017_18972_MOESM2_ESM.xlsx"
  miniRes$PhenoData[105] = miniRes$GenoData[105]
  miniRes$GenoData[107]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5605940/bin/mbo004173493st1.pdf"
  miniRes$PhenoData[108] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5558719/bin/12864_2017_4017_MOESM1_ESM.csv" 
  miniRes$GenoData[109]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA325248"
  miniRes$GenoData[110]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA376414"
  miniRes$GenoData[111]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5890716/bin/dkx067_supp.docx"
  miniRes$PhenoData[111] = miniRes$GenoData[111]
  miniRes$GenoData[112]  = "https://www.clinicalmicrobiologyandinfection.com/cms/10.1016/j.cmi.2017.02.021/attachment/3ee2ba25-2a7a-43a6-b60c-7c2282811c5b/mmc1.docx"
  miniRes$GenoData[113]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA350274"
  miniRes$GenoData[114]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB[2090/2999/7904]"
  miniRes$PhenoData[114] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5091375/bin/supp_jiw420_jiw420supp.docx"
  miniRes$GenoData[115]  = "https://www.clinicalmicrobiologyandinfection.com/cms/10.1016/j.cmi.2016.08.001/attachment/60cddab3-b90a-4070-acf3-4752e37bfef3/mmc1.docx"
  miniRes$GenoData[116]  = "http://www.ncbi.nlm.nih.gov/bioproject/315363"
  miniRes$GenoData[117]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4540916/bin/JCM.01357-15_zjm999094462so1.pdf"
  miniRes$PhenoData[117] = miniRes$GenoData[117]
  miniRes$UpdatedSize[117] = 21L
  miniRes$GenoData[118]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA264310"
  miniRes$PhenoData[118] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291382/bin/AAC.03954-14_zac001153606sd2.xlsx"
  miniRes$GenoData[119]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3970364/bin/NIHMS56749-supplement-2.xls"
  miniRes$PhenoData[119] = miniRes$GenoData[119]
  miniRes$GenoData[121]  = "https://microreact.org/project/kRW7Z2TLg3FEM7rmq8sZ1e"
  miniRes$PhenoData[121] = miniRes$GenoData[121]
  miniRes$GenoData[122]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA540750"
  miniRes$GenoData[123]  = "https://ftp.ebi.ac.uk/pub/databases/cryptic/release_june2022/reuse/CRyPTIC_reuse_table_20221019.csv"
  miniRes$PhenoData[123] = miniRes$GenoData[123]
  miniRes = rbind(miniRes, tibble(URL = "www.ncbi.nlm.nih.gov/pubmed/?term=33075053",
                            Size = 1999L,
                            Species = "S. enterica",
                            Context = "human",
                            N = nrow(miniRes) + 1,
                            GenoData = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7595632/bin/pcbi.1008319.s002.xlsx",
                            PhenoData = GenoData,
                            UpdatedSize = Size))
  miniRes = rbind(miniRes, tibble(URL = "www.ncbi.nlm.nih.gov/pubmed/?term=34884852",
                                  Size = 489L,
                                  Species = "Enterobacter spp.",
                                  Context = "human",
                                  N = nrow(miniRes) + 1,
                                  GenoData = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8657983/bin/ijms-22-13049-s001.zip",
                                  PhenoData = GenoData,
                                  UpdatedSize = Size))
  miniRes = rbind(miniRes, tibble(URL = "www.ncbi.nlm.nih.gov/pubmed/?term=34884852",
                                  Size = 222L,
                                  Species = "Serratia spp.",
                                  Context = "human",
                                  N = nrow(miniRes) + 1,
                                  GenoData = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8657983/bin/ijms-22-13049-s001.zip",
                                  PhenoData = GenoData,
                                  UpdatedSize = Size))
  write_csv(miniRes, "SelectedPapersWithDetails.csv")
  Rest$GenoData[1] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB48275"
  Rest$PhenoData[1] = "https://www.thelancet.com/cms/10.1016/S2666-5247(22)00116-1/attachment/3a8a18b9-ad66-4d7d-a6ce-5ea299ce796e/mmc2.xlsx"
  Rest$GenoData[2:3] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9226621/bin/Table_1.XLSX"
  Rest$PhenoData[2:3] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9226621/bin/Table_2.XLSX"
  Rest$GenoData[4] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJDB12075"
  Rest$PhenoData[4] = "https://www.microbiologyresearch.org/docserver/fulltext/mgen/8/5/mgen000827-f3.gif"
  Rest$UpdatedSize[4] = 21L
  Rest$GenoData[5] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB43284"
  Rest$GenoData[6] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA822663"
  Rest$PhenoData[6] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9241780/bin/spectrum.01594-21-s002.xlsx"
  Rest$UpdatedSize[6] = 23L
  Rest$GenoData[7] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA765107"
  Rest$GenoData[8] = "https://pubmlst.org/bigsdb?page=info&db=pubmlst_saureus_isolates&id=37650"
  Rest$GenoData[9] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8924536/bin/Table_2.XLSX"
  Rest$PhenoData[9] = Rest$GenoData[9]
  Rest$UpdatedSize[9] = 98L
  Rest$GenoData[10] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8941879/bin/spectrum01256-21_supp_2_seq12.xlsx"
  Rest$PhenoData[10] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8941879/figure/fig1/"
  Rest$UpdatedSize[10] = 54L
  Rest$GenoData[11] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA761409"
  Rest$GenoData[12] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA736718"
  Rest$GenoData[13] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB45389"
  Rest$GenoData[14] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB43270"
  Rest$PhenoData[14] = "RIF R"
  Rest$GenoData[15] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8851352/bin/hygsup.zip"
  Rest$PhenoData[15] = Rest$GenoData[15]
  Rest$UpdatedSize[15] = 78L
  Rest$PhenoData[16] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8613203/table/Tab2/?report=objectonly"
  Rest$PhenoData[17] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8568246/table/T2/?report=objectonly"
  Rest$PhenoData[18] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8586210/bin/DataSheet_1.docx"
  Rest$PhenoData[19] = "https://ndownloader.figstatic.com/files/31383010"
  Rest$GenoData[20] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA756552"
  Rest$PhenoData[20] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8558520/bin/Table_1.XLSX"
  Rest$PhenoData[21] = "https://www.dovepress.com/get_supplementary_file.php?f=331890.docx"
  Rest$PhenoData[22] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8529152/bin/Data_Sheet_1.docx"
  Rest$PhenoData[23] = "https://www.clinicalmicrobiologyandinfection.com/cms/10.1016/j.cmi.2021.09.014/attachment/b32254dc-c8b4-4e3d-80bb-ae0b04cc9029/mmc1.docx"
  Rest$GenoData[24] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB32666/"
  Rest$GenoData[25] = "https://www.ncbi.nlm.nih.gov/bioproject/[PRJNA733242/PRJCA004763]"
  Rest$PhenoData[25] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8715440/bin/mgen-7-0610-s002.xlsx"
  Rest$GenoData[26] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA395086"
  Rest$PhenoData[26] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8547452/bin/msystems.00194-21-st001.pdf"
  Rest$GenoData[27] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA376414"
  Rest$GenoData[28] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8715434/bin/mgen-7-0629-s001.pdf"
  Rest$GenoData[30] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA564992"
  Rest$GenoData[31] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA744410"
  Rest$GenoData[32] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA656305"
  Rest$GenoData[33] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[390471/613706/598981]"
  Rest$PhenoData[33] = "https://ars.els-cdn.com/content/image/1-s2.0-S0924857921001448-mmc2.xlsx"
  Rest$PhenoData[34] = "https://ars.els-cdn.com/content/image/1-s2.0-S0740002021000861-mmc1.xlsx"
  Rest$GenoData[35] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA494909"
  Rest$GenoData[36] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8209723/bin/mgen-7-0565-s002.xlsx"
  Rest$GenoData[37] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB42355"
  Rest$GenoData[38] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA639216"
  Rest$PhenoData[38] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8209721/bin/mgen-7-0573-s001.pdf"
  Rest$GenoData[39] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA487590"
  Rest$GenoData[40] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA681718"
  Rest$PhenoData[40] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8316078/bin/jcm.00202-21-s0001.pdf"
  Rest$UpdatedSize[40] = 86L
  Rest$GenoData[41] = "https://ars.els-cdn.com/content/image/1-s2.0-S0963996921000971-mmc1.xlsx"
  Rest$GenoData[42] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA415194"
  Rest$GenoData[44] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8190621/bin/mgen-7-0531-s001.pdf"
  Rest$PhenoData[44] = Rest$GenoData[44]
  Rest$UpdatedSize[44] = 41L
  Rest$GenoData[45] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA630106"
  Rest$GenoData[46] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA561696"
  Rest$GenoData[47] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7785249/bin/pcbi.1008518.s022.csv"
  Rest$PhenoData[47] = Rest$GenoData[47]
  Rest$GenoData[48] = "https://www.researchgate.net/profile/Balaji-Veeraraghavan/publications"
  Rest$GenoData[49] = "https://www.researchgate.net/profile/Balaji-Veeraraghavan/publications"
  Rest$GenoData[51] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7595632/bin/pcbi.1008319.s002.xlsx"
  Rest$PhenoData[51] = Rest$GenoData[51]
  Rest$GenoData[53] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA650381"
  Rest$PhenoData[53] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7771457/bin/JCM.00583-20-s0006.xlsx"
  Rest$GenoData[55] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7549793/bin/ppat.1008357.s009.xlsx"
  Rest$GenoData[56] = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB[31023/25999]"
  Rest$PhenoData[56] = "https://erj.ersjournals.com/highwire/filestream/165891/field_highwire_adjunct_files/0/ERJ-02338-2020.Tables.xlsx"
  Rest$GenoData[57]  = "https://github.com/patbaa/AMR_ciprofloxacin/blob/master/meta.tsv"
  Rest$PhenoData[57] = "https://github.com/patbaa/AMR_ciprofloxacin/blob/master/supplementary_meta_table.csv"
  Rest$GenoData[60]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7343994/table/tab3/"
  Rest$GenoData[61]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA600338"
  Rest$GenoData[62]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA594714"
  Rest$GenoData[63]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA565946"
  Rest$GenoData[64]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7253370/bin/mSystems.00656-19-sd001.xlsx"
  Rest$PhenoData[64] = Rest$GenoData[64]
  Rest$GenoData[65]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8094441/bin/NIHMS1584152-supplement-1.xlsx"
  Rest$PhenoData[65] = Rest$GenoData[65]
  Rest$PhenoData[66] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7228079/bin/pone.0232543.s001.xlsx"
  Rest$PhenoData[68] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7164301/bin/40249_2020_652_MOESM1_ESM.xlsx"
  Rest$GenoData[69]  = "https://www.microbiologyresearch.org/content/journal/jmm/10.1099/jmm.0.001146#supplementary_data"
  Rest$PhenoData[69] = Rest$GenoData[69]
  Rest$GenoData[70]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB34425"
  Rest$PhenoData[70] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7382555/figure/dkaa040-F2/"
  Rest$UpdatedSize[70] = 0L
  Rest$PhenoData[71] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7034084/figure/F0003/"
  Rest$PhenoData[72] = Rest$PhenoData[71]
  Rest$GenoData[73]  = "https://www.ncbi.nlm.nih.gov/bioproject/SRP128089"
  Rest$PhenoData[73] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6974034/bin/pmed.1003008.s005.xlsx"
  Rest$UpdatedSize[73] = 0L
  Rest$GenoData[74]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA376414"
  Rest$PhenoData[74] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6952207/bin/mSphere.00918-19-st001.docx"
  Rest$GenoData[76]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6834686/bin/Data_Sheet_1.ZIP"
  Rest$PhenoData[76] = Rest$GenoData[76]
  Rest$GenoData[77]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6798173/bin/Table_1.csv"
  Rest$PhenoData[77] = "3GC-R"
  Rest$UpdatedSize[77] = 112L
  Rest$GenoData[78]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJDB8553"
  Rest$GenoData[79]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6796477/bin/12866_2019_1598_MOESM1_ESM.xlsx"
  Rest$GenoData[80]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB4024"
  Rest$PhenoData[80] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6817357/bin/EMS83238-supplement-Supplementary_Table_1.xlsx"
  Rest$GenoData[81]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB29576"
  Rest$PhenoData[81] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6710905/bin/mmc2.docx"
  Rest$GenoData[82]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB10999"
  Rest$PhenoData[82] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6604184/bin/12866_2019_1520_MOESM1_ESM.xlsx"
  Rest$GenoData[83]  = "https://www.ncbi.nlm.nih.gov/bioproject/ERS2958092"
  Rest$GenoData[84]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6513847/bin/41467_2019_10110_MOESM4_ESM.xlsx"
  Rest$PhenoData[84] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6513847/bin/41467_2019_10110_MOESM7_ESM.xlsx"
  Rest$GenoData[85]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA475751"
  Rest$PhenoData[85] = "MRSA"
  Rest$GenoData[87]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA244279"
  Rest$PhenoData[87] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6211763/bin/pcbi.1006434.s005.xlsx"
  Rest$GenoData[88]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB12513"
  Rest$GenoData[89]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5890773/bin/dkx405_supplementary_data1.docx"
  Rest$PhenoData[89] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5890773/bin/dkx405_supplementary_data2.xlsx"
  Rest$GenoData[90]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5654935/bin/mbo005173554st2.pdf"
  Rest$GenoData[91]  = "https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/jac/72/12/10.1093_jac_dkx301/1/supplementary_data_dkx301.xlsx"
  Rest$PhenoData[91] = Rest$GenoData[91]
  Rest$PhenoData[92] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5434337/bin/cix169_suppl_Supp_Table1.xlsx"
  Rest$GenoData[94]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA306133"
  Rest$PhenoData[94] = "https://static-content.springer.com/esm/art%3A10.1038%2Fnmicrobiol.2016.41/MediaObjects/41564_2016_BFnmicrobiol201641_MOESM201_ESM.xlsx"
  Rest$GenoData[95]  = Rest$GenoData[94]
  Rest$PhenoData[95] = Rest$PhenoData[94]
  Rest$GenoData[97]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4937209/bin/mbo003162861st1.pdf"
  Rest$GenoData[98]  = "https://www.ncbi.nlm.nih.gov/bioproject/SRP034661"
  Rest$PhenoData[98] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4958182/bin/AAC.00075-16_zac008165389so1.pdf"
  Rest$GenoData[99]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB2478"
  Rest$PhenoData[99] = "https://microreact.org/project/EkUvg9uY?tt=rc"
  Rest$GenoData[100] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4844716/bin/JCM.03195-15_zjm999094941so1.pdf"
  Rest$PhenoData[100] = Rest$GenoData[100]
  Rest$GenoData[101]  = "http://www.broadinstitute.org/annotation/genome/mtb_drug_resistance.1/DirectedSequencingHome.html"
  Rest$GenoData[102]  = "https://www.ncbi.nlm.nih.gov/bioproject/[PRJEB5162/PRJEB6273/PRJEB7727/PRJNA282721]"
  Rest$PhenoData[102] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4703848/bin/ncomms10063-s[7/8/9/10].txt"
  Rest$GenoData[103]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB5261"
  Rest$PhenoData[103] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4703848/bin/ncomms10063-s[2/3/4].txt"
  Rest$GenoData[104]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[268101/302362]"
  Rest$PhenoData[104] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4698465/bin/mmc1.pdf" 
  Rest$GenoData[105]  = "https://www.ncbi.nlm.nih.gov/nuccore/KJ803828:KJ803829[pacc]"
  Rest$GenoData[106]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA[183624/235615]"
  Rest$PhenoData[106] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4587932/bin/pmed.1001880.s006.pdf"
  Rest$GenoData[107]  = "https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/jac/70/10/10.1093/jac/dkv186/2/dkv186supp.xls"
  Rest$PhenoData[107] = Rest$GenoData[107]
  Rest$GenoData[108]  = "https://www.ncbi.nlm.nih.gov/bioproject/[PRJEB5162/PRJEB6273/PRJEB7727/PRJNA282721]"
  Rest$PhenoData[108] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4579482/bin/mmc2.xlsx"
  Rest$GenoData[110]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4432642/bin/ncomms8119-s2.xlsx"
  Rest$PhenoData[110] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4432642/bin/ncomms8119-s5.xlsx"
  Rest$GenoData[112]  = "https://www.ncbi.nlm.nih.gov/bioproject/DRA001219"
  Rest$GenoData[113]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA266539"
  Rest$PhenoData[113] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4290921/bin/JCM.02589-14_zjm999093959so3.xlsx"
  Rest$UpdatedSize[113] = 174L
  Rest$GenoData[114]  = "https://www.ncbi.nlm.nih.gov/bioproject/SRP042632"
  Rest$PhenoData[114] = "https://genome.cshlp.org/content/suppl/2014/11/11/gr.180190.114.DC1/Dataset_S1.xlsx"
  Rest$GenoData[115]  = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4040999/bin/supp_evu092_Supplementary_Tables.doc"
  Rest$PhenoData[115] = Rest$GenoData[115]
  Rest$UpdatedSize[115] = 49L
  Rest$GenoData[116]  = Rest$PhenoData[94]
  Rest$PhenoData[116] = Rest$GenoData[116]
  Rest$UpdatedSize[116] = 992L
  Rest$GenoData[117]  = "https://www.ncbi.nlm.nih.gov/bioproject/ERP000192"
  Rest$PhenoData[117] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3939361/bin/NIHMS56217-supplement-3.xls"
  Rest$GenoData[118]  = "https://www.ncbi.nlm.nih.gov/bioproject/PRJEB[2090/2999/7904]"
  Rest$PhenoData[118] = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4030102/bin/mmc1.pdf"
  Rest$GenoData[121]  = "https://www.ncbi.nlm.nih.gov/bioproject/ERP[000150/000633]"
  Rest$PhenoData[121] = "https://genome.cshlp.org/content/suppl/2013/02/04/gr.147710.112.DC1/Supp_TableS1.xls"
  Rest = rbind(Rest, tibble(URL = "www.ncbi.nlm.nih.gov/pubmed/?term=35086603", 
                            Size = 217L, 
                            Species = "M. tuberculosis", 
                            Context = "human", 
                            N = nrow(Rest) + 1, 
                            GenoData = "https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S095026882100279X/resource/name/S095026882100279Xsup001.xlsx", 
                            PhenoData = GenoData,
                            UpdatedSize = 78L))
  write_csv(Rest, "RemainingPapersWithDetails.csv")
  finalData = bind_rows(mutate(miniRes, topTen = TRUE), mutate(Rest, topTen = FALSE)) %>%
    filter(!is.na(GenoData) & !is.na(PhenoData))
  print(paste("There are", nrow(finalData), "fully specified datasets in", n_distinct(finalData$URL), "papers"))
  finalData %<>%
    filter(UpdatedSize >= Size | (UpdatedSize >= 75 & Species != "M. tuberculosis") | 
             (UpdatedSize >= 150 & Species == "M. tuberculosis") | UpdatedSize == 0)
  print(paste("There are", nrow(finalData), "datasets of sufficient size in", n_distinct(finalData$URL), "papers"))
  finalData %<>%
    group_by(Species, GenoData, PhenoData) %>%
    slice(1) %>%
    ungroup()
  print(paste("There are", nrow(finalData), "deduplicated datasets in", n_distinct(finalData$URL), "papers"))
  finalData %<>%
    filter(UpdatedSize > 0)
  print(paste("There are", nrow(finalData), "datasets with enough matching IDs in", n_distinct(finalData$URL), "papers"))
  finalData %<>% 
    mutate(PMID = str_sub(URL, -8, -1)) %>%
    mutate_at("PMID", ~{.[str_sub(URL, 1, 3) == "doi"] = str_sub(URL[str_sub(URL, 1, 3) == "doi"], 9); .})
  # Adding information about the 5 publications which were discovered via MicroReact
  microTab = tibble(GenoData = c("https://static-content.springer.com/esm/art%3A10.1186%2F1741-7007-12-49/MediaObjects/12915_2014_781_MOESM1_ESM.xlsx",
                                 "https://microreact.org/api/files/raw?ea7cf80c6f69f8bbbfd962e738693b6af762994e",
                                 "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6557916/bin/mmc1.xlsx",
                                 "https://microreact.org/api/files/raw?296219f5e09067778f9a90b9ef8b65d33202c55f",
                                 "https://microreact.org/api/files/raw?83fcd308f5bfcdae28c7227ad54e12a5fe0ceadf"),
                    PhenoData = GenoData,
                    N = as.integer(245:249),
                    Context = "human",
                    Species =  c("Streptococcus pneumoniae", "Streptococcus pneumoniae", "Streptococcus pneumoniae", "Klebsiella pneumoniae", "Salmonella"),
                    Size = as.integer(c(189, 616, 13454, 270, 121)),
                    UpdatedSize = Size,
                    topTen = FALSE,
                    PMID = as.character(c(24957517, 30465642, 31003929, 31038449, 31107206)),
                    URL = paste0("www.ncbi.nlm.nih.gov/pubmed/?term=", PMID))
  finalData %<>%
    bind_rows(microTab)
  write_csv(finalData, "FinalDatasets.csv")
  setwd(initDir)
  finalData
}
