prepareAntibiotics = function() {
  ABs = antibiotics[, c("name", "ab", "atc_group2")]
  colnames(ABs) = c("name", "abbreviation", "group")
  write_csv(ABs, paste0(WORKING_DIR, "FullAntibioticsTable.csv"))
}

