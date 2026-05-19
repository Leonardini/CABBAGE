### Extracts a simplified lookup table (antibiotic name, abbreviation, ATC drug class)
### from the AMR package's built-in antibiotics dataset and writes it to
### FullAntibioticsTable.csv for use in drug-name normalisation downstream.
prepareAntibiotics = function() {
  ABs = antibiotics[, c("name", "ab", "atc_group2")]
  colnames(ABs) = c("name", "abbreviation", "group")
  write_csv(ABs, paste0(WORKING_DIR, "FullAntibioticsTable.csv"))
}

