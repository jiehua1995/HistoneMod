##### Global variables #####
# Column names used inside dplyr/ggplot2 non-standard evaluation. Declaring
# them here keeps R CMD check free of "no visible binding" notes.
utils::globalVariables(c(
  "Group", "Isotope.Label.Type", "PC1", "PC2", "Peptide.Note", "Percentage",
  "Protein.Name", "Replicate.Name", "Total.Area.MS1", "Total.Area.MS1.Sum",
  "ci95", "err", "mean_value", "sd_value", "se", "n", "value", "metric",
  "direction", "p_adj", "effect", "sample", "feature", ".data",
  "predicted", "dose", "n_replicates", "power", "time",
  "effect_a", "effect_b", "occupancy", "site", "pattern"
))
