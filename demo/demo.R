library(HistoneMod)

ms1_path <- system.file("extdata", "MS1_demo.csv", package = "HistoneMod")
sample_path <- system.file("extdata", "samples_demo.csv", package = "HistoneMod")

ms1_demo <- utils::read.csv(ms1_path, check.names = TRUE)
sample_demo <- utils::read.csv(sample_path, check.names = TRUE)

cat("HistoneMod packaged example files\n")
cat("These bundled demo data are anonymized and numerically randomized,\n")
cat("with only four representative histone modification states retained.\n\n")
cat("MS1 demo   :", ms1_path, "\n")
cat("Sample demo:", sample_path, "\n\n")

cat("MS1 preview:\n")
print(utils::head(ms1_demo[, c("Protein.Name", "Peptide.Note", "Replicate.Name", "Total.Area.MS1")]))

cat("\nSample preview:\n")
print(sample_demo)

copy_dir <- file.path(tempdir(), "HistoneMod-demo")
dir.create(copy_dir, showWarnings = FALSE, recursive = TRUE)
file.copy(c(ms1_path, sample_path), copy_dir, overwrite = TRUE)

cat("\nCopied demo files to:\n")
cat(" ", copy_dir, "\n")

cat("\nLaunch the app with:\n")
cat("  HistoneMod::runHistoneMod()\n")
cat("Then either click 'Load Demo Data' or upload the copied CSV files.\n")
