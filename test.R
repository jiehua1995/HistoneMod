source("functions.R")
depends_check()

# -----------------------
# 1. Read the MS1 and sample files
# -----------------------
#ms1_file <- "F:/Quarto/MS1_modifications.csv"
ms1_file <- "H:/Projects/202509_Ref11075_Histone Modifications/MS1_modifications.csv"
#sample_file <- "F:/Quarto/samples.csv" # 修改为你的文件路径
sample_file <- "Z:/AIG/Shared/Histones/Quarto/samples.csv"

ms1 <- read.csv(ms1_file)
sample <- read.csv(sample_file)

temp <- subset(df, Protein.Name == "H3_54-63")

# -----------------------
# 2. Test percentage_calculation function
# -----------------------
filtered <- percentage_calculation(
  ms1, sample,
  exclude_un = FALSE,
  selected_peptides = unique(ms1$Peptide.Note),
  selected_samples = unique(sample$Replicate.Name)
)

cat("Filtered data:\n")
print(head(filtered))

# -----------------------
# 3. Test PCA plot
# -----------------------
cat("Testing PCA plot...\n")
pca_plot <- plot_pca(filtered)
print(pca_plot)

# -----------------------
# 4. Test Heatmap
# -----------------------
cat("Testing Heatmap...\n")
heatmap_plot <- tryCatch({
  plot_heatmap(filtered)
}, error=function(e){
  cat("Heatmap error:", e$message, "\n")
})

# -----------------------
# 5. Test Barplot
# -----------------------
proteins <- unique(filtered$Protein.Name)
if(length(proteins) > 0) {
  protein_name <- proteins[1]
  cat("Testing Barplot for protein:", protein_name, "\n")
  barplots <- plot_barplot(filtered, protein_name, add_signif=TRUE)
  
  # Display the first peptide
  if(length(barplots) > 0) {
    print(barplots[[1]])
  }
}
