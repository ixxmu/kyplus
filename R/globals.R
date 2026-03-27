# 声明全局变量，避免R CMD check警告
utils::globalVariables(c(
  "name3", "details", "genes", "name", "name1", "name2",
  "entrezID", "s3ID_raw", "L3", "symbol", "pathway",
  "gs_name", "gene_symbol", "entrez_gene",
  ".data", "cluster", "prop"
))
