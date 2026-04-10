# 声明全局变量，避免R CMD check警告
utils::globalVariables(c(
  "name3", "details", "genes", "name", "name1", "name2",
  "entrezID", "s3ID_raw", "L3", "symbol", "pathway",
  "gs_name", "gene_symbol", "entrez_gene",
  ".data", "cluster", "prop",
  "logFC", "SpatialFDR", "PValue",
  "mean_sfdr", "pct_sig", "logFC_cv", "mean_purity", "mean_logFC",
  "n_nhoods", "logFC_sd", "score_sfdr", "score_pct", "score_cv",
  "score_purity", "score_effect", "score_nhood",
  "total_score", "importance", "priority", "direction",
  "sample", "tissue", "N_tot", "N_cls", "freq", "rid", "cid", "sig"
))
