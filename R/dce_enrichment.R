#' Pathway Enrichment Analysis for DCE Results
#'
#' @description
#' Performs pathway enrichment analysis on genes showing significant differential
#' causal effects. Integrates with clusterProfiler for comprehensive pathway analysis.
#'
#' @param dce_results DCE results object from dce::dce() or run_dce_analysis().
#' @param pvalue_threshold Numeric threshold for filtering significant DCE edges.
#'   Default is 0.05.
#' @param min_abs_dce Minimum absolute DCE value to consider. Default is 0.3.
#' @param organism Character string specifying the organism. Options: "human" (default),
#'   "mouse", "rat".
#' @param gene_id_type Character string specifying gene ID type. Default is "SYMBOL".
#' @param pathway_db Character string specifying pathway database. Options:
#'   \itemize{
#'     \item "KEGG": KEGG pathways
#'     \item "GO_BP": Gene Ontology Biological Process
#'     \item "GO_MF": Gene Ontology Molecular Function
#'     \item "GO_CC": Gene Ontology Cellular Component
#'     \item "Reactome": Reactome pathways
#'     \item "all": All databases (slow)
#'   }
#'   Default is "KEGG".
#' @param gene_set Character string specifying which genes to use for enrichment:
#'   \itemize{
#'     \item "source": Source genes of significant edges
#'     \item "target": Target genes of significant edges
#'     \item "both": Both source and target genes
#'   }
#'   Default is "both".
#' @param qvalue_cutoff Q-value cutoff for enrichment significance. Default is 0.05.
#' @param min_gene_set_size Minimum number of genes in pathway. Default is 10.
#' @param max_gene_set_size Maximum number of genes in pathway. Default is 500.
#' @param universe Character vector of background genes. If NULL, uses all genes
#'   in the network. Default is NULL.
#'
#' @return A list containing:
#' \itemize{
#'   \item enrichment_results: Enrichment analysis results
#'   \item significant_genes: Vector of genes used for enrichment
#'   \item total_edges: Number of significant DCE edges
#'   \item parameters: Analysis parameters
#' }
#'
#' @examples
#' \dontrun{
#' # After running DCE analysis
#' dce_results <- run_dce_analysis(wt_data, mt_data)
#'
#' # Perform KEGG enrichment
#' enrich_kegg <- enrich_dce_results(
#'   dce_results$dce_results,
#'   pathway_db = "KEGG",
#'   organism = "human"
#' )
#'
#' # View top enriched pathways
#' head(enrich_kegg$enrichment_results)
#'
#' # GO enrichment
#' enrich_go <- enrich_dce_results(
#'   dce_results$dce_results,
#'   pathway_db = "GO_BP",
#'   pvalue_threshold = 0.01
#' )
#' }
#'
#' @export
enrich_dce_results <- function(dce_results,
                               pvalue_threshold = 0.05,
                               min_abs_dce = 0.3,
                               organism = "human",
                               gene_id_type = "SYMBOL",
                               pathway_db = "KEGG",
                               gene_set = "both",
                               qvalue_cutoff = 0.05,
                               min_gene_set_size = 10,
                               max_gene_set_size = 500,
                               universe = NULL) {

  # Check if required packages are available
  if(!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("Package 'clusterProfiler' is required. Install with:\n",
         "  BiocManager::install('clusterProfiler')")
  }

  # Filter DCE results
  sig_results <- filter_dce_results(
    dce_results,
    pvalue_threshold = pvalue_threshold,
    min_abs_dce = min_abs_dce,
    remove_na = TRUE
  )

  if(nrow(sig_results) == 0) {
    stop("No significant DCE results found with current thresholds")
  }

  # Extract genes based on gene_set parameter
  if(gene_set == "source") {
    gene_list <- unique(sig_results$source)
  } else if(gene_set == "target") {
    gene_list <- unique(sig_results$target)
  } else if(gene_set == "both") {
    gene_list <- unique(c(sig_results$source, sig_results$target))
  } else {
    stop("gene_set must be 'source', 'target', or 'both'")
  }

  message("Using ", length(gene_list), " unique genes from ",
          nrow(sig_results), " significant DCE edges")

  # Get universe (background) genes
  if(is.null(universe)) {
    all_results <- as.data.frame(dce_results)
    universe <- unique(c(all_results$source, all_results$target))
    message("Using ", length(universe), " genes as background (all genes in network)")
  }

  # Get organism database
  org_db <- get_organism_db(organism)

  # Perform enrichment based on pathway database
  enrich_result <- perform_enrichment(
    genes = gene_list,
    universe = universe,
    pathway_db = pathway_db,
    org_db = org_db,
    gene_id_type = gene_id_type,
    qvalue_cutoff = qvalue_cutoff,
    min_gene_set_size = min_gene_set_size,
    max_gene_set_size = max_gene_set_size
  )

  # Return results
  result <- list(
    enrichment_results = enrich_result,
    significant_genes = gene_list,
    total_edges = nrow(sig_results),
    parameters = list(
      pvalue_threshold = pvalue_threshold,
      min_abs_dce = min_abs_dce,
      organism = organism,
      pathway_db = pathway_db,
      gene_set = gene_set,
      qvalue_cutoff = qvalue_cutoff
    )
  )

  return(result)
}


#' Get Organism Database Code
#'
#' @description
#' Helper function to get the correct organism database code for clusterProfiler.
#'
#' @param organism Character string: "human", "mouse", or "rat".
#'
#' @return Character string of organism database code.
#'
#' @keywords internal
get_organism_db <- function(organism) {
  organism <- tolower(organism)

  org_map <- list(
    "human" = "org.Hs.eg.db",
    "mouse" = "org.Mm.eg.db",
    "rat" = "org.Rn.eg.db"
  )

  if(!organism %in% names(org_map)) {
    stop("Organism must be 'human', 'mouse', or 'rat'")
  }

  org_db <- org_map[[organism]]

  # Check if package is installed
  if(!requireNamespace(org_db, quietly = TRUE)) {
    stop("Package '", org_db, "' is required. Install with:\n",
         "  BiocManager::install('", org_db, "')")
  }

  return(org_db)
}


#' Perform Pathway Enrichment
#'
#' @description
#' Internal function to perform the actual enrichment analysis.
#'
#' @param genes Character vector of gene symbols.
#' @param universe Character vector of background genes.
#' @param pathway_db Pathway database to use.
#' @param org_db Organism database code.
#' @param gene_id_type Gene ID type.
#' @param qvalue_cutoff Q-value cutoff.
#' @param min_gene_set_size Minimum gene set size.
#' @param max_gene_set_size Maximum gene set size.
#'
#' @return Enrichment result object.
#'
#' @keywords internal
perform_enrichment <- function(genes,
                              universe,
                              pathway_db,
                              org_db,
                              gene_id_type,
                              qvalue_cutoff,
                              min_gene_set_size,
                              max_gene_set_size) {

  # Convert gene symbols to Entrez IDs if needed
  if(gene_id_type == "SYMBOL" && pathway_db == "KEGG") {
    message("Converting gene symbols to Entrez IDs for KEGG analysis...")

    gene_mapping <- tryCatch({
      clusterProfiler::bitr(
        genes,
        fromType = "SYMBOL",
        toType = "ENTREZID",
        OrgDb = org_db
      )
    }, error = function(e) {
      stop("Error converting gene IDs: ", e$message)
    })

    genes_entrez <- gene_mapping$ENTREZID

    if(!is.null(universe)) {
      universe_mapping <- tryCatch({
        clusterProfiler::bitr(
          universe,
          fromType = "SYMBOL",
          toType = "ENTREZID",
          OrgDb = org_db
        )
      }, error = function(e) {
        warning("Error converting universe IDs. Using all genes as background.")
        return(NULL)
      })

      if(!is.null(universe_mapping)) {
        universe_entrez <- universe_mapping$ENTREZID
      } else {
        universe_entrez <- NULL
      }
    } else {
      universe_entrez <- NULL
    }
  } else {
    genes_entrez <- genes
    universe_entrez <- universe
  }

  # Perform enrichment based on database
  if(pathway_db == "KEGG") {
    # Get organism code for KEGG
    kegg_org <- get_kegg_organism_code(org_db)

    enrich_result <- clusterProfiler::enrichKEGG(
      gene = genes_entrez,
      organism = kegg_org,
      keyType = "ncbi-geneid",
      pvalueCutoff = 1,  # Get all results, filter later
      qvalueCutoff = qvalue_cutoff,
      universe = universe_entrez,
      minGSSize = min_gene_set_size,
      maxGSSize = max_gene_set_size
    )

  } else if(grepl("^GO_", pathway_db)) {
    # Gene Ontology enrichment
    ont <- sub("GO_", "", pathway_db)

    enrich_result <- clusterProfiler::enrichGO(
      gene = genes,
      OrgDb = org_db,
      keyType = gene_id_type,
      ont = ont,
      pvalueCutoff = 1,
      qvalueCutoff = qvalue_cutoff,
      universe = universe,
      minGSSize = min_gene_set_size,
      maxGSSize = max_gene_set_size,
      readable = TRUE
    )

  } else if(pathway_db == "Reactome") {
    if(!requireNamespace("ReactomePA", quietly = TRUE)) {
      stop("Package 'ReactomePA' is required for Reactome enrichment. Install with:\n",
           "  BiocManager::install('ReactomePA')")
    }

    enrich_result <- ReactomePA::enrichPathway(
      gene = genes_entrez,
      organism = get_reactome_organism(org_db),
      pvalueCutoff = 1,
      qvalueCutoff = qvalue_cutoff,
      universe = universe_entrez,
      minGSSize = min_gene_set_size,
      maxGSSize = max_gene_set_size,
      readable = TRUE
    )

  } else {
    stop("Unsupported pathway database: ", pathway_db)
  }

  return(enrich_result)
}


#' Get KEGG Organism Code
#'
#' @keywords internal
get_kegg_organism_code <- function(org_db) {
  kegg_codes <- list(
    "org.Hs.eg.db" = "hsa",
    "org.Mm.eg.db" = "mmu",
    "org.Rn.eg.db" = "rno"
  )

  if(!org_db %in% names(kegg_codes)) {
    stop("Unsupported organism for KEGG")
  }

  return(kegg_codes[[org_db]])
}


#' Get Reactome Organism Name
#'
#' @keywords internal
get_reactome_organism <- function(org_db) {
  reactome_orgs <- list(
    "org.Hs.eg.db" = "human",
    "org.Mm.eg.db" = "mouse",
    "org.Rn.eg.db" = "rat"
  )

  if(!org_db %in% names(reactome_orgs)) {
    stop("Unsupported organism for Reactome")
  }

  return(reactome_orgs[[org_db]])
}


#' Plot Enrichment Results
#'
#' @description
#' Creates visualization of pathway enrichment results from DCE analysis.
#'
#' @param enrichment_result Enrichment result from enrich_dce_results().
#' @param plot_type Character string specifying plot type:
#'   \itemize{
#'     \item "bar": Bar plot (default)
#'     \item "dot": Dot plot
#'     \item "network": Network plot
#'     \item "enrichmap": Enrichment map
#'   }
#' @param top_n Integer. Number of top pathways to show. Default is 20.
#' @param color_by Character string specifying what to color by. Default is "qvalue".
#' @param font_size Numeric value for font size. Default is 12.
#' @param ... Additional arguments passed to enrichplot functions.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' enrich_results <- enrich_dce_results(dce_results$dce_results)
#' plot_enrichment(enrich_results, plot_type = "dot", top_n = 15)
#' }
#'
#' @export
plot_enrichment <- function(enrichment_result,
                           plot_type = "bar",
                           top_n = 20,
                           color_by = "qvalue",
                           font_size = 12,
                           ...) {

  if(!requireNamespace("enrichplot", quietly = TRUE)) {
    stop("Package 'enrichplot' is required. Install with:\n",
         "  BiocManager::install('enrichplot')")
  }

  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Extract enrichment object
  enrich_obj <- enrichment_result$enrichment_results

  if(is.null(enrich_obj) || nrow(as.data.frame(enrich_obj)) == 0) {
    stop("No enrichment results to plot")
  }

  # Create plot based on type
  if(plot_type == "bar") {
    p <- enrichplot::barplot(enrich_obj, showCategory = top_n, ...)
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = font_size))

  } else if(plot_type == "dot") {
    p <- enrichplot::dotplot(enrich_obj, showCategory = top_n, color = color_by, ...)
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = font_size))

  } else if(plot_type == "network") {
    if(nrow(as.data.frame(enrich_obj)) < 2) {
      stop("Network plot requires at least 2 enriched pathways")
    }
    p <- enrichplot::cnetplot(enrich_obj, showCategory = top_n, ...)

  } else if(plot_type == "enrichmap") {
    if(nrow(as.data.frame(enrich_obj)) < 2) {
      stop("Enrichment map requires at least 2 enriched pathways")
    }
    # Calculate pairwise similarities
    enrich_obj <- enrichplot::pairwise_termsim(enrich_obj)
    p <- enrichplot::emapplot(enrich_obj, showCategory = top_n, ...)

  } else {
    stop("plot_type must be 'bar', 'dot', 'network', or 'enrichmap'")
  }

  return(p)
}


#' Compare Enrichment Between Multiple DCE Results
#'
#' @description
#' Compares pathway enrichment across multiple DCE analysis results
#' (e.g., from different target genes or conditions).
#'
#' @param dce_results_list Named list of DCE results objects.
#' @param pvalue_threshold P-value threshold for DCE filtering. Default is 0.05.
#' @param pathway_db Pathway database to use. Default is "KEGG".
#' @param organism Organism. Default is "human".
#' @param qvalue_cutoff Q-value cutoff for enrichment. Default is 0.05.
#' @param plot Logical. If TRUE, creates comparison plot. Default is TRUE.
#' @param ... Additional arguments passed to enrichment analysis.
#'
#' @return A list containing enrichment results for each input and optionally a plot.
#'
#' @examples
#' \dontrun{
#' # Compare enrichment for multiple target genes
#' batch_results <- batch_dce_analysis(expr_data, target_genes = c("Gene1", "Gene2"))
#' dce_list <- lapply(batch_results, function(x) x$dce_results)
#'
#' compare_results <- compare_enrichment(
#'   dce_list,
#'   pathway_db = "KEGG",
#'   plot = TRUE
#' )
#' }
#'
#' @export
compare_enrichment <- function(dce_results_list,
                              pvalue_threshold = 0.05,
                              pathway_db = "KEGG",
                              organism = "human",
                              qvalue_cutoff = 0.05,
                              plot = TRUE,
                              ...) {

  if(!is.list(dce_results_list) || length(dce_results_list) < 2) {
    stop("dce_results_list must be a list with at least 2 DCE results")
  }

  if(is.null(names(dce_results_list))) {
    names(dce_results_list) <- paste0("Result_", seq_along(dce_results_list))
  }

  message("Performing enrichment analysis for ", length(dce_results_list), " results...")

  # Perform enrichment for each result
  enrich_list <- lapply(names(dce_results_list), function(name) {
    message("  Processing: ", name)

    tryCatch({
      enrich_dce_results(
        dce_results_list[[name]],
        pvalue_threshold = pvalue_threshold,
        pathway_db = pathway_db,
        organism = organism,
        qvalue_cutoff = qvalue_cutoff,
        ...
      )
    }, error = function(e) {
      warning("Error in ", name, ": ", e$message)
      return(NULL)
    })
  })

  names(enrich_list) <- names(dce_results_list)

  # Remove NULL results
  enrich_list <- enrich_list[!sapply(enrich_list, is.null)]

  if(length(enrich_list) == 0) {
    stop("All enrichment analyses failed")
  }

  # Create comparison if plot requested
  if(plot && requireNamespace("enrichplot", quietly = TRUE)) {
    # Extract enrichment objects
    enrich_objs <- lapply(enrich_list, function(x) x$enrichment_results)

    # Try to create comparison plot
    tryCatch({
      comparison_plot <- enrichplot::compareCluster_internal(enrich_objs)
      return(list(
        enrichment_results = enrich_list,
        comparison_plot = comparison_plot
      ))
    }, error = function(e) {
      warning("Could not create comparison plot: ", e$message)
      return(list(enrichment_results = enrich_list))
    })
  }

  return(list(enrichment_results = enrich_list))
}
