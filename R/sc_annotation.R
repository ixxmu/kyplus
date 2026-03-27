#' Annotate Single-Cell Clusters Using ChatGPT API
#'
#' @description
#' Calls the OpenAI ChatGPT API (or compatible API) to annotate single-cell
#' clusters based on their top marker genes. Returns a structured data.frame
#' with major cell lineage, specific subtype annotation with marker genes,
#' and reasoning.
#'
#' This function uses \code{\link{ai_chat}} as the underlying API layer.
#'
#' @param marker_list A named list where names are cluster identifiers
#'   (e.g., "cluster0", "cluster1") and values are character vectors of
#'   top marker genes for each cluster (recommended: top 25).
#' @param species Character. Species, e.g., "human" or "mouse".
#'   Default is "human".
#' @param tissue Character. Optional tissue context, e.g., "PBMC",
#'   "tumor microenvironment", "brain", "liver". Helps improve annotation
#'   accuracy. Default is NULL.
#' @param model Character. Model to use. Default is "gpt-4o".
#' @param api_key Character. API key. Default reads from environment
#'   variable \code{OPENAI_API_KEY}.
#' @param base_url Character. API base URL. Default reads from environment
#'   variable \code{OPENAI_BASE_URL} (set by \code{ai_setup}), falling back
#'   to \code{"https://api.openai.com/v1"}.
#' @param temperature Numeric. Sampling temperature (0-2). Lower = more
#'   deterministic. Default is 0.1.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{cluster}{Cluster number (integer: 0, 1, 2, ...)}
#'     \item{major_lineage}{Major cell lineage (e.g., CD8_T, CD4_T, NK,
#'       Treg, B_cell, Monocyte)}
#'     \item{subtype_anno}{Specific subtype with 1-2 marker genes
#'       (e.g., Activated_CD8_Tem_GZMK)}
#'     \item{reason}{Brief explanation for the annotation (Chinese)}
#'   }
#'
#' @details
#' The function constructs a carefully crafted prompt that instructs the
#' AI model to:
#' \itemize{
#'   \item Identify broad cell lineages (major_lineage)
#'   \item Provide specific subtype annotations with key marker genes
#'     (subtype_anno)
#'   \item Explain the annotation reasoning briefly (reason)
#' }
#'
#' API key can be set via:
#' \code{ai_setup(api_key = "your-key-here")}
#'
#' Or: \code{Sys.setenv(OPENAI_API_KEY = "your-key-here")}
#'
#' @examples
#' \dontrun{
#' # Prepare marker list
#' markers <- list(
#'     cluster0 = c("CD3D", "CD8A", "CD8B", "GZMK", "NKG7"),
#'     cluster1 = c("CD3D", "CD4", "IL7R", "CCR7", "LEF1"),
#'     cluster2 = c("FOXP3", "IL2RA", "CTLA4", "TIGIT", "IKZF2")
#' )
#'
#' # Annotate
#' anno <- sc_annotate_clusters(markers, species = "human", tissue = "PBMC")
#' print(anno)
#'
#' # Use custom API endpoint
#' anno <- sc_annotate_clusters(
#'     markers,
#'     base_url = "https://your-proxy.com/v1",
#'     model = "gpt-4o"
#' )
#' }
#'
#' @seealso \code{\link{ai_chat}}, \code{\link{ai_setup}}
#'
#' @export
sc_annotate_clusters <- function(marker_list,
                                 species = "human",
                                 tissue = NULL,
                                 model = "gpt-4o",
                                 api_key = Sys.getenv("OPENAI_API_KEY"),
                                 base_url = .ai_get_base_url(),
                                 temperature = 0.1,
                                 verbose = TRUE) {
    # --- Validate inputs ---
    if (!is.list(marker_list) || is.null(names(marker_list))) {
        stop("marker_list must be a named list, ",
            "e.g.: list(cluster0 = c('CD3D', 'CD8A', ...), cluster1 = c(...))",
            call. = FALSE
        )
    }

    # --- Build marker text for prompt ---
    marker_text <- vapply(names(marker_list), function(nm) {
        genes <- marker_list[[nm]]
        if (length(genes) > 25) genes <- genes[1:25]
        paste0(nm, ": ", paste(genes, collapse = ", "))
    }, character(1))
    marker_text <- paste(marker_text, collapse = "\n")

    n_clusters <- length(marker_list)

    # --- Build tissue context ---
    tissue_context <- ""
    if (!is.null(tissue) && nchar(tissue) > 0) {
        tissue_context <- paste0(
            "The tissue/sample type is: ", tissue, ". ",
            "Please consider tissue-specific cell types. "
        )
    }

    # --- System prompt ---
    system_prompt <- paste0(
        "You are an expert single-cell RNA-seq bioinformatician with deep knowledge ",
        "of cell type markers across tissues and species. ",
        "Your task is to annotate cell clusters based on their top marker genes. ",
        "You must return ONLY a valid JSON object with a single key 'annotations' ",
        "containing an array of annotation objects. No markdown, no extra text."
    )

    # --- User prompt ---
    user_prompt <- paste0(
        "Below are ", n_clusters, " single-cell RNA-seq clusters with their top ",
        "marker genes from **", species, "** samples. ", tissue_context, "\n\n",
        marker_text, "\n\n",
        "Please annotate each cluster. Return a JSON object like:\n",
        "{\"annotations\": [{...}, ...]}\n\n",
        "Each annotation object must have exactly these 4 fields:\n",
        "1. \"cluster\" (integer): cluster number extracted from the name (e.g., ",
        "cluster0 -> 0, cluster1 -> 1)\n",
        "2. \"major_lineage\" (string): major cell lineage using concise names with ",
        "underscores, NO spaces. Examples: CD8_T, CD4_T, NK, Treg, B_cell, ",
        "Plasma, Monocyte, Macrophage, DC, pDC, Mast, Neutrophil, Fibroblast, ",
        "Epithelial, Endothelial, Myeloid_like, Cycling_T/NK, Innate_like_T, ",
        "T_cell, CD8/NK_T, etc.\n",
        "3. \"subtype_anno\" (string): specific subtype annotation that includes ",
        "functional state and 1-2 KEY marker gene names, all joined by underscores, ",
        "NO spaces. Examples:\n",
        "   - Activated_CD8_Tem_GZMK\n",
        "   - Naive_Tcm_CD4_IL7R_CCR7\n",
        "   - Activated_Treg_FOXP3_IL2RA_CTLA4\n",
        "   - Memory_Trm_like_T_ITGA1_SLAMF6\n",
        "   - Exhausted_CD8_Tex_CD39_TIM3_LAYN\n",
        "   - Tissue_NK_XCL1_XCL2_AREG\n",
        "   - Cycling_Lymphocytes_TK1_TYMS_BIRC5\n",
        "   - Myeloid_like_VEGFA_LST1_IRAK3\n",
        "   - CD16_Cytotoxic_NK_FCGR3A_FGFBP2\n",
        "4. \"reason\" (string): a brief ONE-sentence explanation in Chinese ",
        "for why this annotation was given, highlighting the key ",
        "marker gene evidence.\n\n",
        "Important rules:\n",
        "- Use standard cell biology nomenclature\n",
        "- major_lineage should be broad (e.g., for all T cell subtypes like CD8 ",
        "effector/memory, use CD8_T; for generic T cells use T_cell)\n",
        "- subtype_anno must be specific and always include 1-2 distinctive marker ",
        "gene names from the provided list\n",
        "- All naming uses underscores, no spaces\n",
        "- Return valid JSON only"
    )

    if (verbose) {
        message("-> Annotating ", n_clusters, " clusters...")
    }

    # --- Call AI via shared layer ---
    result <- ai_chat(
        system_prompt = system_prompt,
        user_prompt = user_prompt,
        model = model,
        api_key = api_key,
        base_url = base_url,
        temperature = temperature,
        parse_json = TRUE,
        verbose = verbose
    )

    # --- Extract annotations from parsed JSON ---
    parsed <- result$content
    annotations <- tryCatch(
        {
            if (is.data.frame(parsed)) {
                parsed
            } else if (is.list(parsed)) {
                # Object wrapper like {"annotations": [...]}
                df_found <- NULL
                for (nm in names(parsed)) {
                    if (is.data.frame(parsed[[nm]])) {
                        df_found <- parsed[[nm]]
                        break
                    }
                }
                if (is.null(df_found)) {
                    if (length(parsed) > 0 && is.list(parsed[[1]])) {
                        do.call(rbind, lapply(parsed, as.data.frame,
                            stringsAsFactors = FALSE
                        ))
                    } else {
                        stop("Cannot parse JSON structure returned by AI")
                    }
                } else {
                    df_found
                }
            } else {
                stop("Unknown return format")
            }
        },
        error = function(e) {
            stop("Failed to parse annotation result: ", e$message, call. = FALSE)
        }
    )

    # --- Validate and clean output ---
    expected_cols <- c("cluster", "major_lineage", "subtype_anno", "reason")
    missing_cols <- setdiff(expected_cols, names(annotations))
    if (length(missing_cols) > 0) {
        warning("Result missing columns: ", paste(missing_cols, collapse = ", "))
    }

    # Sort by cluster
    annotations$cluster <- as.integer(annotations$cluster)
    annotations <- annotations[order(annotations$cluster), ]
    rownames(annotations) <- NULL

    # Reorder columns
    present_cols <- intersect(expected_cols, names(annotations))
    extra_cols <- setdiff(names(annotations), expected_cols)
    annotations <- annotations[, c(present_cols, extra_cols), drop = FALSE]

    if (verbose) {
        message("Done! ", nrow(annotations), " clusters annotated.")
    }

    return(annotations)
}
