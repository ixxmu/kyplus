# DCE (Differential Causal Effects) Analysis Features

## Overview

This document summarizes the comprehensive DCE analysis functionality implemented in the `kyplus` package. The implementation follows the DCE methodology from [Pirkl et al. (2022)](https://doi.org/10.1093/bioinformatics/btab847) and provides integration with the Bioconductor `dce` package.

## New Features Implemented

### 1. Core Network Construction (`R/netdce.R`)

**Function:** `build_regulatory_network()`

**Improvements:**
- Enhanced error handling and input validation
- Support for data frames in addition to matrices
- Automatic removal of constant genes (zero variance)
- Improved edge weight scaling with numerical stability
- Better handling of edge cases (all equal weights, no edges, etc.)

**Supported Methods:**
- `correlation`: Pearson correlation (fast, robust)
- `pcor`: Partial correlation with ridge regression for collinearity
- `mi`: Mutual information (non-linear relationships)
- `aracne`: ARACNE algorithm (recommended, removes indirect edges)
- `random`: Random networks (for testing)

**Key Parameters:**
- `threshold`: Edge inclusion threshold (0-1)
- `max_parents`: Maximum parent nodes per gene
- `handle_colinearity`: Use ridge regression for pcor
- `ridge_lambda`: Ridge penalty parameter

### 2. DCE Helper Functions (`R/dce_helpers.R`)

#### 2.1 `split_expression_by_gene()`
Splits expression data into groups based on target gene expression levels.

**Features:**
- Customizable quantile cutoffs
- Flexible group labeling
- Optional group assignment output
- Automatic summary statistics

**Use Case:**
Compare networks between samples with high vs low expression of a key regulator gene.

#### 2.2 `filter_dce_results()`
Filters and sorts DCE analysis results.

**Filter Options:**
- P-value threshold
- Minimum absolute DCE value
- Source/target gene filtering
- Multiple sorting options (DCE, p-value, gene names)

#### 2.3 `run_dce_analysis()`
Complete DCE analysis pipeline wrapper.

**Features:**
- Automated network construction
- DCE computation
- Result filtering
- Comprehensive error handling
- Progress reporting

**Options:**
- Use same network for both conditions or build separate networks
- Automatic filtering of results
- Verbose mode for debugging

#### 2.4 `batch_dce_analysis()`
Batch processing for multiple target genes.

**Features:**
- Parallel processing support (optional)
- Individual error handling per gene
- Metadata tracking (target gene, comparison groups, sample sizes)
- Summary reporting

**Use Case:**
Analyze pathway dysregulation across multiple key genes simultaneously.

### 3. Visualization Functions (`R/dce_visualization.R`)

#### 3.1 `plot_dce_network()`
Enhanced network visualization with DCE overlay.

**Features:**
- Highlight specific nodes (e.g., target genes)
- Color edges by DCE magnitude and direction
- Filter by significance and effect size
- Customizable node/edge appearance
- Shadow text for readability

#### 3.2 `plot_dce_heatmap()`
Heatmap of DCE values across all edges.

**Features:**
- Optional hierarchical clustering
- Custom color scales (RdBu, viridis, magma)
- Display DCE values in cells (optional)
- Automatic filtering of zero-DCE edges

#### 3.3 `plot_dce_volcano()`
Volcano plot showing significance vs effect size.

**Features:**
- Significance thresholds (horizontal/vertical lines)
- Highlight significant edges
- Label top N edges
- Interactive labeling (with ggrepel if available)

#### 3.4 `compare_networks()`
Side-by-side network comparison.

**Features:**
- Shows both networks and their difference
- Consistent node positioning
- Highlight common genes
- Combined plot layout (with patchwork)

### 4. Enrichment Analysis Integration (`R/dce_enrichment.R`)

#### 4.1 `enrich_dce_results()`
Pathway enrichment for genes with significant DCE.

**Supported Databases:**
- KEGG pathways
- Gene Ontology (BP, MF, CC)
- Reactome pathways

**Supported Organisms:**
- Human (Homo sapiens)
- Mouse (Mus musculus)
- Rat (Rattus norvegicus)

**Features:**
- Automatic gene ID conversion
- Background gene set option
- Flexible gene selection (source, target, or both)
- Comprehensive parameter control

#### 4.2 `plot_enrichment()`
Visualization of enrichment results.

**Plot Types:**
- Bar plot: Simple pathway ranking
- Dot plot: Shows gene ratio and significance
- Network plot: Gene-pathway relationships
- Enrichment map: Pathway similarity network

#### 4.3 `compare_enrichment()`
Compare enrichment across multiple DCE analyses.

**Use Case:**
Compare pathway dysregulation across different target genes or conditions.

### 5. Comprehensive Testing (`tests/testthat/test-dce.R`)

**Test Coverage:**
- Network construction with all methods
- Edge case handling (small samples, constant genes)
- Parameter validation
- Data splitting functionality
- DCE result filtering
- Integration tests (full pipeline)
- Batch analysis
- Error handling

**Test Types:**
- Unit tests for individual functions
- Integration tests for complete workflows
- Edge case and error condition tests
- Optional tests for heavy dependencies (skip_if_not_installed)

### 6. Complete Documentation (`vignettes/dce-analysis.Rmd`)

**Content:**
- Introduction to DCE methodology
- Key concepts and when to use DCE
- Complete workflow examples (3 workflows)
- Method comparison guide
- Advanced filtering techniques
- Real-world example (interferon response)
- Troubleshooting guide
- Best practices
- Citation information

**Workflows Covered:**
1. Complete DCE pipeline (split → build → analyze → visualize)
2. Batch analysis for multiple target genes
3. Direct network comparison between conditions

## Dependencies

### Required (Imports)
- Core: `stats`, `methods`, `utils`
- Data manipulation: `dplyr`, `tidyr`, `stringr`, `magrittr`
- Visualization: `ggplot2`, `reshape2`
- Network: `graph`, `naturalsort`
- Statistics: `ridge`, `infotheo`, `minet`

### Suggested (for full functionality)
- DCE analysis: `dce`, `mnem`
- Enrichment: `clusterProfiler`, `enrichplot`, `ReactomePA`, `DOSE`, `msigdbr`
- Organism databases: `org.Hs.eg.db`, `org.Mm.eg.db`, `org.Rn.eg.db`
- Enhanced visualization: `ggrepel`, `patchwork`
- Parallel processing: `future.apply`

## File Structure

```
kyplus/
├── R/
│   ├── netdce.R                 # Core network construction
│   ├── dce_helpers.R            # Helper functions (split, filter, run, batch)
│   ├── dce_visualization.R      # Visualization functions
│   └── dce_enrichment.R         # Pathway enrichment integration
├── tests/testthat/
│   └── test-dce.R               # Comprehensive test suite
├── vignettes/
│   └── dce-analysis.Rmd         # Complete tutorial
└── man/                         # Auto-generated documentation (12 new functions)
```

## Usage Example

```r
library(kyplus)
library(dce)

# 1. Load expression data
expr_data <- your_expression_matrix  # samples × genes

# 2. Split by target gene
split_data <- split_expression_by_gene(expr_data, "ISG15")

# 3. Run DCE analysis
results <- run_dce_analysis(
  wt_data = split_data$Low,
  mt_data = split_data$High,
  network_method = "aracne",
  filter_results = TRUE
)

# 4. Visualize
plot_dce_network(results$dce_results, highlight_nodes = "ISG15")
plot_dce_volcano(results$dce_results)

# 5. Pathway enrichment
enrichment <- enrich_dce_results(
  results$dce_results,
  pathway_db = "KEGG",
  organism = "human"
)

# 6. Plot enrichment
plot_enrichment(enrichment, plot_type = "dot")
```

## Function Summary

| Function | Purpose | Input | Output |
|----------|---------|-------|--------|
| `build_regulatory_network()` | Build gene network | Expression matrix | Network (graphNEL) |
| `split_expression_by_gene()` | Group samples | Expression + target gene | List of matrices |
| `filter_dce_results()` | Filter DCE results | DCE results + thresholds | Filtered data frame |
| `run_dce_analysis()` | Complete pipeline | Two expression matrices | DCE results + network |
| `batch_dce_analysis()` | Multiple targets | Expression + gene list | List of DCE results |
| `plot_dce_network()` | Network viz | DCE results | ggplot object |
| `plot_dce_heatmap()` | Heatmap viz | DCE results | ggplot object |
| `plot_dce_volcano()` | Volcano plot | DCE results | ggplot object |
| `compare_networks()` | Network comparison | Two networks | Combined plot |
| `enrich_dce_results()` | Pathway enrichment | DCE results | Enrichment results |
| `plot_enrichment()` | Enrichment viz | Enrichment results | ggplot object |
| `compare_enrichment()` | Multi-enrichment | List of DCE results | Comparison results |

## Key Improvements Over Original DCE Package

1. **Integrated Workflow**: Seamless pipeline from expression data to enrichment
2. **Enhanced Visualization**: Multiple plot types with better customization
3. **Batch Processing**: Efficient analysis of multiple target genes
4. **Better Error Handling**: Comprehensive validation and informative errors
5. **Enrichment Integration**: Direct pathway analysis of DCE results
6. **Comprehensive Testing**: Extensive test coverage
7. **Complete Documentation**: Tutorial-style vignette with multiple examples

## Performance Considerations

- **Network Construction**: ARACNE is slowest but most accurate; correlation is fastest
- **Sample Size**: Minimum 20 samples per condition recommended
- **Gene Count**: Optimal range 50-500 genes; use `max_genes` for large datasets
- **Batch Analysis**: Use `parallel = TRUE` for multiple genes (requires future.apply)
- **Memory**: Large networks (>1000 genes) may require substantial RAM

## Troubleshooting

### Common Issues

1. **No significant edges**
   - Solution: Lower threshold, try different methods, check data quality

2. **Network construction fails**
   - Solution: Check for constant genes, increase sample size, use correlation method

3. **Enrichment returns no results**
   - Solution: Lower p-value threshold, try different pathway databases

4. **Gene ID conversion fails**
   - Solution: Verify gene symbols are correct, update organism database

## Citation

If you use this implementation in your research, please cite:

1. **DCE Method**: Pirkl, M., et al. (2022). Identifying cancer pathway dysregulations using differential causal effects. *Bioinformatics*, 38(6), 1550-1559.

2. **DCE Package**: https://bioconductor.org/packages/dce

3. **kyplus Package**: [Your citation information]

## Future Enhancements (Potential)

- [ ] Support for additional network inference methods (BN-learn, GRNBoost2)
- [ ] Time-series DCE analysis
- [ ] Integration with scRNA-seq data
- [ ] Interactive visualizations (plotly, Shiny)
- [ ] GPU acceleration for large networks
- [ ] Export to Cytoscape format
- [ ] Integration with more pathway databases (WikiPathways, etc.)

## Contact & Support

For questions, bug reports, or feature requests:
- GitHub Issues: https://github.com/ixxmu/kyplus/issues
- Package Documentation: https://ixxmu.github.io/kyplus

## Version History

- **v0.1.2+**: Comprehensive DCE functionality added
  - Network construction improvements
  - Helper functions for workflow
  - Visualization suite
  - Enrichment integration
  - Complete documentation and tests
