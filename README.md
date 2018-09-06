## Cytoscape WorkFlows

Collection of workflows as R Notebooks that use functionaility in R to process data and cyRest to communicate directly with Cytoscape.

# Enrichment Map Pipeline
Set of R notebooks to transform expression data to a ranked list and run them through Pathay enrichment pipeline. Pathway enrichment analysis helps gain mechanistic insight into large gene lists typically resulting from genome scale (–omics) experiments. It identifies biological pathways that are enriched in the gene list more than expected by chance. We explain pathway enrichment analysis and present a practical step-by-step guide to help interpret gene lists. The protocol is designed for biologists with no prior bioinformatics training and uses freely available software including g:Profiler, GSEA, Cytoscape and Enrichment Map.

## Main Pipeline Steps
[Pipeline Index](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/index.html)
 1. [Download TCGA data](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/Download_TCGA_data.html) - R notebook shows you how to download legacy microarray and rnaseq ovarian cancer data. The notebook can be modified to download any data from GDC
 1. [Supplemental Protocol 1](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/supplemental_protocol1_rnaseq.html) - convert raw RNASeq expression data to a ranked list
 1. [Supplemental Protocol 2](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/supplemental_protocol2_microarray.html) - convert RMA normalized microarray expression data to a ranked list
 1. [Supplemental Protocol 3](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/supplemental_protocol3_R_gse_methods.html) - Pathway Enrichment Analysis in R using ROAST and Camera
 1. [Supplemental Protocol 4](Supplemental_protocol_4_manual_phenotype_rand_with_edgeR.html) - perform phenotype randomizations using edgeR with GSEA.
 1. [Main Protocol - Create Enrichment Map](https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/Protocol2_createEM.html) - run GSEA on ranked list and automatically create an Enrichment Map from the results.
 
# Cell Cell Interactions Workflow

Using a set of proteins designated as receptors, and ligands defined with a set of GO terms calculate the set of interactions that represent cell-cell interactions (for example Ligand-receptor, receptor-receptor, ...). This analysis is not limited to Cell-Cell interactions. You can define your own protein types, either manually or by choosing different go terms, and create your customized protein-protein interaction network.

[editor on GitHub](https://github.com/BaderLab/Cytoscape_workflows/edit/master/README.md)
