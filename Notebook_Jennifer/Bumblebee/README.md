# Bumblebee

**Data**: samples per experimental group

| group \ nest| A | B | C | D | E | F | total |
|:--|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|Control| 18 | - | 8 | - | - |- |=26| 
|Exposed| - | 8 | - | 9 | 7 | 10 |=34|

**Analysis**: RNASeq pipelines

| File | Description |
|:--|:--|
| [01_Quality-Control](01_Quality-Control.md) | check read quality fastqc & multiqc |
| [02_Genome](02_Genome.md) | download reference files (dna, cdna, gff) |
| [03_GSNAP](03_GSNAP.md) | gsnap alignment pipeline (reads + dna + gff) -> (genecount files) |
| [04_DESeq2](04_DESeq2.md) | identify DEGs (genecount files) -> (siggenes.tsv)...refactoring|
| [05_WGCNA](05_WGCNA.md) | network analysis (genecounts)-> DESeq2 normalization -> WGCNA -> modules ...refactoring|
| [06_Kallisto](06_Kallisto.md) | kallisto pipeline (reads + cdna) -> (genecounts as quant folders) |
| [07_Sleuth](07_Sleuth.md) | identify DEGs (kallisto_quant folders) -> (siggenes.tsv) |
| [08_Salmon](08_Salmon.md) | salmon pipeline (reads + cdna) -> (genecounts as quant folders) |
| [09_Nextflow](09_Nextflow.md) | wrap steps 01, 03, 06, and 08 in a nextflow script |
| [10_Biomart](10_Biomart.md) | Map IDs to biologically relevant gene names ... in progress|


