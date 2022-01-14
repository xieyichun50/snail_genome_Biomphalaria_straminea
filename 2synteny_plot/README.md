# Generate macrosynteny plot.
Procedure:
1. run `format.fai.sh` to get genome chromosome and scaffold info.
2. run `format.gtf.sh` to prepare the gene location.
3. run `orthologues.format.sh` to prepare orthologs.
4. run `plotecho.sh` to generate all pairs of plotting scripts.
5. run `plotall.sh` (output of `plotecho.sh`) to get all figures.

Files:
1. `genome.fa`: whole genome sequence.
2. `longest-gene.gtf`: gtf generated in `1sequence_prep`
3. orthogues result in Folder `OrthoFinder/$date/Orthologues/`, `.tsv` format.

Software:
1. samtools
2. R/Rscript
3. R packages: 'optparse', 'tidyr', 'ggplot2', 'plotly'
