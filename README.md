# CNV-clinviewer

The CNV-clinviewer (www.cnv-clinviewer.broadinstitute.org) is a user-friendly web-application for the interactive visualization, genomic exploration and standardized clinical significance interpretation of large copy number variants (CNVs).

## Synopsis

#### Purpose

To develop a user-friendly web-application for the visualization, genomic exploration and standardized clinical significance interpretation of large copy number variants (CNVs).

#### Methods

Aggregated data of >200,000 population CNVs from the UK Biobank and gnomAD as well as >10,000 patient CNVs from ClinVar. In addition, we identified ten genomic annotations such as gene dosage-sensitivity scores and two bioinformatic tools (ClassifyCNV, enrichr). All data and tools were integrated into a novel R Shiny based interface and were deployed in the Google Cloud platform as web-application.

#### Results

We present the CNV-clinviewer, an interactive visualization and interpretation tool that enables intuitive real-time exploration of CNV data online. After upload of CNV data CNV-clinviewer enables:
```
a) semi-automated CNV classification based on the 2019 ACMG/ClinGen Technical Standards for CNVs,
b) generation of comprehensive reports including CNV classification details, and overlap with established/predicted haploinsufficient/ triploinsufficient and clinically relevant genes and genomic regions,
c) visualization and dynamic filtering of uploaded CNVs and identification of overlap in cases and controls, 
d) evaluation and prioritization of the genomic content by various gene dosage sensitivity scores and clinical annotations, 
e) gene set enrichment analyses to infer knowledge about genes from a selected genomic region.
```

#### Conclusion

We developed the user-friendly CNV-clinviewer, a web application that facilitates consistent and transparent evaluation of CNVs. 

## CNV-clinviewer GitHub contents

CNV-clinviewer web server data pre-processing and R source code of http://simple-clinvar.broadinstitute.org.

### Data processing

```
Input: all data files in "/data"
Processing: processing of data with "data_generation.R" file
Output: R Object with all data tables "master_data.RDS"
```

### App & deployment

Description of files/folders that are part of the app:
```
server.R: Server of Shiny app
ui.R: User interface of Shiny app
master_data.RDS: R object with all data.
CNV-clinviewer_report.Rmd: Markdown file for generation of report in Shiny app
ClassifyCNV-master: Folder with ClassifyCNV python tool integrated in Shiny app
Dockerfile: Dockerfile used for deployment in GoogleCloud
```

## Run app locally

If you wish to run the ShinyApp locally: 
```
1. open this repository in R
2. runApp('~/CNV-clinviewer') 
```

Requirements:
```
- R version >= 4.1.1
- bedtools 2.3
- Python3
- R packages (see server.R for list)
```


