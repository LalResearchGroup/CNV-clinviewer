# CNV-clinviewer

The CNV-clinviewer (www.cnv-clinviewer.broadinstitute.org) is a user-friendly web-application for the interactive visualization, genomic exploration and standardized clinical significance interpretation of large copy-number variants (CNVs).

Contact: Dennis Lal (lald@ccf.org)
All code copyright (c) 2022 Dennis Lal and is distributed under terms of the MIT license.

## Synopsis

Copy-number variants (CNVs) and their emerging role in complex and rare diseases are an active field of research. Clinical CNV pathogenicity classification and genotype-phenotype analyses are challenging and time-consuming tasks that require the integration and analysis of information from many sources. Here, we introduce the CNV-ClinViewer, an open-source web-application for the clinical evaluation and visual exploration of CNVs. 

We combined data of >250,000 CNVs from patients and the general population with publicly available genomic, bioinformatic and clinical annotations, using R studio’s Shiny framework. We integrated an existing CNV classification tool in CNV-ClinViewer and provide access to enrichment analyses in >180 gene-set libraries. To ensure effectiveness and usability without bioinformatical expertise, we put great emphasis on an interactive workflow with real-time results, visualizations and a user-friendly interface design. 

After uploading CNV data, the CNV-ClinViewer enables:
```
a) semi-automated CNV clinical significance classification based on the 2019 ACMG/ClinGen Technical Standards for CNVs, 
b) comparative and interactive visual inspection of uploaded CNVs along with other pathogenic and general population CNV datasets, 
c) evaluation and prioritization of the genomic content by various gene dosage sensitivity scores, clinical annotations, and gene set enrichment analyses, and 
d) generation of comprehensive individual CNV reports including clinical significance classification and observed annotations details. 
```

The tool aids in identifying possible driver genes in a given CNV, which in turn is important for genetic counselling and possibly disease prognosis. Overall, this resource will facilitate biomedical CNV interpretation and re-evaluations of large sets of CNVs identified in rare disease cases and, in combination with clinical judgment, enable clinicians and researchers to formulate novel hypotheses and guide their decision-making process. The web-application is publicly available at https://cnv-ClinViewer.broadinstitute.org.

## CNV-clinviewer GitHub contents

CNV-clinviewer data pre-processing and R source code of https://cnv-ClinViewer.broadinstitute.org.

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
ClassifyCNV-master: Folder with ClassifyCNV tool integrated in Shiny app
Dockerfile: Dockerfile used for deployment
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


