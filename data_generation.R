### SCRIPT TO GENERATE DATA FILES FOR CNV-CLINVIEWER ###

#all processed data tables are saved as R objects in /processed. 
#The last lines of code in this script combines all those objects to "master_data.RDS" -> the data input for the ShinyApp

# sections:
#   -CHROMOSOME BANDS
#   -GENE TABLE
#   -DISEASE REGIONS / SYNDROMES
#   -CNV population data (gnomAD/UKB)
#   -ClinVar data
#   -example CNVs
#   -About/ description tables
#   -Merge of all data to master data


library(dplyr)
library(readr)
library(tidyr)
library(valr)

setwd("data")

##intersect function
intersect_table = function(table, ranges){
  names(table)[1:3] = c("chrom","start","end")
  table$start = as.integer(table$start)
  table$end = as.integer(table$end)
  y = data.frame(chrom = ranges$chr, start= ranges$x[1], end= ranges$x[2])
  table = bed_intersect(table, y, suffix = c("", ""))
  return(table)
}

##### CHROMOSOME BANDS #####
cytoBand_hg19 <- read_delim("cytoBand_hg19.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(cytoBand_hg19) = c("chrom", "chromStart", "chromEnd", "Chromosome location", "Giemsa stain")

saveRDS(cytoBand_hg19, file = "processed_data/cytoBand_hg19.RDS") 
cytoBand_hg19 <- readRDS("processed_data/cytoBand_hg19.RDS")

##### GENE TABLE #####
#load genetables
hugo_genetable <- read_delim("hugo_genetable.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
refGene <- read_delim("refGene.txt", delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
MANE_GRCh38_v0_95_summary <- read_delim("MANE.GRCh38.v0.95.summary.txt",delim = "\t", escape_double = FALSE, trim_ws = TRUE)

#process
hugo_genetable <- hugo_genetable[!is.na(hugo_genetable$`NCBI gene ID`),]
gene_table <- hugo_genetable[!hugo_genetable$Chromosome == "not on reference assembly",]


#remove duplicated genes due to several entries (eg. several OMOM IDs)
non_unique_genes = unique(gene_table$`Approved symbol`[duplicated(gene_table$`Approved symbol`)])
gene_table$`OMIM ID` = as.character(gene_table$`OMIM ID`)
gene_table$`Orphanet ID` = as.character(gene_table$`Orphanet ID`)
for(i in non_unique_genes){
  tmp_data = gene_table[gene_table$`Approved symbol` == i,]
  gene_table[which(gene_table$`Approved symbol` == i)[1], "UniProt accession"] = paste(unique(tmp_data$'UniProt accession'), collapse=",")
  gene_table[which(gene_table$`Approved symbol` == i)[1], "OMIM ID"] = paste(unique(tmp_data$'OMIM ID'), collapse=",")
  gene_table[which(gene_table$`Approved symbol` == i)[1], "Orphanet ID"] = paste(unique(tmp_data$'Orphanet ID'), collapse=",")
}
gene_table = gene_table[!duplicated(gene_table$`Approved symbol`),]

#annotation of longest transcript from refseq or MANE transcript
gene_table$chromStart <- NA
gene_table$chromEnd <- NA
gene_table$transcript <- NA

MANE_GRCh38_v0_95_summary <- MANE_GRCh38_v0_95_summary[MANE_GRCh38_v0_95_summary$MANE_status == "MANE Select", ]
MANE_GRCh38_v0_95_summary$RefSeq_nuc <- gsub("\\..*","",MANE_GRCh38_v0_95_summary$RefSeq_nuc)
MANE_GRCh38_v0_95_summary <- MANE_GRCh38_v0_95_summary[,c("HGNC_ID", "RefSeq_nuc")]

gene_table <- merge(gene_table, MANE_GRCh38_v0_95_summary, by.x="HGNC ID", by.y="HGNC_ID", all.x=T)


for(i in 1:nrow(gene_table)){
  if(!is.na(gene_table$RefSeq_nuc[i]) & gene_table$RefSeq_nuc[i] %in% refGene$X2) {
    #cat("MANE")
    index = which(gene_table$RefSeq_nuc[i] == refGene$X2)
    gene_table[i, "chromStart"] = refGene[index, "X5"]
    gene_table[i, "chromEnd"] = refGene[index, "X6"]
    gene_table[i, "transcript"] = refGene[index, "X2"] 
  } else {
    #cat("not MANE")
    gene_name = as.character(gene_table[i, "Approved symbol"])
    ref_data = as.data.frame(refGene[refGene$X13 == gene_name,])
    if(nrow(ref_data)>0){
      ref_data$length = ref_data$X6 - ref_data$X5
      ref_data = ref_data[ref_data$length == max(ref_data$length),]
      gene_table[i, "chromStart"] = ref_data$X5[1]
      gene_table[i, "chromEnd"] = ref_data$X6[1]
      gene_table[i, "transcript"] = ref_data$X2[1]
    } else{
      print(paste(gene_name, "gene not in refseq data"))
    }
  }
}

#remove genes without coordinates
gene_table = gene_table[!is.na(gene_table$chromStart),]

#reorder gene table
gene_table = gene_table[,c("Chromosome",  "chromStart", "chromEnd", "transcript", "Approved symbol", "HGNC ID", "Chromosome location", "NCBI gene ID",
                           "Ensembl gene ID", "UCSC gene ID", "UniProt accession", "OMIM ID", "Orphanet ID" )]
names(gene_table) = c("chrom",  "chromStart", "chromEnd", "transcript", "gene_name", "HGNC ID", "Chromosome location", "NCBI gene ID",
                      "Ensembl gene ID", "UCSC gene ID", "UniProt accession", "OMIM ID", "Orphanet ID" )
gene_table$chrom = paste0("chr", gene_table$chrom)

#add OMIM link
gene_table$OMIM_links = NA
for(row in 1:nrow(gene_table)){
  if(!is.na(gene_table$`OMIM ID`[row])){
    ids = as.character(str_split(gene_table$`OMIM ID`[row],",", simplify=T))
    for(i in 1:length(ids)){
      ids[i] = paste0('<a href="', URLdecode(paste0("https://omim.org/entry/",ids[i])), '" target="_blank">',  as.character(ids[i]) ,'</a>')
    }
    gene_table$OMIM_links[row] = paste0(ids, collapse=", ")
  }
}

##add gene dosage sensitivity scores
dosage_score <- read_delim("20210820_gene_list_pHI_pTS_Collins2021.csv", delim = ",", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
gnomad_constraint <- read_delim("gnomad.v2.1.1.lof_metrics.by_gene.txt.bgz", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
exac_pli <- read_delim("fordist_cleaned_exac_r03_march16_z_pli_rec_null_data.txt",delim = "\t", escape_double = FALSE,col_names =TRUE, trim_ws = TRUE)

#pTS, pHI
gene_table <- merge(gene_table, dosage_score, by.x = "gene_name", by.y = "gene", all.x = TRUE)

#loeuf and pLI score
canonical_gnomad_loeuf_pli <- gnomad_constraint[,c('gene', 'oe_lof_upper', "exac_pLI")]

gene_table <- merge(gene_table, canonical_gnomad_loeuf_pli, by.x = "gene_name", by.y = "gene", all.x = TRUE)
gene_table <- gene_table %>%
  dplyr::rename(loeuf = oe_lof_upper) %>%
  dplyr::rename(pLI = exac_pLI)
gene_table <- gene_table[!duplicated(gene_table$gene_name),]

#%HI DECIPHER
HI_Predictions_Version3 <- read_table2("HI_Predictions_Version3.bed", col_names = FALSE)
HI_Predictions_Version3 = HI_Predictions_Version3[-1,4]
HI_Predictions_Version3 <- HI_Predictions_Version3 %>%
  separate("X4", c("gene_name","value","%HI"), '\\|') 
HI_Predictions_Version3 = HI_Predictions_Version3[,-2]
HI_Predictions_Version3$`%HI` = as.numeric(str_replace_all(HI_Predictions_Version3$`%HI`, "%", ""))

gene_table = merge(gene_table, HI_Predictions_Version3, by= "gene_name", all.x=T)

#ClinGen dosage sensitive genes
ClinGen_triplosensitivity_gene_GRCh37 <- read_table2("ClinGen_triplosensitivity_gene_GRCh37.bed")
ClinGen_triplosensitivity_gene_GRCh37 = ClinGen_triplosensitivity_gene_GRCh37[,c(4,5)]
names(ClinGen_triplosensitivity_gene_GRCh37) = c("gene_name", "TS Score ClinGen")
ClinGen_triplosensitivity_gene_GRCh37$`TS Score ClinGen` = as.character(ClinGen_triplosensitivity_gene_GRCh37$`TS Score ClinGen`)
ClinGen_triplosensitivity_gene_GRCh37$`TS Score ClinGen` = recode(ClinGen_triplosensitivity_gene_GRCh37$`TS Score ClinGen`, "0" = "0 (No Evidence)", "1" = "1 (Little Evidence)", "2" = "2 (Emerging Evidence)", "3" = "3 (Sufficient Evidence)", "40" = "40 (Dosage Sensitivity Unlikely)")

ClinGen_haploinsufficiency_gene_GRCh37 <- read_table2("ClinGen_haploinsufficiency_gene_GRCh37.bed")
ClinGen_haploinsufficiency_gene_GRCh37 = ClinGen_haploinsufficiency_gene_GRCh37[,c(4,5)]
names(ClinGen_haploinsufficiency_gene_GRCh37) = c("gene_name", "HI Score ClinGen")
ClinGen_haploinsufficiency_gene_GRCh37$`HI Score ClinGen` = as.character(ClinGen_haploinsufficiency_gene_GRCh37$`HI Score ClinGen`)
ClinGen_haploinsufficiency_gene_GRCh37$`HI Score ClinGen` = recode(ClinGen_haploinsufficiency_gene_GRCh37$`HI Score ClinGen`, "0" = "0 (No Evidence)", "1" = "1 (Little Evidence)", "2" = "2 (Emerging Evidence)", "3" = "3 (Sufficient Evidence)", "30" = "30 (Autosomal Recessive)", "40" = "40 (Dosage Sensitivity Unlikely)")

#merge
gene_table <- merge(gene_table, ClinGen_haploinsufficiency_gene_GRCh37, by = "gene_name", all.x = TRUE)
gene_table <- merge(gene_table, ClinGen_triplosensitivity_gene_GRCh37, by = "gene_name", all.x = TRUE)

gene_table[gene_table$chrom == "chrX and Y", "chrom"] = "chrX;chrY"
gene_table = gene_table %>%
  mutate(chrom = strsplit(as.character(chrom), ";")) %>%
  unnest(chrom) %>%
  filter(chrom != "")

#reorder in bed format
gene_table = gene_table[,c("chrom",  "chromStart", "chromEnd" ,"gene_name","transcript", "HGNC ID", "Chromosome location", "NCBI gene ID", "Ensembl gene ID", "UCSC gene ID", "UniProt accession", 
                           "OMIM ID","OMIM_links", "Orphanet ID","pHI", "pTS" ,"loeuf","pLI" , "%HI" , "HI Score ClinGen","TS Score ClinGen")]

#save gene table
saveRDS(gene_table, file = "processed_data/gene_table.RDS") 
gene_table <- readRDS("processed_data/gene_table.RDS")


##### DISEASE REGIONS / SYNDROMES  #####
#clingen dosage sensitive regions
ClinGen_region <- read_csv("ClinGen_region.txt")
ClinGen_region = ClinGen_region[-nrow(ClinGen_region),]
ClinGen_region = ClinGen_region %>% 
  separate(GRCh37, c("chrom","chromStart"), ":") %>% 
  separate(chromStart, c("chromStart","chromEnd"), "-")
ClinGen_region = ClinGen_region[,c("chrom", "chromStart", "chromEnd", "Gene/Region", "HI Score", "TS Score", "Last Eval.")]
names(ClinGen_region) = c("chrom", "chromStart", "chromEnd", "Region", "HI Score ClinGen", "TS Score ClinGen", "Last Eval.")
ClinGen_region$chrom = paste0("chr", ClinGen_region$chrom)

saveRDS(ClinGen_region, file = "processed_data/ClinGen_region.RDS") 
ClinGen_region <- readRDS("processed_data/ClinGen_region.RDS")

#Decipher CNV syndromes
decipher_syndromes <- read_delim("decipher_syndromes.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
saveRDS(decipher_syndromes, file = "processed_data/decipher_syndromes.RDS") 
decipher_syndromes <- readRDS("processed_data/decipher_syndromes.RDS")

#gene-disease
Clingen_Gene_Disease_Summary <- read_csv("Clingen-Gene-Disease-Summary-2022-01-26.csv", col_names = FALSE)
names(Clingen_Gene_Disease_Summary) = as.character(Clingen_Gene_Disease_Summary[5,])
Clingen_Gene_Disease_Summary = Clingen_Gene_Disease_Summary[-c(1:7),]

saveRDS(Clingen_Gene_Disease_Summary, file = "processed_data/Clingen_Gene_Disease_Summary.RDS") 
Clingen_Gene_Disease_Summary <- readRDS("processed_data/Clingen_Gene_Disease_Summary.RDS")


##### CNV population data #####
##UKB frequency in sliding window
cnv_ukb_reference_v2_20190626 <- read_delim("cnv.ukb.reference_v2.20190626.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

biobank_cnv_data = cnv_ukb_reference_v2_20190626 %>% 
  separate(ID, c("CHR","START"), ":") %>% 
  separate(START, c("START","END"), "-") %>%
  separate(END, c("END","CN"), "_")

biobank_cnv_data$CN = ifelse(biobank_cnv_data$CN == "+", "duplication", "deletion")

biobank_cnv_data= biobank_cnv_data[,2:8]
biobank_cnv_data$LENGTH = (as.numeric(biobank_cnv_data$END) - as.numeric(biobank_cnv_data$START)) + 1

#filter for CNVs >50kb
biobank_cnv_data = biobank_cnv_data[biobank_cnv_data$LENGTH > 50000,]
biobank_cnv_data$CHR = paste0("chr",  biobank_cnv_data$CHR)

ukb_freq_list = list()

for(i in 1:22){

  cnv_data <- biobank_cnv_data[biobank_cnv_data$CHR == paste0("chr",i), ]
  cnv_data_deletions = cnv_data[cnv_data$CN == "deletion",]
  cnv_data_duplications = cnv_data[cnv_data$CN == "duplication",]

  
  #sliding window
  window = 200000
  steps = 100000
  
  chr_max = max(cytoBand_hg19[cytoBand_hg19$chrom == paste0("chr",i),"chromEnd"])
  cnv_frequency_df = data.frame(chrom= paste0("chr",i), 
                               chromStart= seq(steps,chr_max,steps)-(window/2), 
                               chromEnd= seq(steps,chr_max,steps)+(window/2),
                               position=seq(steps,chr_max,steps), 
                               deletions_freq=0, 
                               duplications_freq=0)
  
  n=1

  for(r in seq(steps,chr_max,steps)) {
    
    min= r- window/2
    max= min((r+ window/2), chr_max)
    
    ranges = list(chr=paste0("chr",i), x=c(min, max))
    
    number_deletions = intersect_table(cnv_data_deletions, ranges)
    number_deletions = sum(number_deletions$AC_ALL)
    cnv_frequency_df$deletions_freq[n] = number_deletions/482734 #number of individuals
    
    number_duplications = intersect_table(cnv_data_duplications, ranges)
    number_duplications = sum(number_duplications$AC_ALL)
    cnv_frequency_df$duplications_freq[n] = number_duplications/482734 #number of individuals
    
    n= n+1
  }
  
  ukb_freq_list[[paste0("chr",i)]] = cnv_frequency_df

}

saveRDS(ukb_freq_list, file = "processed_data/ukb_freq_list.RDS") 
ukb_freq_list <- readRDS("processed_data/ukb_freq_list.RDS")


##gnomad SV frequency in sliding window
gnomad_v2_1_sv_sites <- read_delim("gnomad_v2.1_sv.sites.bed", delim = "\t", escape_double = FALSE,  trim_ws = TRUE)
gnomad_v2_1_sv_sites = gnomad_v2_1_sv_sites[gnomad_v2_1_sv_sites$svtype == "DEL"|gnomad_v2_1_sv_sites$svtype == "DUP", ]

gnomad_v2_1_sv_sites = gnomad_v2_1_sv_sites[,c("#chrom","start","end", "AC","AF", "svtype")]
gnomad_v2_1_sv_sites$AC = as.numeric(gnomad_v2_1_sv_sites$AC)
gnomad_v2_1_sv_sites$LENGTH =  gnomad_v2_1_sv_sites$end - gnomad_v2_1_sv_sites$start

#filter for CNVs >50kb
gnomad_v2_1_sv_sites = gnomad_v2_1_sv_sites[gnomad_v2_1_sv_sites$LENGTH > 50000,]

gnomad_freq_list = list()

for(i in unique(gnomad_v2_1_sv_sites$`#chrom`)){
  
  cnv_data <- gnomad_v2_1_sv_sites[gnomad_v2_1_sv_sites$`#chrom` == i, ]
  cnv_data_deletions = cnv_data[cnv_data$svtype == "DEL",]
  cnv_data_duplications = cnv_data[cnv_data$svtype == "DUP",]
  
  #sliding window
  window = 200000
  steps = 100000
  
  chr_max= max(cytoBand_hg19[cytoBand_hg19$chrom == paste0("chr",i),"chromEnd"])
  cnv_frequency_df= data.frame(chrom= paste0("chr",i), 
                               chromStart= seq(steps,chr_max,steps)-(window/2), 
                               chromEnd= seq(steps,chr_max,steps)+(window/2),
                               position=seq(steps,chr_max,steps), 
                               deletions_freq=NA, 
                               duplications_freq=NA)
  
  n=1
  
  for(r in seq(steps,chr_max,steps)) {
    
    min= r- window/2
    max= min((r+ window/2), chr_max)
    
    # column AC = allele count
    number_deletions = sum(cnv_data_deletions$AC[(
      (cnv_data_deletions$start >= min & cnv_data_deletions$start <= max) | #cnv that starts within the x coordinates
        (cnv_data_deletions$end <= max & cnv_data_deletions$end >= min) | #cnv that ends within the coordinates
        (cnv_data_deletions$start < min & cnv_data_deletions$end > max)| #cnv that has a start and end outside of the coordinates
        (cnv_data_deletions$start > min & cnv_data_deletions$end < max) #cnv inside of coordinates
    )]
    ) #close sum
    cnv_frequency_df$deletions_freq[n] = number_deletions/14891 #number of individuals
    
    number_duplications = sum(cnv_data_duplications$AC[(
      (cnv_data_duplications$start >= min & cnv_data_duplications$start <= max) | #cnv that starts within the x coordinates
        (cnv_data_duplications$end <= max & cnv_data_duplications$end >= min) | #cnv that ends within the coordinates
        (cnv_data_duplications$start < min & cnv_data_duplications$end > max)| #cnv that has a start and end outside of the coordinates
        (cnv_data_duplications$start > min & cnv_data_duplications$end < max) #cnv inside of coordinates
    )]
    ) #close sum
    cnv_frequency_df$duplications_freq[n] = number_duplications/14891 #number of individuals
    n= n+1
  }
  
  gnomad_freq_list[[paste0("chr",i)]] = cnv_frequency_df
  
}

saveRDS(gnomad_freq_list, file = "processed_data/gnomad_freq_list.RDS") 
gnomad_freq_list <- readRDS("processed_data/gnomad_freq_list.RDS")


##### ClinVar data ####

# file from clinvar is to large, please download here: https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz
# and add file to the data folder (working directory)

variant_summary <- read_delim("variant_summary_160121.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

cnvs_clinvar = variant_summary[variant_summary$Type == "copy number loss"|variant_summary$Type == "copy number gain", ]
cnvs_clinvar = cnvs_clinvar[cnvs_clinvar$Assembly == "GRCh37",]
cnvs_clinvar = cnvs_clinvar[, c("Chromosome", "Start", "Stop", "Type", "ClinicalSignificance", "PhenotypeList", "Origin", "VariationID","OtherIDs")]

names(cnvs_clinvar) <- c("chrom", "chromStart", "chromEnd", "Type", "Classification", "Phenotype", "Origin", "VariationID", "OtherIDs")
cnvs_clinvar$Type = ifelse(cnvs_clinvar$Type == "copy number gain", "duplication", "deletion")
cnvs_clinvar$chrom = paste0("chr",cnvs_clinvar$chrom)

#save data per chromosome
clinvar_cnv_list = list()
for(i in unique(cnvs_clinvar$chrom)) {
  cnv_data = cnvs_clinvar[cnvs_clinvar$chrom == i,]
  clinvar_cnv_list[[i]] = cnv_data
}

saveRDS(clinvar_cnv_list, file = "processed_data/clinvar_cnv_list.RDS") 
clinvar_cnv_list <- readRDS("processed_data/clinvar_cnv_list.RDS")

#save count in sliding window
clinvar_freq_list = list()

for(i in unique(cnvs_clinvar$chrom)){
  
  cnv_data = cnvs_clinvar[cnvs_clinvar$chrom == i,]
  
  cnv_data_deletions = cnv_data[cnv_data$Type == "deletion" & 
                                  (cnv_data$Classification == "Pathogenic" | cnv_data$Classification == "Likely pathogenic") ,]
  cnv_data_duplications = cnv_data[cnv_data$Type == "duplication" &  
                                     (cnv_data$Classification == "Pathogenic" | cnv_data$Classification == "Likely pathogenic") ,]
  
  #sliding window
  window = 200000
  steps = 100000
  
  chr_max= max(cytoBand_hg19[cytoBand_hg19$chrom == i,"chromEnd"])
  cnv_frequency_df= data.frame(chrom= i, 
                               chromStart= seq(steps,chr_max,steps)-(window/2), 
                               chromEnd= seq(steps,chr_max,steps)+(window/2),
                               position=seq(steps,chr_max,steps), 
                               number_deletions=NA, 
                               number_duplications=NA)
  n=1
  
  for(r in seq(steps,chr_max,steps)) {
    
    min= r- window/2
    max= min((r+ window/2), chr_max)
    
    ranges = list(chr=i, x=c(min, max))
    
    number_deletions = intersect_table(cnv_data_deletions, ranges)
    number_deletions = nrow(number_deletions)
    cnv_frequency_df$number_deletions[n] = number_deletions
    
    number_duplications = intersect_table(cnv_data_duplications, ranges)
    number_duplications = nrow(number_duplications)
    cnv_frequency_df$number_duplications[n] = number_duplications
    
    n= n+1
    
  }
  
  clinvar_freq_list[[i]] = cnv_frequency_df

}

saveRDS(clinvar_freq_list, file = "processed_data/clinvar_freq_list.RDS") 
clinvar_freq_list <- readRDS("processed_data/clinvar_freq_list.RDS")


##### example CNVs #####
example_cnvs <- read_table("cnv-clinviewer_example_table.bed")
example_cnvs$ID = as.character(example_cnvs$ID)
example_cnvs$START = as.integer(example_cnvs$START)
example_cnvs$END = as.integer(example_cnvs$END)
example_cnvs = example_cnvs[,c(1:3,5:6,4,7:8)]
saveRDS(example_cnvs, file = "processed_data/example_cnvs.RDS") 
example_cnvs <- readRDS("processed_data/example_cnvs.RDS")

cnvs_9q_example_input <- read_excel("cnvs_9q33.3q34.11microdeletions.xlsx")
saveRDS(cnvs_9q_example_input, file = "processed_data/cnvs_9q_example_input.RDS") 

cnv_classification_details_chr9q33 <- read_delim("cnv_classification_details_chr9q33.tsv",delim = "\t", escape_double = FALSE,trim_ws = TRUE)
saveRDS(cnv_classification_details_chr9q33, file = "processed_data/cnv_classification_details_chr9q33.RDS") 

##### About/ description tables #####
data_sources <- read_excel("data_about.xlsx", sheet = "data_sources")
saveRDS(data_sources, file = "processed_data/data_sources.RDS") 

gene_scores <- read_excel("data_about.xlsx", sheet = "gene_scores")
saveRDS(gene_scores, file = "processed_data/gene_scores.RDS") 

Copy_number_loss <- read_excel("data_about.xlsx", sheet = "Copy_number_loss")
saveRDS(Copy_number_loss, file = "processed_data/Copy_number_loss.RDS") 

Copy_number_gain <- read_excel("data_about.xlsx", sheet = "Copy_number_gain")
saveRDS(Copy_number_gain, file = "processed_data/Copy_number_gains.RDS") 


##### Merge of all data to master data #####
master_data = list()

for(i in list.files("processed_data")){
  name = sub('\\.RDS$', '', i) 
  master_data[[name]] = readRDS(paste0("processed_data/",i))
}

saveRDS(master_data, file = "../master_data.RDS") 

