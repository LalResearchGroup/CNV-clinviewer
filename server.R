# CNV-clinviewer #

##### load packagaes #####
library(ggplot2)
library(scales)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(plyr)
library(dplyr)

library(shiny)
library(plotly)
library(shinydashboard)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(shinydashboardPlus)
library(RColorBrewer)
library(shinycssloaders)
library(shinyjs)
library(DT)

library(writexl)
library(gtools) 
library(tinytex)
library(rmarkdown)
library(enrichR)
library(bedtoolsr)

data <- readRDS("master_data.RDS")

setEnrichrSite("Enrichr")
databases <- listEnrichrDbs()

intersect_table = function(table, ranges){
  names(table)[1:3] = c("chrom","start","end")
  col_names = names(table)
  table$start = as.integer(table$start)
  table$end = as.integer(table$end)
  y = data.frame(chrom = as.character(ranges$chr), start= as.integer(ranges$x[1]), end= as.integer(ranges$x[2]))
  table= as.data.frame(table)
  table = bedtoolsr::bt.intersect(a= table, b=y,  wao=T) #here is the actual intersecting
  names(table)[ncol(table)] = "overlap"
  table = table[table$overlap > 0, ]
  names(table)[1:length(col_names)] = col_names
  table$percentage_inregion = round(((table$overlap+1) / (table$end - (table$start -1)))*100,2)
  table$percentage_inregion[table$percentage_inregion >= 100] = 100
  return(table)
}

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

jscode <- "
  shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
  }
  "
  
server <- function(input, output, session) {
  
  ##### HOME UI / UPLOAD HELP #####
  observeEvent(input$go_to_about, {
    updateTabsetPanel(session, "inTabset", selected = "panel4")
  })
  
  observeEvent(input$go_to_contact, {
    showModal(modalDialog(
      title= span( icon("comment-lines"),"Contact the CNV-clinviewer team"),
      
      HTML(paste(
        "<b>The CNV-clinviewer relies on your feedback. Please send an Email if you wish to make a request, a comment, or report a bug.</b>",
        "<br>",
        "<b>Dennis Lal</b>",
        "Genomic Medicine Institute, Lerner Research Institute, Cleveland Clinic, Cleveland, OH, USA",
        "<i class=\"fa fa-envelope\" role=\"presentation\" aria-label=\"envelope icon\"></i> lald@ccf.org","<br>",
        "<b>Marie Macnee</b>",
        "Cologne Center for Genomics (CCG), Medical Faculty of the University of Cologne, University Hospital of Cologne, Cologne, Germany",
        "<i class=\"fa fa-envelope\" role=\"presentation\" aria-label=\"envelope icon\"></i> mgramm2@uni-koeln.de","<br>",
        "<b>Eduardo Pérez-Palma</b>",
        "Universidad del Desarrollo, Centro de Genética y Genómica, Facultad de Medicina Clínica Alemana, Santiago, Chile",
        "<i class=\"fa fa-envelope\" role=\"presentation\" aria-label=\"envelope icon\"></i> eduardoperez@udd.cl",
        
        sep="<br>")),
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  observeEvent(input$terms_use, {
    showModal(modalDialog(
      HTML(paste(
        "<b>Terms of Use By clicking “Accept”, You agree to the following:</b>",
        "(1) The CNV-clinviewer is developed for Research Use Only (RUO), and it does not provide any medical or healthcare services or advices whatsoever.",
        "(2) The CNV-clinviewer is freely available for academic and non-profit purposes only.",
        "(3) When using results obtained from the CNV-clinviewer, you agree to cite the CNV-clinviewer.",
        "(4) Your IP address will be recorded by Google Analytics or other means for tracking purposes.",
        "(5) You confirm and warrant that you have the full right and authority to provide genome data to the CNV-clinviewer, to analyze such data, and to obtain results on such data. 
        You further confirm and warrant that the data does not contain any identifiable information, and that we may create derivative work for research and educational purposes. 
        You also understand that the CNV-clinviewer web server does not require user registration, so that your data is potentially accessible by third parties by decrypting URLs.",
        "(6) THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, F
        ITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
        WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
        sep="<br>")),
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  observeEvent(input$terms_use2, {
    showModal(modalDialog(
      HTML(paste(
        "<b>Terms of Use By clicking “Accept”, You agree to the following:</b>",
        "(1) The CNV-clinviewer is developed for Research Use Only (RUO), and it does not provide any medical or healthcare services or advices whatsoever.",
        "(2) The CNV-clinviewer is freely available for academic and non-profit purposes only.",
        "(3) When using results obtained from the CNV-clinviewer, you agree to cite the CNV-clinviewer.",
        "(4) Your IP address will be recorded by Google Analytics or other means for tracking purposes.",
        "(5) You confirm and warrant that you have the full right and authority to provide genome data to the CNV-clinviewer, to analyze such data, and to obtain results on such data. 
        You further confirm and warrant that the data does not contain any identifiable information, and that we may create derivative work for research and educational purposes. 
        You also understand that the CNV-clinviewer web server does not require user registration, so that your data is potentially accessible by third parties by decrypting URLs.",
        "(6) THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, F
        ITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
        WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
        sep="<br>")),
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  observeEvent(input$upload_help, {
    showModal( tags$div(id="modal2", modalDialog(
      HTML(paste(
        "<b>You can upload CNVs in a bed or tab-delimited text file, or in an excel file with the following columns:</b>", "<br>",
        "<br>",
        "<b>Required columns:</b>", "<br>",
        "1. <b>CHR</b>: chromosome, for example 'chr1'", "<br>",
        "2. <b>START</b>: genomic start coordiante of CNV (hg19)", "<br>",
        "3. <b>END</b>: genomic end coordiante of CNV (hg19)","<br>",
        "4. <b>TYPE</b>: type of CNV ('DEL' or 'DUP')","<br>",
        "<br>",
        "<b>Optional columns:</b>", "<br>",
        "5. <b>ID</b>: sample ID/ identifier (CNVs with the same sample ID will be visualized in the same row; if you do not provide sample IDs each CNV will be given a unique ID)", "<br>",
        "6. <b>POINTS</b>: The CNVs are automatically classified based on the",
        paste0("<a href='", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7313390/", "' target='_blank'>", "2019 ACMG/ClinGen Technical Standards for CNVs.","</a>"),
        "Evaluated evidence categories for copy number losses are: 1A/B, 2A-H, 3A-C, 4O, and for copy number gains: 1A/B, 2A-H, 2J-L, 3A-C, 4O.
                   If you have further information e.g. about the family history or the 'de novo' status you can give the total score of the non-evaluated evidence categories in the 'POINTS' column as numeric values (eg. -1 or 0.9).", "<br>",
        "7. <b>FILTER_'name'</b> : additional binary variables (with values 1 (='yes') and 0 (='no')) for filtering. The name of the variable should be provided after 'FILTER_'.", "<br>","<br>"
      )),
      hr(),
      div(tableOutput("example_cnvs"), style = "font-size:70%", align= "center"),
      hr(),
      downloadLink('download_example', 'Download example (.bed)', style = 'font-size:14px'),
      br(),
      downloadLink('download_example_xlsx', 'Download example (.xlsx)', style = 'font-size:14px'),
      footer = tagList(
        modalButton("OK"),
      ),
    size="xl"
    )))
  })
  
  observeEvent(input$terms_use, {
    showModal(modalDialog(
      HTML(paste(
        "<b>Terms of Use By clicking “Accept”, You agree to the following:</b>",
        "(1) The CNV-clinviewer is developed for Research Use Only (RUO), and it does not provide any medical or healthcare services or advices whatsoever.",
        "(2) The CNV-clinviewer is freely available for academic and non-profit purposes only.",
        "(3) When using results obtained from the CNV-clinviewer, you agree to cite the CNV-clinviewer.",
        "(4) Your IP address will be recorded by Google Analytics or other means for tracking purposes.",
        "(5) You confirm and warrant that you have the full right and authority to provide genome data to the CNV-clinviewer, to analyze such data, and to obtain results on such data. 
        You further confirm and warrant that the data does not contain any identifiable information, and that we may create derivative work for research and educational purposes. 
        You also understand that the CNV-clinviewer web server does not require user registration, so that your data is potentially accessible by third parties by decrypting URLs.",
        "(6) THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, F
        ITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
        WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
        sep="<br>")),
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  output$example_cnvs = renderTable({
    example_cnvs = as.data.frame(data$example_cnvs)
    example_cnvs[,7] = as.integer(example_cnvs[,7])
    example_cnvs[,8] = as.integer(example_cnvs[,8])
    head(example_cnvs)
  })
  
  output$download_example <- downloadHandler(
    filename = function() {
      #paste('data-', Sys.Date(), '.bed', sep='')
      paste0('clincnv_example_data', '.bed')
    },
    content = function(con) {
      write.table(data$example_cnvs, con, quote = FALSE, sep = "\t", row.names = FALSE)
    }
  )
  
  output$download_example_xlsx <- downloadHandler(
    filename = function() {
      #paste('data-', Sys.Date(), '.bed', sep='')
      paste0('clincnv_example_data', '.xlsx')
    },
    content = function(con) {
      writexl::write_xlsx(data$example_cnvs, con)
    }
  )
  
  ##### DATA UPLOAD AND NAVIGATION TO ANALYSIS PAGE #####
  uploaded_cnvs = reactiveValues(table = NULL)
  
  observeEvent(input$submit_data_upload, {
    
    req(input$agree_terms2)
    
    inFile <- input$file1
    
    if (stringr::str_ends(input$file_upload$datapath, "(xlsx|xls)")) {
      df <-  readxl::read_excel(input$file_upload$datapath) 
    } else  {
      tryCatch({
        df <- read_table(input$file_upload$datapath)
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    }

    validate(
      need(ncol(df) > 4, "Please check the format of your file.")
    )
    
    if(!"ID" %in% names(df)){
      df$ID = seq(1, nrow(df))
    }
    
    if(!"POINTS" %in% names(df)){
      df$POINTS = NA
    }
    
    df = data.frame("CHR" = df$CHR, 
                    "START"= as.integer(df$START), 
                    "END"= as.integer(df$END), 
                    "TYPE"= df$TYPE, 
                    "ID"= as.character(df$ID), 
                    "POINTS"= df$POINTS, 
                    df[,grep("FILTER_", names(df))])
    
    uploaded_cnvs$table <- df
  })
  
  observeEvent(input$submit_data_paste, { 
    req(input$agree_terms)
    
    df = data.frame(do.call("rbind", strsplit(unlist(strsplit(input$file_paste, "\n")[1]), " ", fixed = TRUE)))
    names(df)[1:min(5,ncol(df))] = c("CHR", "START", "END", "TYPE", "ID")[1:min(5,ncol(df))]
    df$START = as.integer(df$START)
    df$END = as.integer(df$END)
    
    if(!"ID" %in% names(df)){
      df$ID = seq(1, nrow(df))
    }
    
    if(!"POINTS" %in% names(df)){
      df$POINTS = NA
    }
    
    df = data.frame("CHR" = df$CHR, 
                    "START"= as.integer(df$START), 
                    "END"= as.integer(df$END), 
                    "TYPE"= df$TYPE, 
                    "ID"= as.character(df$ID), 
                    "POINTS"= df$POINTS, 
                    df[,grep("FILTER_", names(df))])
    
    uploaded_cnvs$table <- df
  })
  
  output$uploaded_data = renderTable({
    head(uploaded_cnvs$table)
  })
  
  observeEvent(input$submit_example, { 
    df <- read_excel("cnvs_9q33.3q34.11microdeletions.xlsx")
    #df <- read_table2("clinncnv_viewer_example.bed")
    #df <- read_table2("cnvs_allanbayat_2.tsv")
    #df <- read_table2("cnvs_ausnz.tsv")
    uploaded_cnvs$table <- as.data.frame(df)
  })
  
  observeEvent(input$submit_data_upload | input$submit_data_paste | input$submit_example, {
    req(input$submit_data_upload > 0 | input$submit_data_paste > 0 | input$submit_example > 0)
    updateTabsetPanel(session, "inTabset", selected = "panel2")
    showModal(modalDialog(
      HTML(paste(
        "<b>Please wait.</b>", "<br>",
        "Your uploaded CNVs are being classified. You will automatically be directed to the results."
      ))
      ,
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  ##### CLASSIFICATION AND TABLE #####
  
  output$evidence_categories_table_loss = renderTable({
    evidence_categories <- data$Copy_number_loss 
    return(as.data.frame(evidence_categories))
  })
  
  output$evidence_categories_table_gain = renderTable({
    evidence_categories <- data$Copy_number_gains 
    return(as.data.frame(evidence_categories))
  })
  
  observeEvent(input$interpretation_help, {
    showModal( tags$div(id="modal1", modalDialog(
      
      HTML(paste0(
        "<b>How are the CNVs classified?</b>", 
        "<br>",
        "The uploaded CNVs are classified based on the ",
        paste0("<a href='", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7313390/", "' target='_blank'>", "2019 ACMG/ClinGen Technical Standards for CNVs ","</a>"),
        "using the",
        paste0("<a href='", "https://www.nature.com/articles/s41598-020-76425-3", "' target='_blank'>", " ClassifyCNV ","</a>"),"tool. ",
        "A description of the scoring algorithm can be found", 
        paste0("<a href='", "https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-020-76425-3/MediaObjects/41598_2020_76425_MOESM1_ESM.pdf", "' target='_blank'>", " here","</a>"),". ",
        "The final classification results from the score from ClassifyCNV and the additional score from manually evaluated evidence categories by the user (if given as column in the input file).",
        "<br>","<br>",
        "Scoring: Pathogenic: 0.99 or more points, Likely Pathogenic: 0.90 to 0.98 points, Variant of Uncertain Significance: 0.89 to −0.89 points, Likely Benign: −0.90 to −0.98 points, Benign: −0.99 or fewer points.",
        
        "<br>","<br>",
        
        "Automatically evaluated evidence categories by ClassifyCNV:", "<br>",
        "- for copy number losses: 1A/B, 2A-H, 3A-C, 4O","<br>",
        "- for copy number gains: 1A/B, 2A-H, 2J-L, 3A-C, 4O",
       
       "<br>","<br>",
       "<b>Information on evaluated evidence categories for copy number losses:</b>")),
      
      div(tableOutput("evidence_categories_table_loss"), style = "font-size:90%", align= "center"),
      
      
      HTML(paste0("<b>Information on evaluated evidence categories for copy number gains:</b>")),
      
      div(tableOutput("evidence_categories_table_gain"), style = "font-size:90%", align= "center"),

      footer = tagList(
        modalButton("OK"),
      ),
      size="xl"
    )))
  })
  
  
  scoresheet <- reactiveValues(table= NULL, files=NULL)
  
  observeEvent(input$submit_data_paste | input$submit_data_upload , { #| input$submit_example, {
   req(input$submit_data_upload > 0 | input$submit_data_paste > 0 ) #| input$submit_example > 0)
  # observeEvent(input$submit_data_paste | input$submit_data_upload | input$submit_example, { 
  #   req(input$submit_data_upload > 0 | input$submit_data_paste > 0 | input$submit_example > 0)
    #save table for classification
    
    write.table(uploaded_cnvs$table, file = paste0(tempdir(),"/",session$token,"uploaded_table.bed"), quote = F, sep='\t', row.names = F)
    system(paste0("rm -r ", tempdir(),"/results_",session$token))
    system(paste0("mkdir ", tempdir(), "/results_",session$token))
    system(paste0("python3 ClassifyCNV-master/ClassifyCNV.py --infile ", tempdir(),"/",session$token,"uploaded_table.bed --GenomeBuild hg19 --precise --outdir ",tempdir(), "/results_",session$token))
    scoresheet$table <- read_delim(paste0(tempdir(), "/results_",session$token,"/Scoresheet.txt"),
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE)

    system(paste0("rm -r ",tempdir(), "/results_",session$token))
    system(paste0("rm -r ",tempdir(),"/",session$token,"uploaded_table.bed"))

    #add score to uploaded table (merge by variantID)
    uploaded_cnvs$table$VariantID = paste0(uploaded_cnvs$table$CHR, "_", uploaded_cnvs$table$START, "_", uploaded_cnvs$table$END, "_", uploaded_cnvs$table$TYPE) 
    uploaded_cnvs$table <- merge(uploaded_cnvs$table, scoresheet$table, by= "VariantID", all.x=T)
    #remove variantID as first column
    uploaded_cnvs$table$Variant_ID = uploaded_cnvs$table$VariantID
    uploaded_cnvs$table$Size = paste0(round((as.numeric(uploaded_cnvs$table$END) - (as.numeric(uploaded_cnvs$table$START)-1))/1000000,2), " Mb")
    uploaded_cnvs$table = uploaded_cnvs$table[,-1]
    uploaded_cnvs$table$Score_details <- "0 points in all categories"
    tmp= uploaded_cnvs$table[ ,c(which(names(uploaded_cnvs$table) == "1A-B"):which(names(uploaded_cnvs$table) == "5H"))]
    
    for(i in 1:nrow(tmp)){
      if(rowSums(tmp[i,])>0){
        colnames= names(tmp)[tmp[i,]>0]
        tmp_row = as.data.frame(tmp[i, colnames])
        names(tmp_row) = colnames
        uploaded_cnvs$table$Score_details[i] = paste0("0 points in all categories besides: ", paste0(names(tmp_row), " (", tmp_row, " points)", collapse = ', '))
      }
    }
    
    #change classification based on additional points given in the POINTS column
    uploaded_cnvs$table$combined_score = rowSums(uploaded_cnvs$table[,c("POINTS", "Total score")], na.rm=TRUE)
    
    uploaded_cnvs$table <- uploaded_cnvs$table %>% mutate(Classification = case_when(combined_score >= 0.99 ~ 'Pathogenic',
                                                              combined_score >= 0.9 & combined_score <= 0.98 ~ 'Likely Pathogenic',
                                                              combined_score >= -0.89 & combined_score <= 0.89 ~ 'Uncertain Significance',
                                                              combined_score >= -0.98 & combined_score <= -0.9 ~ 'Likely Benign',
                                                              combined_score <= -0.99 ~ 'Benign'))
    
    
  })
  
  observeEvent(input$submit_example, {
  req(input$submit_example > 0)

   scoresheet$table <- read_delim(paste0("cnv_classification_details_chr9q33.tsv"),
                                  #paste0("cnv_classification_details.tsv"),
                                  #paste0("cnv_classification_details_platzer.tsv"),
                                  #paste0("cnv_classification_details_allan_2.tsv"),
                                  #paste0("cnv_classification_details_ausnz.tsv"),
                                  delim = "\t", escape_double = FALSE,
                                  trim_ws = TRUE)

   uploaded_cnvs$table$VariantID = paste0(uploaded_cnvs$table$CHR, "_", uploaded_cnvs$table$START, "_", uploaded_cnvs$table$END, "_", uploaded_cnvs$table$TYPE)
   uploaded_cnvs$table <- merge(uploaded_cnvs$table, scoresheet$table, by= "VariantID", all.x=T)
   #remove variantID as first column, add as VariantID
   uploaded_cnvs$table$Variant_ID = uploaded_cnvs$table$VariantID
   uploaded_cnvs$table$Size = paste0(round((as.numeric(uploaded_cnvs$table$END) - (as.numeric(uploaded_cnvs$table$START)-1))/1000000,2), " Mb")
   uploaded_cnvs$table = uploaded_cnvs$table[,-1]
   uploaded_cnvs$table$Score_details <- "0 points in all categories"
   tmp= uploaded_cnvs$table[ ,c(which(names(uploaded_cnvs$table) == "1A-B"):which(names(uploaded_cnvs$table) == "5H"))]
   
   for(i in 1:nrow(tmp)){
     if(rowSums(tmp[i,])>0){
       colnames= names(tmp)[tmp[i,]>0]
       tmp_row = as.data.frame(tmp[i, colnames])
       names(tmp_row) = colnames
       uploaded_cnvs$table$Score_details[i] = paste0("0 points in all categories besides: ", paste0(names(tmp_row), " (", tmp_row, " points)", collapse = ', '))
     }
   }
   
   #change classification based on additional points given in the POINTS column
   uploaded_cnvs$table$combined_score = rowSums(uploaded_cnvs$table[,c("POINTS", "Total score")], na.rm=TRUE)
   uploaded_cnvs$table <- uploaded_cnvs$table %>% mutate(Classification = case_when(combined_score >= 0.99 ~ 'Pathogenic',
                                                                                    combined_score >= 0.9 & combined_score <= 0.98 ~ 'Likely Pathogenic',
                                                                                    combined_score >= -0.89 & combined_score <= 0.89 ~ 'Uncertain Significance',
                                                                                    combined_score >= -0.98 & combined_score <= -0.9 ~ 'Likely Benign',
                                                                                    combined_score <= -0.99 ~ 'Benign'))
   
   })
  
  output$classify_table = renderDataTable(
    
    if(!is.null(scoresheet$table)){
      uploaded_scored_cnvs = data.frame("Chromosome" = uploaded_cnvs$table$CHR, 
                                        "Start" = uploaded_cnvs$table$START, 
                                        "End" = uploaded_cnvs$table$END, 
                                        "Type" = uploaded_cnvs$table$TYPE, 
                                        "ID" = uploaded_cnvs$table$ID, 
                                        "Size" = uploaded_cnvs$table$Size, 
                                        "Classification" = uploaded_cnvs$table$Classification, 
                                        "Total score" = uploaded_cnvs$table$combined_score, 
                                        "Score (ClassifyCNV)" = uploaded_cnvs$table$'Total score',
                                        "Score details (ClassifyCNV)" = uploaded_cnvs$table$Score_details,
                                        "Add. points by user" = uploaded_cnvs$table$POINTS)
                                          #shinyInput(actionButton, 27, 'evidence_info', label = "details", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ))
      
      uploaded_scored_cnvs
      
    }, escape = FALSE, 
    filter = 'top',
    rownames = FALSE,
    colnames= c("Chromosome","Start","End","Type","ID","Size",
                paste0("ACMG classification",tags$sup("1")),
                paste0("Total score",tags$sup("1")), 
                paste0("Score (ClassifyCNV)",tags$sup("1")), 
                paste0("Score details (ClassifyCNV)",tags$sup("1")), 
                paste0("Manual score by user",tags$sup("1"))
                ), 
    options = list(paging = FALSE, scrollY= "350px", scrollCollapse = TRUE, dom = 't',
                   columnDefs = list(list(className = 'dt-center', targets = "_all"))
                   ),
    selection = 'single',
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      tags$sup("1"),"Automated scoring of evidence categories (for deletions: 1A/B, 2A-H, 3A-C, 4O, and for duplications: 1A/B, 2A-H, 2J-L, 3A-C, 4O) by",
      htmltools::a(href="https://www.nature.com/articles/s41598-020-76425-3", "ClassifyCNV", target="_blank", style="color:#c1c1c1;"), 
      "based on the", 
      htmltools::a(href="https://www.gimjournal.org/article/S1098-3600(21)01300-9/fulltext", "2019 ACMG/ClinGen Technical Standards for CNVs.", target="_blank", style="color:#c1c1c1;"),
      "The final classification results from the score from ClassifyCNV and the additional score from manually evaluated evidence categories by the user (if given as column in the input file).",
      "Details about the evaluated evidence categories can be found in the help section of this track."
       #tags$a(id = "evidence_info",class = "action-link",style="color:#c1c1c1;",onclick = 'Shiny.onInputChange(\"select_button\",  evidence_info)',  "here.")
      #tags$button(id="evidence_info", type="button", class="btn btn-default action-button", onclick='Shiny.onInputChange(\"select_button\",  this.id)', "here.")
      #shinyInput(actionButton, 1, 'button_', label = "here.", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
      )
    )

  output$download_classification <- downloadHandler(
    filename = function() {
      #paste('data-', Sys.Date(), '.bed', sep='')
      paste0('cnv_classification_details', '.tsv')
    },
    content = function(con) {
      
      #remove columns of non-evaluated columns
      uploaded_cnvs$table[, -c( "2I", "4A","4B", "4C","4D","4E" ,"4F-H","4I","4J","4K", "4L","4M","4N","5A","5B", "5C","5D","5E" ,"5F","5G","5H")] 
      
      write.table(uploaded_cnvs$table, con, quote = FALSE, sep = "\t", row.names = FALSE)
    }
  )
  
  ####value boxes on top of analysis page 
  output$box1 <- renderValueBox({
    valueBox(nrow(uploaded_cnvs$table), "uploaded CNVs", color = "orange")
  })
  
  output$box2 <- renderValueBox({
    valueBox(sum(uploaded_cnvs$table$TYPE == "DEL"), "uploaded deletions", color = "orange")
  })
  
  output$box3 <- renderValueBox({
    valueBox(sum(uploaded_cnvs$table$TYPE == "DUP"), "uploaded duplications", color = "orange")
  })
  
  output$box4 <- renderValueBox({
    valueBox(length(unique(uploaded_cnvs$table$ID)), "unique IDs", color = "orange")
  })

  ##### MARKDOWN report #####
    ##### part1: initialize ranges, set to range of selected CNV and set markdown values of that CNV ####
  
  ranges <- reactiveValues(x = NULL, chr = NULL)

  markdown <- reactiveValues(chr=NULL, start=NULL, end=NULL, type= NULL, ISCN_id= NULL, 
                             number_genes=NA,  number_genediseasepairs = NA, overlapping_syndromes = NA, syndromes= NULL, gene_disease = NULL,
                             Classification = NULL,Total_score = NULL,points=NULL, section2_text = NULL,length=NULL,size=NULL,
                             "1A" = NULL,"1B" = NULL,
                             "2A"	= NULL,"2B"=NULL,"2C" = NULL, "2D" = NULL,"2E" = NULL, "2F"= NULL,"2G"= NULL,"2H"	= NULL,"2I"	= NULL,"2J"	= NULL,"2K"	= NULL,"2L"	= NULL,
                             "3A"	= NULL,"3B"	= NULL,"3C"	= NULL,
                             "4O"	= NULL,
                             number_haplogenes= NULL,  number_triplogenes= NULL,
                             loeuf = NULL, pLI= NULL, pHI= NULL, pTS = NULL, HI = NULL, TS_clingen = NULL, HI_clingen = NULL)
  
  observeEvent(input$classify_table_rows_selected, {
    
    ranges$x <- as.numeric(c(uploaded_cnvs$table[input$classify_table_rows_selected, c("START", "END")]))
    ranges$chr = as.character(uploaded_cnvs$table[input$classify_table_rows_selected, "CHR"])

    markdown$length= (as.numeric(uploaded_cnvs$table[input$classify_table_rows_selected, "END"]) - (as.numeric(uploaded_cnvs$table[input$classify_table_rows_selected, "START"])-1))/1000000
    markdown$size =  paste0(round(markdown$length,2), "Mb")
    markdown$chr = uploaded_cnvs$table[input$classify_table_rows_selected, "CHR"]
    markdown$start = format(uploaded_cnvs$table[input$classify_table_rows_selected, "START"], scientific=F)
    markdown$end = format(uploaded_cnvs$table[input$classify_table_rows_selected, "END"], scientific=F)
    markdown$Classification = uploaded_cnvs$table[input$classify_table_rows_selected, "Classification"]
    markdown$Total_score = uploaded_cnvs$table[input$classify_table_rows_selected, "Total score"]
    markdown$points =  uploaded_cnvs$table[input$classify_table_rows_selected, "POINTS"]
    markdown$combined_score = uploaded_cnvs$table[input$classify_table_rows_selected, "combined_score"]
    
    if(uploaded_cnvs$table[input$classify_table_rows_selected, "TYPE"] == "DEL"){
      markdown$section2_text = "Overlap with Established/Predicted HI or Established Benign Genes/Genomic Regions"
      markdown$type = "Copy number loss"
      markdown$ISCN_id = paste0("[GRCh37/hg19] ", chr_location()$`Chromosome location`[1], chr_location()$`Chromosome location`[nrow(chr_location())],
                         gsub("chr", "", markdown$chr),  "(", floor(ranges$x[1]), "_", ceiling(ranges$x[2]), ")x1")
    } else if(uploaded_cnvs$table[input$classify_table_rows_selected, "TYPE"] == "DUP"){
      markdown$section2_text = "Overlap with Established Triplosensitive (TS), Haploinsufficient (HI), or Benign Genes or Genomic Regions"
      markdown$type = "Copy number gain"
      markdown$ISCN_id = paste0("[GRCh37/hg19] ", chr_location()$`Chromosome location`[1], chr_location()$`Chromosome location`[nrow(chr_location())],
                        gsub("chr", "", markdown$chr),  "(", floor(ranges$x[1]), "_", ceiling(ranges$x[2]), ")x3")

    }

    #section1
      if(uploaded_cnvs$table[input$classify_table_rows_selected, "1A-B"] == 0){
        markdown$"1A" = 0
        markdown$"1B" = 0
      } else if (uploaded_cnvs$table[input$classify_table_rows_selected, "1A-B"] == -0.6) {
        markdown$"1A" = 0
        markdown$"1B" = -0.6
      }

    #section2
    markdown$"2A" = uploaded_cnvs$table[input$classify_table_rows_selected, "2A"]
    markdown$"2B" = uploaded_cnvs$table[input$classify_table_rows_selected, "2B"]
    markdown$"2C" = uploaded_cnvs$table[input$classify_table_rows_selected, "2C"]
    markdown$"2D" = uploaded_cnvs$table[input$classify_table_rows_selected, "2D"]
    markdown$"2E" = uploaded_cnvs$table[input$classify_table_rows_selected, "2E"]
    markdown$"2F" = uploaded_cnvs$table[input$classify_table_rows_selected, "2F"]
    markdown$"2G" = uploaded_cnvs$table[input$classify_table_rows_selected, "2G"]
    markdown$"2H" = uploaded_cnvs$table[input$classify_table_rows_selected, "2H"]
    markdown$"2I" = uploaded_cnvs$table[input$classify_table_rows_selected, "2I"]
    markdown$"2J" = ifelse(markdown$type == "Copy number gain",
                           paste(uploaded_cnvs$table[input$classify_table_rows_selected, "2J"], "points"), 
                           "Category not existent for copy number losses.")
    markdown$"2K" = ifelse(markdown$type == "Copy number gain",
                           paste(uploaded_cnvs$table[input$classify_table_rows_selected, "2K"], "points"), 
                           "Category not existent for copy number losses.")
    markdown$"2L" = ifelse(markdown$type == "Copy number gain",
                           paste(uploaded_cnvs$table[input$classify_table_rows_selected, "2L"], "points"), 
                           "Category not existent for copy number losses.")

    #section3
    if(uploaded_cnvs$table[input$classify_table_rows_selected, "3"] == 0){
      markdown$"3A" = 0
      markdown$"3B" = 0
      markdown$"3C" = 0
    } else if (uploaded_cnvs$table[input$classify_table_rows_selected, "3"] == 0.45) {
      markdown$"3A" = 0
      markdown$"3B" = 0.45
      markdown$"3C" = 0
    } else if (uploaded_cnvs$table[input$classify_table_rows_selected, "3"] == 0.9) {
      markdown$"3A" = 0
      markdown$"3B" = 0
      markdown$"3C" = 0.9
    }

    #section4
    markdown$"4O" = uploaded_cnvs$table[input$classify_table_rows_selected, "4O"]
    
    #decipher syndromes
    decipher_syndromes = intersect_table(data$decipher_syndromes, ranges)
    markdown$overlapping_syndromes = nrow(decipher_syndromes)
    markdown$syndromes = ifelse(nrow(decipher_syndromes)>0, paste(decipher_syndromes$Syndrome, collapse=", "), "/")
    
    #gene info
    filtered_genes = intersect_table(data$gene_table, ranges)
    
    markdown$number_genes = nrow(filtered_genes)
    
    if(nrow(filtered_genes)>0){
      
      haplogenes = filtered_genes %>%
        filter(pHI > 0.833 | pLI > 0.9 | loeuf < 0.35 | `%HI` < 10 | `HI Score ClinGen` == "3 (Sufficient Evidence)")
      markdown$number_haplogenes = nrow(haplogenes)
      
      triplogenes = filtered_genes %>%
        filter(pTS > 0.993 | `TS Score ClinGen` == "3 (Sufficient Evidence)")
      markdown$number_triplogenes= nrow(triplogenes)
      
      loeuf_genes = filtered_genes %>% filter(loeuf < 0.35)
      markdown$loeuf = ifelse(nrow(loeuf_genes)>0, paste(loeuf_genes$gene_name, collapse =", "), "/")
      
      pLI_genes = filtered_genes %>% filter(pLI > 0.9)
      markdown$pLI = ifelse(nrow(pLI_genes)>0, paste(pLI_genes$gene_name, collapse =", "), "/")
      
      pHI_genes = filtered_genes %>% filter(pHI > 0.833)
      markdown$pHI = ifelse(nrow(pHI_genes)>0, paste(pHI_genes$gene_name, collapse =", "), "/")
      
      pTS_genes = filtered_genes %>% filter(pTS > 0.993)
      markdown$pTS = ifelse(nrow(pTS_genes)>0, paste(pTS_genes$gene_name, collapse =", "), "/")
      
      HI_genes = filtered_genes %>% filter(`%HI` < 10)
      markdown$HI = ifelse(nrow(HI_genes)>0, paste(HI_genes$gene_name, collapse =", "), "/")
      
      TS_clingen_genes = filtered_genes %>% filter(`TS Score ClinGen` == "3 (Sufficient Evidence)")
      markdown$TS_clingen = ifelse(nrow(TS_clingen_genes)>0, paste(TS_clingen_genes$gene_name, collapse =", "), "/")
      
      HI_clingen_genes = filtered_genes %>% filter(`HI Score ClinGen` == "3 (Sufficient Evidence)")
      markdown$HI_clingen = ifelse(nrow(HI_clingen_genes)>0, paste(HI_clingen_genes$gene_name, collapse =", "), "/")
      
    } else {
      
      markdown$number_haplogenes= "0"
      markdown$number_triplogenes= "0"
      markdown$loeuf = "/"
      markdown$pLI = "/"
      markdown$pHI= "/"
      markdown$pTS = "/"
      markdown$HI = "/"
      markdown$TS_clingen = "/"
      markdown$HI_clingen = "/"
      
    }
    
    #clingen info
    clingen_table = as.data.frame(data$Clingen_Gene_Disease_Summary)
    clingen_table = clingen_table[clingen_table$`GENE SYMBOL` %in% filtered_genes$gene_name, ]
    clingen_table$CLASSIFICATION <- factor(clingen_table$CLASSIFICATION, ordered = TRUE, levels = c("Definitive", "Strong", "Moderate", "Limited", 
                                                                                    "No known Disease Relationship","Disputed", "Refuted"))
    clingen_table = clingen_table[order(clingen_table$CLASSIFICATION), ]
    
    markdown$number_genediseasepairs = nrow(clingen_table)
    markdown$gene_disease =  ifelse(nrow(clingen_table)>0, 
                                    paste(
                                      paste0(clingen_table$"GENE SYMBOL", " - ",clingen_table$"DISEASE LABEL", " (Classification: ", clingen_table$"CLASSIFICATION", ", ", 
                                             paste0("<a href='", clingen_table$"ONLINE REPORT", "' target='_blank'>", "report","</a>")
                                             ,")")
                                      , collapse="<br>"), 
                                    "/")

  })
  
    ##### part2: generate and download report ####

  output$selected_cnv = renderText({

    paste("Download report of selected variant:",
          "<hr>",
          paste0("Chromosome: ","<b>",markdown$chr, "</b>"), "<br>",
          paste0("Start: ","<b>",markdown$start, "</b>"), "<br>",
          paste0("End: ","<b>",markdown$end, "</b>"), "<br>",
          paste0("Type: ","<b>",markdown$type, "</b>"), "<br>",
          paste0("Size: ", "<b>", round(markdown$length,2), "Mb</b>"),
          "<br>")}
  )
  
  observeEvent(input$report_help, {
    showModal(modalDialog(
      HTML(paste("After selecting a CNV of interest in the table above a report in html format can be downloaded. The report contains summary information of the region, 
                 the classification of the CNV as well as the specific scores given in the different evidence categories from the ACMG criteria.",
                 sep = "<br>")),
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  output$download_report <- downloadHandler(

    filename = "CLINCNV_report.html",
    
    content = function(file) {
      
      tempReport <-
        file.path(tempdir(), "CLINCNV_report.Rmd")
      file.copy("CLINCNV_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        chr= markdown$chr, 
        start=markdown$start,
        end=markdown$end,
        type= markdown$type,
        ISCN_id= markdown$ISCN_id,
        number_genes=markdown$number_genes, 
        number_genediseasepairs= markdown$number_genediseasepairs,
        overlapping_syndromes = markdown$overlapping_syndromes, 
        Classification = markdown$Classification,
        Total_score = markdown$Total_score,
        combined_score = markdown$combined_score,
        size = markdown$size,
        points = markdown$points,
        section2_text = markdown$section2_text,
        "c1A" = markdown$"1A",
        "c1B" = markdown$"1B",
        "c2A"	= markdown$"2A",
        "c2B"	= markdown$"2B",
        "c2C"	= markdown$"2C",
        "c2D"	= markdown$"2D",
        "c2E"	= markdown$"2E",
        "c2F"	= markdown$"2F",
        "c2G"	= markdown$"2G",
        "c2H"	= markdown$"2H",	
        "c2I"	= markdown$"2I",
        "c2J"	= markdown$"2J",
        "c2K"	= markdown$"2K",
        "c2L"	= markdown$"2L",
        "c3A"	= markdown$"3A",
        "c3B"	= markdown$"3B",
        "c3C"	= markdown$"3C",
        "c4O"	= markdown$"4O",
        syndromes = markdown$syndromes,
        number_haplogenes = markdown$number_haplogenes,
        number_triplogenes = markdown$number_triplogenes,
        loeuf=markdown$loeuf,
        pLI=markdown$pLI,
        pHI=markdown$pHI,
        pTS=markdown$pTS,
        HI=markdown$HI,
        TS_clingen=markdown$TS_clingen,
        HI_clingen=markdown$HI_clingen,
        gene_disease=markdown$gene_disease)
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##### intersect data tables upon CNV selection  ######
  
  filtered_genes <- reactive({
    req(!is.null(ranges$x))
    filtered_genes = intersect_table(data$gene_table, ranges)
    filtered_genes
  })
  
  filtered_uploaded_cnvs <- reactive({
    req(!is.null(ranges$x))
    intersect_table(uploaded_cnvs$table, ranges)
  })
  
  filtered_decipher_syndromes <- reactive({
    req(!is.null(ranges$x))
    decipher_syndromes = intersect_table(data$decipher_syndromes, ranges)
    decipher_syndromes
  })
  
  filtered_clingen_gene_disease <- reactive({
    req(!is.null(filtered_genes()))
    table = as.data.frame(data$Clingen_Gene_Disease_Summary)
    table = table[table$`GENE SYMBOL` %in% filtered_genes()$gene_name, ]
    table$CLASSIFICATION <- factor(table$CLASSIFICATION, ordered = TRUE, levels = c("Definitive", "Strong", "Moderate", "Limited", 
                                                                                    "No known Disease Relationship","Disputed", "Refuted"))
    table$'Date of classification' = substr(table$`CLASSIFICATION DATE`, 1, 10) #remove time, just keep data 
    table = table[order(table$CLASSIFICATION), ]
    table
  })
  
  
  filtered_clingen_regions <- reactive({
    req(!is.null(ranges$x))
    intersect_table(data$ClinGen_region, ranges)
  })
  
  
  ##### open boxes upon CNV selection  ######
  collapse_helper = reactiveValues(x=TRUE) #only once at the start the boxes should automatically collapse
  
  observeEvent(req(!is.null(input$classify_table_rows_selected)),{
    if(collapse_helper$x){
      js$collapse("viewer_box")
      js$collapse("report_box")
      js$collapse("gene_box")
      collapse_helper$x = FALSE 
    }
  })
  
  ##### VIEWER ######
    observeEvent(input$viewer_help, {
      showModal(modalDialog(
        HTML(paste("<b>Ideogram</b>",
                   "",
                   "The ideogram shows the whole chromosome of the selected CNV with its chromosome bands.",
                   "To change the selected region you can select a region by click-and-drag on the plot.",
                   "<hr>",
                   "<b>Genes</b>",
                   "",
                   "In the gene track all protein-coding genes are shown. When hovering over the genes you get more information about the gene.
                   Genes that are dosage-sensitive, i.e. genes that are likely to cause a phenotypic effect, are shown in orange.",
                   "There are several dosage-sensitivity scores to choose from: loeuf, pLI, pHI, pTS, %HI.",
                   " ",
                   "LOEUF: Loss-of-function observed/expected upper bound fraction",
                   "Range: 0 < LOEUF < Inf",
                   "Low LOEUF scores indicate strong selection against predicted loss-of-function (pLoF) variation in a given gene",
                   "High LOEUF scores suggest a relatively higher tolerance to inactivation",
                   paste("LOEUF < 0.35 is used as a threshold for loss-of-function intolerance", 
                         tags$a(href="https://www.nature.com/articles/s41586-020-2308-7", "(Karczewski et al., 2020)", target="_blank")), "",
                   
                   "pLI: Probability of loss of function intolerance", 
                   "Range: 0 < pLI < 1",
                   "Genes with low pLI scores are considered relatively tolerant to protein truncating variations (frameshift, splice donor, splice acceptor, and stop-gain variants).",
                   "Genes with high pLI score are considered more intolerant to protein truncation variations",
                   paste("pLI > 0.9 is used as a threshold for loss-of-function intolerance", 
                         tags$a(href="https://www.nature.com/articles/nature19057", "(Lek et al., 2016)", target="_blank")), "",
                   
                   "pHI: Probability of haploinsufficiency",
                   "Range: 0 < pHI < 1",
                   "Genes with low pHI scores are considered less likely to be haplosensitive",
                   "Genes with high pHI scores are considered more likely to be haplosensitive",
                   paste("pHI > 0.84 is used as a threshold for haploinsufficient genes", 
                         tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "",
                   
                   "pTS: Probability of triplosensitivity",
                   "Range: 0 < pTS < 1",
                   "Genes with low pTS scores are considered less likely to be triplosensitive",
                   "Genes with high pTS scores are considered more likely to be triplosensitive",
                   paste("pTS > 0.993 is used as a threshold for triplosensitive genes", 
                         tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "",
                   
                   "%HI: Haploinsufficiency score",
                   "Range: 0% < %HI < 100% (Percentages refer to genome-wide percentiles of genes ranked according to their haploinsufficient score.)",
                   "Genes with high ranks (e.g. 0-10%) indicate a gene is more likely to exhibit haploinsufficiency",
                   "Genes with low ranks (e.g. 90%-100%) indicate a gene is more likely to not exhibit haploinsufficiency",
                   paste("%HI < 10% is used as a threshold for haploinsufficient genes", 
                         tags$a(href="https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1001154", "(Huang et al., 2021)", target="_blank")),
                   "<hr>",
                   "<b>Uploaded CNVs</b>",
                   "",
                   "In the 'Uploaded CNV' track all uploaded CNVs that intersect the selected region are shown. Deletions are visualized with red bars and duplications with 
                   blue bars while each row represents one sample/ID. 
                   When hovering over the start or end of the CNV a tooltip with information about the CNV is shown.",
                   "The uploaded CNVs can be filtered/ selected based on the Classification and their ID. 
                   In case binary phenotype variables were uploaded the CNVs can also be filtered based on those.",
                   "To change the selected region you can select a region by click-and-drag on the plot or use the zoom-in/ zoom-out button.",
                   "<hr>",
                   "<b>ClinVar CNVs</b>",
                   "",
                   paste("In the 'ClinVar' track the number of interescting pathogenic/ likely pathogenic deletions and duplications from", 
                         tags$a(href="https://www.ncbi.nlm.nih.gov/clinvar/", "ClinVar", target="_blank"),
                         "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                   "When hovering over the plot the number of deletions and duplications are shown.",
                   "To change the selected region you can select a region by click-and-drag on the plot or use the zoom-in/ zoom-out button.",
                   "<hr>",
                   "<b>UK Biobank CNVs</b>",
                   "",
                   paste("In 'UK Biobank' track the combined allele frequency of interescting deletions and duplications from", 
                         tags$a(href="https://doi.org/10.1016/j.ajhg.2019.07.001", "the UK Biobank", target="_blank"),
                         "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                   "To remove small CNVs/ structural variants, all CNVs < 50kb were removed prior to the calculation.",
                   "When hovering over the plot the allele frequency of deletions and duplications are shown.",
                   "To change the selected region you can select a region by click-and-drag on the plot or use the zoom-in/ zoom-out button.",
                   "<hr>",
                   "<b>GnomAD CNVs</b>",
                   "",
                   paste("In 'GnomAD' track the combined allele frequency of interescting deletions and duplications from", 
                         tags$a(href="https://doi.org/10.1016/j.ajhg.2019.07.001", "GnomAD", target="_blank"),
                         "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                   "To remove small CNVs/ structural variants, all CNVs < 50kb were removed prior to the calculation.",
                   "When hovering over the plot the allele frequency of deletions and duplications are shown.",
                   "To change the selected region you can select a region by click-and-drag on the plot or use the zoom-in/ zoom-out button.",
                   sep = "<br>")),
        footer = tagList(
          modalButton("OK"),
        )
      ))
    })
    
    
    observeEvent(input$zoom_out | input$zoom_out2 | input$zoom_out3 | input$zoom_out4 | input$zoom_out5 | input$zoom_out6 | input$zoom_out7, {
      length = ranges$x[2] - ranges$x[1]
      chr_length= max(data$cytoBand_hg19[data$cytoBand_hg19$chrom == ranges$chr, "chromEnd"])
      ranges$x[1] = max(1, ranges$x[1] - (length/2))
      ranges$x[2] = min(chr_length, ranges$x[2] + (length/2))
    }, ignoreInit = TRUE)
    
    observeEvent(input$zoom_in | input$zoom_in2 | input$zoom_in3 | input$zoom_in4 | input$zoom_in5| input$zoom_in6 | input$zoom_in7, {
      length = ranges$x[2] - ranges$x[1]
      chr_length= max(data$cytoBand_hg19[data$cytoBand_hg19$chrom == ranges$chr, "chromEnd"])
      ranges$x[1] = ranges$x[1] + (length/4)
      ranges$x[2] = ranges$x[2] - (length/4)
    }, ignoreInit = TRUE)
    
    observeEvent(input$zoom_back |  input$zoom_back2 |  input$zoom_back3|  input$zoom_back4|  input$zoom_back5|  input$zoom_back6|  input$zoom_back7, {
      ranges$x[1] = as.numeric(c(uploaded_cnvs$table[input$classify_table_rows_selected, "START"]))
      ranges$x[2] = as.numeric(c(uploaded_cnvs$table[input$classify_table_rows_selected, "END"]))
    })
    
    observeEvent(input$moveleft |input$moveleft2 |input$moveleft3 |input$moveleft4 |input$moveleft5 |input$moveleft6 |input$moveleft7, {
      length = ranges$x[2] - ranges$x[1]
      move_length = round(length*0.5, digits=0)
      
      ranges$x[1] = max(1, ranges$x[1] - move_length)
      ranges$x[2] = max(1, ranges$x[2] - move_length)

    }, ignoreInit = TRUE)
    
    observeEvent(input$moveleft_more| input$moveleft_more2 |input$moveleft_more3 |input$moveleft_more4 |input$moveleft_more5 |input$moveleft_more6 |input$moveleft_more7, {
      
      length = ranges$x[2] - ranges$x[1]
      move_length = round(length*0.95, digits=0)
      
      ranges$x[1] = max(1, ranges$x[1] - move_length)
      ranges$x[2] = max(1, ranges$x[2] - move_length)
      
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$moveright | input$moveright2 | input$moveright3| input$moveright4| input$moveright5| input$moveright6| input$moveright7, {

      length = ranges$x[2] - ranges$x[1]
      move_length = round(length*0.5, digits=0)

      chr_length= max(data$cytoBand_hg19[data$cytoBand_hg19$chrom == ranges$chr, "chromEnd"])
      ranges$x[1] = min(chr_length, ranges$x[1] + move_length)
      ranges$x[2] = min(chr_length, ranges$x[2] + move_length)

    }, ignoreInit = TRUE)

    observeEvent(input$moveright_more| input$moveright_more2|input$moveright_more3|input$moveright_more4|input$moveright_more5|input$moveright_more6|input$moveright_more7, {

      length = ranges$x[2] - ranges$x[1]
      move_length = round(length*0.95, digits=0)

      chr_length= max(data$cytoBand_hg19[data$cytoBand_hg19$chrom == ranges$chr, "chromEnd"])
      ranges$x[1] = min(chr_length, ranges$x[1] + move_length)
      ranges$x[2] = min(chr_length, ranges$x[2] + move_length)

    }, ignoreInit = TRUE)
                                                       

    observeEvent(input$ideogram_brush, {
      brush <- input$ideogram_brush
      req(!is.null(brush))
        ranges$x <- c(brush$xmin, brush$xmax)
    })
    
    zoomed_range <- reactive({
      if (!is.null(event_data("plotly_brushed"))){
        xmin <- event_data("plotly_brushed")$x[1]
        xmax <- event_data("plotly_brushed")$x[2]
        ymin <- event_data("plotly_brushed")$y[1]
        ymax <- event_data("plotly_brushed")$y[2]
        values <- list(x = c(xmin,xmax), y = c(ymin, ymax))
        return(values)
      } else(
        return(NULL)
      )
    })
    
    observe(if(!is.null(zoomed_range())){ranges$x <- zoomed_range()$x})   
    
  
    ## ideogram
    chr_location = reactive({chr_location = intersect_table(data$cytoBand_hg19, ranges)})
    
    output$selected_region_ideogram <- renderText(
      paste0("Selected region: GRCh37/hg19 ",
             chr_location()$`Chromosome location`[1],chr_location()$`Chromosome location`[nrow(chr_location())],
             "(", markdown$chr,":", floor(ranges$x[1]), "-", ceiling(ranges$x[2]),")")
    )
    
    output$ideogram <- renderPlot({
      chr_data = data$cytoBand_hg19[data$cytoBand_hg19$chrom == ranges$chr, ]
      
      p = ggplot(data= chr_data) +
        geom_rect(data= chr_data, aes(NULL,NULL,xmin=chromStart,xmax=chromEnd,fill=`Giemsa stain`,
                                      ymin=-10000,ymax=10000, text=paste(`Chromosome location`)), colour="black",size=0.5) +
        scale_fill_manual(breaks = c("gneg","gpos100","gpos75","gpos50","gpos25","stalk","gvar","acen"),
                          values=c("white", "black", "gray25","gray50","gray75","gray90","grey","#800000")) +
        scale_x_continuous(labels = label_number(suffix = " Mb", scale = 1e-6)) +
        theme(axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())+
        coord_cartesian(xlim = c(min(chr_data$chromStart), max(chr_data$chromEnd)), expand = FALSE)
      
      
      p <- p + geom_rect(data = chr_data[chr_data$`Giemsa stain` == "acen",],
                         aes(xmin = chromStart, xmax = chromEnd, ymin = -10000, ymax = 10000),
                         size = 0.5, fill = "#FFFFFF", color = "#FFFFFF")
      
      xmin= as.numeric(min(chr_data[chr_data$`Giemsa stain` == "acen",c("chromStart","chromEnd")]))
      xmax= as.numeric(max(chr_data[chr_data$`Giemsa stain` == "acen",c("chromStart","chromEnd")]))
      xmean= xmax - ((xmax-xmin)/2)
      centromer_data =data.frame(x=c(xmin,xmin,xmean, xmean, xmax, xmax),
                                 y=c(-10000,10000,0, 0,-10000,10000),
                                 t=c('a', 'a', 'a',  'b', 'b', 'b'))
      
      p <- p + geom_polygon(data=centromer_data, 
                            mapping=aes(x=x, y=y, group=t))
      
      if(!is.null(ranges$x)){
        p <- p + geom_rect(data = data.frame(),
                           aes(xmin = ranges$x[1], xmax = ranges$x[2], ymin=-12000, ymax=12000),
                           size = 1.5, fill = "#3c8dbc", color = "#3c8dbc", alpha=0.5)
      }
      
      p
      
    })
  
    
    ##### GENE PLOT #####
        
        observeEvent(input$viewer_help_color, {
          showModal(modalDialog(
            HTML(paste("Genes that are dosage-sensitive, i.e. genes that are likely to cause a phenotypic effect, are shown in orange.",
                       "There are several dosage-sensitivity scores to choose from: loeuf, pLI, pHI, pTS, %HI.",
                       " ", "<hr>",
                       "<b>LOEUF: Loss-of-function observed/expected upper bound fraction</b>", "<br>",
                       "Range: 0 < LOEUF < Inf", "<br>",
                       "Low LOEUF scores indicate strong selection against predicted loss-of-function (pLoF) variation in a given gene.", "<br>",
                       "High LOEUF scores suggest a relatively higher tolerance to inactivation.","<br>",
                       paste("LOEUF < 0.35 is used as a threshold for loss-of-function intolerance", 
                             tags$a(href="https://www.nature.com/articles/s41586-020-2308-7", "(Karczewski et al., 2020)", target="_blank")), "", "<hr>",
                       
                       "<b>pLI: Probability of loss of function intolerance</b>", "<br>",
                       "Range: 0 < pLI < 1","<br>",
                       "Genes with low pLI scores are considered relatively tolerant to protein truncating variations (frameshift, splice donor, splice acceptor, and stop-gain variants).","<br>",
                       "Genes with high pLI score are considered more intolerant to protein truncation variations.","<br>",
                       paste("pLI > 0.9 is used as a threshold for loss-of-function intolerance", 
                             tags$a(href="https://www.nature.com/articles/nature19057", "(Lek et al., 2016)", target="_blank")), "", "<hr>",
                       
                       "<b>pHI: Probability of haploinsufficiency</b>","<br>",
                       "Range: 0 < pHI < 1","<br>",
                       "Genes with low pHI scores are considered less likely to be haplosensitive.","<br>",
                       "Genes with high pHI scores are considered more likely to be haplosensitive.","<br>",
                       paste("pHI > 0.84 is used as a threshold for haploinsufficient genes", 
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "", "<hr>",
                       
                       "<b>pTS: Probability of triplosensitivity</b>","<br>",
                       "Range: 0 < pTS < 1","<br>",
                       "Genes with low pTS scores are considered less likely to be triplosensitive.","<br>",
                       "Genes with high pTS scores are considered more likely to be triplosensitive.","<br>",
                       paste("pTS > 0.993 is used as a threshold for triplosensitive genes", 
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "", "<hr>",
                       
                       "<b>%HI: Haploinsufficiency score</b>", "<br>",
                       "Range: 0% < %HI < 100% (Percentages refer to genome-wide percentiles of genes ranked according to their haploinsufficient score.)", "<br>",
                       "Genes with high ranks (e.g. 0-10%) indicate a gene is more likely to exhibit haploinsufficiency.","<br>",
                       "Genes with low ranks (e.g. 90%-100%) indicate a gene is more likely to not exhibit haploinsufficiency.","<br>",
                       paste("%HI < 10% is used as a threshold for haploinsufficient genes", 
                             tags$a(href="https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1001154", "(Huang et al., 2021)", target="_blank"))
            )),
            footer = tagList(
              modalButton("OK"),
            )
          ))
        })
      
      output$number_genes <- renderText({ 
          req(!is.null(ranges$x)) 
          paste0("Genes in region (n= ",nrow(filtered_genes()),")")
      })
      
      output$gene_plotly = renderPlotly({
        
        validate(need(!is.null(ranges$x), "Please select CNV of interest in table."))
        validate(need(nrow(filtered_genes()) > 0, "No genes are intersecting the selected CNV/ genomic region."))
        
        filtered_genes = filtered_genes()
        #colnames(filtered_genes) <- make.unique(names(filtered_genes))
    
        #show overlapping genes in different rows
        filtered_genes = filtered_genes %>% arrange(start)
        filtered_genes$display_row = NA
        row=c("z","y","x","e","d","c","b","a") #in order to show first row on top (alphabetically decreasing)
        n = 1
        while(sum(is.na(filtered_genes$display_row)) != 0){
          
          filtered_gene_data <- filtered_genes[which(is.na(filtered_genes$display_row)),]
          filtered_gene_data$display_row[1] <- row[n]
          
          if (nrow(filtered_gene_data) > 1){
            for(i in 2:nrow(filtered_gene_data)){
              if( sum(filtered_gene_data$start[i] > filtered_gene_data$end[1:i-1]) == i-1  ) {
                filtered_gene_data$display_row[i] = row[n]
              } else {
                filtered_gene_data$display_row[i] = NA
              }
            }
          }
          filtered_genes[which(is.na(filtered_genes$display_row)),"display_row"] = filtered_gene_data$display_row
          n = n+1
        }
        
        #add colour column depending on selected dosage sensitivity score
        if(input$gene_coloring == "scores_combined"){
          filtered_genes$col = ifelse(filtered_genes$loeuf <= 0.35 | filtered_genes$pLI > 0.9 | filtered_genes$pHI > 0.84 | filtered_genes$pTS > 0.993 | filtered_genes$'%HI' < 10, "#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$col)] = "#d3d3d3"
        } else if( input$gene_coloring == "loeuf"){
          filtered_genes$col = ifelse(filtered_genes$loeuf <= 0.35, "#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$loeuf)] = "#d3d3d3"
        } else if (input$gene_coloring == "pLI") {
          filtered_genes$col = ifelse(filtered_genes$pLI > 0.9, "#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$pLI)] = "#d3d3d3"
        } else if (input$gene_coloring == "pHI") {
          filtered_genes$col = ifelse(filtered_genes$pHI > 0.84,"#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$pHI)] = "#d3d3d3"
        } else if (input$gene_coloring == "pTS") {
          filtered_genes$col = ifelse(filtered_genes$pTS > 0.993,"#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$pTS)] = "#d3d3d3"
        } else if (input$gene_coloring == "%HI") {
          filtered_genes$col = ifelse(filtered_genes$'%HI' < 10,"#EC7C25", "#d3d3d3")
          filtered_genes$col[is.na(filtered_genes$'%HI')] = "#d3d3d3"
        }
        
        #dataset with only dosage sensitive genes (for gene annotations)
        hap_trip_genes = filtered_genes[filtered_genes$col == "#EC7C25", ]
        
        #plotly plot
        gene_fig <- plot_ly()
    
          for (i in 1:nrow(filtered_genes)){
              gene_fig <- add_trace(gene_fig,
                                    x = c(max(filtered_genes$start[i], ranges$x[1]), min(filtered_genes$end[i], ranges$x[2])), 
                                    y = c(filtered_genes$display_row[i], filtered_genes$display_row[i]), #y = c(1, 1), #
                                    mode = "lines",
                                    type = "scatter",
                                    line = list(color = filtered_genes$col[i], width = 8),
                                    showlegend = F,
                                    hoverinfo = "text",
                                    text = paste0("Gene: ", filtered_genes$gene_name[i], "<br>",
                                                  "pLI: ", filtered_genes$pLI[i], "<br>",
                                                  "pHI: ", filtered_genes$pHI[i], "<br>",
                                                  "pTS: ", filtered_genes$pTS[i], "<br>",
                                                  "LOEUF: ", filtered_genes$loeuf[i], "<br>",
                                                  "Position: ", filtered_genes$chrom[i], ": ", filtered_genes$start[i], " - ", filtered_genes$end[i]))
          }
        
        
        #add hover also in center of gene line
        center_of_genes = filtered_genes$start  + 0.5 * (filtered_genes$end - filtered_genes$start)
        
        gene_fig <- add_trace(gene_fig,
                                x = center_of_genes,
                                y = filtered_genes$display_row,
                                mode = "markers",
                                type = "scatter",
                                opacity = 0,
                                marker = list(size = 8,
                                              color = filtered_genes$col),
                                showlegend = F,
                                hoverinfo = "text",
                                text = paste0("Gene: ", filtered_genes$gene_name, "<br>",
                                              "pLI: ", filtered_genes$pLI, "<br>",
                                              "pHI: ", filtered_genes$pHI, "<br>",
                                              "pTS: ", filtered_genes$pTS, "<br>",
                                              "LOEUF: ", filtered_genes$loeuf, "<br>",
                                              "Position: ", filtered_genes$chrom, ": ", filtered_genes$start, " - ", filtered_genes$end))
      
        gene_fig <- gene_fig %>% 
          layout(xaxis = list(title = 'Genomic Position (Mb)',
                              range = c(ranges$x[1],ranges$x[2])),
                 yaxis = list(title = "",
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = FALSE,
                              showgrid = FALSE,
                              #autorange="reversed",
                              fixedrange = TRUE),
                 height = 100 + (50*length(unique(filtered_genes$display_row))), #depending on in how many rows genes are shown
                 font = list(family = "Arial"),
                 margin=list(t = 50, b = 20),
                 dragmode = "select",
                 selectdirection = "h") %>%
          event_register(event = "plotly_brushed") %>%
          config(displaylogo = FALSE, displayModeBar = FALSE)
        
      
        if(nrow(filtered_genes) <20){
          
          #gene_rows$n = 100 + (50*length(unique(filtered_genes$display_row)))
          
              gene_fig <- gene_fig %>% add_annotations(
                x = filtered_genes$start, #y = filtered_genes()$display_row,
                y = filtered_genes$display_row, #1,
                text = filtered_genes$gene_name,
                xref = "x",
                yref = "y", # showarrow = TRUE, # arrowhead = 4,
                arrowsize = .25  # ax = 20, # ay = -40
              ) %>%
                layout(margin=list(t = 100, b = 40)) 
        } else if(nrow(hap_trip_genes) >0){
          
          #gene_rows$n = 100 + (50*length(unique(hap_trip_genes$display_row)))
          
              gene_fig <- gene_fig %>% add_annotations(
                x = hap_trip_genes$start, #y = filtered_genes()$display_row,
                y = hap_trip_genes$display_row, #1,
                text = hap_trip_genes$gene_name,
                xref = "x",
                yref = "y", # showarrow = TRUE, # arrowhead = 4,
                arrowsize = .25  # ax = 20, # ay = -40
              ) %>%
              layout(margin=list(t = 100, b = 40)) 
         }
              
        gene_fig
        
      })
    
      output$gene_legend = renderPlot({
        
        legend <- data.frame(x=c(1,20), y=c(1, 1), text=c("Dosage sensitive based on selected gene score   ", "   Not dosage sensitive"))
        p <- ggplot(legend, aes(x=x, y=y, color=text))+
          geom_point(size = 6)+
          scale_color_manual(values = c("#d3d3d3", "#EC7C25"))+
          ylim(c(0,2))+
          xlim(c(0,30))+
          theme_void()+
          geom_text(aes(label=text), hjust=-0.1, color="black")+
          theme(legend.position = "none")
        
        p
        
      })
    
    ##### UPLOADED CNV plot#####
       
        variables <- reactive({
          
          uploaded_data <- uploaded_cnvs$table
          uploaded_data = intersect_table(uploaded_data, ranges)
    
          
          if(sum(grepl("FILTER_", names(uploaded_data))) > 0 ){
            filter_names = names(uploaded_data)[grepl("FILTER_", names(uploaded_data))]
            filter_names = gsub("FILTER_", '', filter_names)
            filter_names
          }else{
            NULL
          }
        })
      
      output$upload_cnv_filter_ui <- renderUI({
        uploaded_data <- uploaded_cnvs$table
        uploaded_data = intersect_table(uploaded_data, ranges)
        req(nrow(uploaded_data)> 0)
        uploaded_data$DELDUP <- ifelse(uploaded_data$TYPE == "DEL", "deletion", "duplication")
        
        if(length(variables()) > 0){
          tagList(
             column(width=2,
            selectizeInput("upload_cnv_filter",
                           p("Phenotype filter:",style = "color:grey"),
                           #width = '15%',
                           choices = unique(variables()),
                           multiple = TRUE,
                           selected = NULL,
                           options = list(plugins = list('drag_drop', 'remove_button')))),
            column(width=2,
            selectizeInput("cnv_type_filter",
                           p("Type filter:",style = "color:grey"),
                           #width = '15%',
                           choices = unique(uploaded_data$DELDUP),
                           multiple = TRUE,
                           selected = NULL,
                           options = list(plugins = list('drag_drop', 'remove_button')))),
            column(width=2,
            selectizeInput("classification_filter_filter",
                           p("Classification filter:",style = "color:grey"),
                           #width = '15%',
                           choices = unique(uploaded_data$Classification),
                           multiple = TRUE,
                           selected = NULL,
                           options = list(plugins = list('drag_drop', 'remove_button')))),
            column(width=2,
            selectizeInput("id_cnv_filter",
                           p("ID/Sample filter:",style = "color:grey"),
                           #width = '15%',
                           choices = unique(uploaded_data$ID),
                           multiple = TRUE,
                           selected = NULL,
                           options = list(plugins = list('drag_drop', 'remove_button'))))
          )
        } else {
          tagList(
            column(width=2,
                   selectizeInput("cnv_type_filter",
                                  p("Type filter:",style = "color:grey"),
                                  #width = '15%',
                                  choices = unique(uploaded_data$DELDUP),
                                  multiple = TRUE,
                                  selected = NULL,
                                  options = list(plugins = list('drag_drop', 'remove_button')))),
            column(width=2,
                   selectizeInput("classification_filter_filter",
                                  p("Clinical significance filter:",style = "color:grey"),
                                  #width = '15%',
                                  choices = unique(uploaded_data$Classification),
                                  multiple = TRUE,
                                  selected = NULL,
                                  options = list(plugins = list('drag_drop', 'remove_button')))),
            column(width=2,
                   selectizeInput("id_cnv_filter",
                                  p("ID/Sample filter:",style = "color:grey"),
                                  #width = '15%',
                                  choices = unique(uploaded_data$ID),
                                  multiple = TRUE,
                                  selected = NULL,
                                  options = list(plugins = list('drag_drop', 'remove_button'))))
            )
        }
      })
        
        uploaded_data_filtered <- reactive({
          req(!is.null(ranges$x))
     
              uploaded_data <- uploaded_cnvs$table
              uploaded_data = intersect_table(uploaded_data, ranges)
              
              if(nrow(uploaded_data) > 0){
                uploaded_data$DELDUP <- ifelse(uploaded_data$TYPE == "DEL", "deletion", "duplication")
                uploaded_data$color <- ifelse(uploaded_data$TYPE == "DEL", 'rgb(205, 12, 24)', 'rgb(22, 96, 167)')
                uploaded_data$id <- as.numeric(factor(uploaded_data$ID))
                
                if(length(variables())>0){
                  data= uploaded_data[,  grepl("FILTER_", names(uploaded_data))]
                  for(i in 1:nrow(data)){
                    if( sum(as.numeric(data[i,]) == 1) > 0) {
                      uploaded_data$phenotypes[i] = paste(gsub("FILTER_", '', names(data[ ,as.numeric(data[i,]) == 1])), collapse = ",")
                    }else{
                      uploaded_data$phenotypes[i] = "/"
                    }
                  }
                } else {
                  phenotypes =  "Not provided."
                }
                
                if(!is.null(variables()) & length(input$upload_cnv_filter) > 0){
                  tmp = uploaded_data[, names(uploaded_data) %in% paste0("FILTER_",input$upload_cnv_filter) ]
                  tmp = as.data.frame(tmp)
                  uploaded_data = uploaded_data[rowSums(tmp) == ncol(tmp), ]
                }
                
                if(length(input$classification_filter_filter) > 0){
                  uploaded_data = uploaded_data[uploaded_data$Classification %in% input$classification_filter_filter, ]
                }
                
                if(length(input$id_cnv_filter) > 0){
                  uploaded_data = uploaded_data[uploaded_data$ID %in% input$id_cnv_filter, ]
                }
                
                if(length(input$cnv_type_filter) > 0){
                  uploaded_data = uploaded_data[uploaded_data$DELDUP %in% input$cnv_type_filter, ]
                }
                
                uploaded_data$PLOT_START = ifelse(uploaded_data$start <= ranges$x[1], ranges$x[1], uploaded_data$start)
                uploaded_data$PLOT_END = ifelse(uploaded_data$end >= ranges$x[2], ranges$x[2], uploaded_data$end)
                
                uploaded_data <- uploaded_data[order(uploaded_data$TYPE, -uploaded_data$PLOT_START), ]
                
                return(as.data.frame(uploaded_data))
              
                } else {
                
                NULL
              }
    
        })
        
        output$number_cnvs <- renderText({ 
          req(!is.null(ranges$x)) 
          paste0("Uploaded CNV(s) in region (n= ",nrow(uploaded_data_filtered()),")")
        })
        
        output$upload_cnv_plotly <- renderPlotly({ 
          
          req(!is.null(ranges$x))
          validate(need(nrow(uploaded_data_filtered())>0, "No uploaded CNVs in this region."))
    
                cnv_upload_fig <- plot_ly()
                
                for (i in 1:nrow(uploaded_data_filtered())){
    
                  cnv_upload_fig <- add_trace(cnv_upload_fig,
                                              x = c(max(uploaded_data_filtered()$PLOT_START[i], ranges$x[1]), min(uploaded_data_filtered()$PLOT_END[i], ranges$x[2])), 
                                              y = c(i, i),  
                                              mode = "lines",
                                              type = "scatter",
                                              line = list(color = uploaded_data_filtered()$color[i], width = 5),
                                              showlegend = F,
                                              hoverinfo = "text",
                                              text = paste0("CNV: ", uploaded_data_filtered()$DELDUP[i], "<br>",
                                                            "ID: ", uploaded_data_filtered()$ID[i], "<br>",
                                                            "Position: ", uploaded_data_filtered()$start[i], "-", uploaded_data_filtered()$end[i], "<br>",
                                                            "Classification: ", uploaded_data_filtered()$Classification[i], "<br>",
                                                            "Phenotype: ",uploaded_data_filtered()$phenotypes[i]))
                }
                
                cnv_upload_fig <- cnv_upload_fig %>% layout(xaxis = list(title = 'Genomic Position (Mb)',
                                                                         range = c(ranges$x[1],ranges$x[2])),
                                                            yaxis = list(title = "",
                                                                         zeroline = FALSE,
                                                                         showline = FALSE,
                                                                         showticklabels = FALSE,
                                                                         showgrid = FALSE,
                                                                         fixedrange = TRUE),
                                                            height= 100 + (nrow(uploaded_data_filtered()) * 8),
                                                            margin=list(t = 50, b = 20),
                                                            dragmode = "select",
                                                            selectdirection = "h",
                                                            font = list(family = "Arial"))%>%
                  event_register(event = "plotly_brushed") %>%
                  config(displaylogo = FALSE, displayModeBar = FALSE)
                
                toWebGL(cnv_upload_fig)
        })
        
        output$cnv_legend = renderPlot({
          legend <- data.frame(x=c(1,15), y=c(1, 1), text=c("Deletion", "Duplication"))
          ggplot(legend, aes(x=x, y=y, color=text))+
            geom_point(size = 6)+
            scale_color_manual(values = c("#cd0c18","#1660a7"))+
            ylim(c(0,2))+
            xlim(c(0,30))+
            theme_void()+
            geom_text(aes(label=text), hjust=-0.4, color="black")+
            theme(legend.position = "none")
        })
        
        output$cnv_legend2 = renderPlot({
          legend <- data.frame(x=c(1,15), y=c(1, 1), text=c("Deletion", "Duplication"))
          ggplot(legend, aes(x=x, y=y, color=text))+
            geom_point(size = 6)+
            scale_color_manual(values = c("#cd0c18","#1660a7"))+
            ylim(c(0,2))+
            xlim(c(0,30))+
            theme_void()+
            geom_text(aes(label=text), hjust=-0.4, color="black")+
            theme(legend.position = "none")
        })
        
        output$cnv_legend3 = renderPlot({
          legend <- data.frame(x=c(1,15), y=c(1, 1), text=c("Deletion", "Duplication"))
          ggplot(legend, aes(x=x, y=y, color=text))+
            geom_point(size = 6)+
            scale_color_manual(values = c("#cd0c18","#1660a7"))+
            ylim(c(0,2))+
            xlim(c(0,30))+
            theme_void()+
            geom_text(aes(label=text), hjust=-0.4, color="black")+
            theme(legend.position = "none")
        })
        
        output$cnv_legend4 = renderPlot({
          legend <- data.frame(x=c(1,15), y=c(1, 1), text=c("Deletion", "Duplication"))
          ggplot(legend, aes(x=x, y=y, color=text))+
            geom_point(size = 6)+
            scale_color_manual(values = c("#cd0c18","#1660a7"))+
            ylim(c(0,2))+
            xlim(c(0,30))+
            theme_void()+
            geom_text(aes(label=text), hjust=-0.4, color="black")+
            theme(legend.position = "none")
        })
        
        output$cnv_legend5 = renderPlot({
          legend <- data.frame(x=c(1,15), y=c(1, 1), text=c("Deletion", "Duplication"))
          ggplot(legend, aes(x=x, y=y, color=text))+
            geom_point(size = 6)+
            scale_color_manual(values = c("#cd0c18","#1660a7"))+
            ylim(c(0,2))+
            xlim(c(0,30))+
            theme_void()+
            geom_text(aes(label=text), hjust=-0.4, color="black")+
            theme(legend.position = "none")
        })
    
    ##### CLINVAR summary ####
   clinvar_data_summary <- reactive({ 
    
     req(!is.null(ranges$chr))
     clinvar_data_summary_data = data$clinvar_freq_list[[ranges$chr]]
     clinvar_data_summary_data = intersect_table(clinvar_data_summary_data, ranges)
     return(as.data.frame(clinvar_data_summary_data))
     
   })
   
   output$clinvar_plot_summary <-  renderPlotly({ 
     req(!is.null(ranges$chr))
       plot <- plot_ly(clinvar_data_summary(), x = ~position, y = ~number_deletions, 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(205, 12, 24)', 
                                   width = 2, 
                                   shape = "spline"),
                       name = "Deletions") %>% 
         add_trace(y = ~number_duplications, 
                   line = list(color = 'rgb(22, 96, 167)', 
                               width = 2),
                   name = "Duplications") %>% 
         layout(xaxis = list(title = 'Genomic Position (Mb)',
                             range = c(ranges$x[1], ranges$x[2])),
                yaxis = list(title = "CNV counts in 200kb region",
                             zeroline = TRUE,
                             showline = TRUE,
                             showticklabels = TRUE,
                             showgrid = TRUE,
                             fixedrange = TRUE),
                font = list(family = "Arial"),
                hovermode = "x unified",
                margin=list(t = 50, b = 20),
                showlegend = FALSE,
                dragmode = "select",
                selectdirection = "h") %>%
         event_register(event = "plotly_brushed") %>%
         config(displaylogo = FALSE, displayModeBar = FALSE)

       toWebGL(plot)  
   })

   
   clinvar_data_list <- reactiveValues(clinvar_data1=NULL, clinvar_data2=NULL, clinvar_data3=NULL)
   
   clinvar_data <- observe({ 
     req(!is.null(ranges$chr))
     clinvar_data = data$clinvar_cnv_list[[ranges$chr]]
     clinvar_data = intersect_table(clinvar_data, ranges)
     
     clinvar_data$PLOT_START = ifelse(clinvar_data$start <= ranges$x[1], ranges$x[1], clinvar_data$start)
     clinvar_data$PLOT_END = ifelse(clinvar_data$end >= ranges$x[2], ranges$x[2], clinvar_data$end)
     
     if(!is.null(input$clinvarfilter_classification)){
       clinvar_data = clinvar_data[clinvar_data$Classification %in% input$clinvarfilter_classification,]
     }
     
     if(!is.null(input$clinvarfilter_type)){
       clinvar_data = clinvar_data[clinvar_data$Type %in% input$clinvarfilter_type,]
     }

     clinvar_data$color <- ifelse(clinvar_data$Type == "deletion", 'rgb(205, 12, 24)', 'rgb(22, 96, 167)')
     clinvar_data$Size <- paste0(round((clinvar_data$end - (clinvar_data$start -1))/1000000,2), " Mb")
     clinvar_data <- clinvar_data[order(clinvar_data$Type, -clinvar_data$PLOT_START),]

     clinvar_data_list$clinvar_data <- clinvar_data
     clinvar_data_list$clinvar_data1 <- clinvar_data[clinvar_data$Classification == "Pathogenic"|clinvar_data$Classification == "Likely pathogenic", ]
     clinvar_data_list$clinvar_data2 <- clinvar_data[clinvar_data$Classification == "Uncertain significance"| clinvar_data$Classification == "drug response"|clinvar_data$Classification == "conflicting data from submitters"|clinvar_data$Classification=="not provided", ]
     clinvar_data_list$clinvar_data3 <- clinvar_data[clinvar_data$Classification == "Benign"|clinvar_data$Classification == "Likely benign"|clinvar_data$Classification =="Benign/Likely benign", ]
   })

   output$clinvar_plot1 = renderPlotly(

     if(!is.null(clinvar_data_list$clinvar_data1)){
       
      clinvar_data1 = clinvar_data_list$clinvar_data1
      validate(need(nrow(clinvar_data1) < 200, "There are too many ClinVar CNVs in this region to display. Please select a smaller region or filter the CNVs."))
      validate(need(nrow(clinvar_data1) > 0, "No ClinVar CNVs in this region."))

      cnv_fig1 <- plot_ly()
        for (i in 1:nrow(clinvar_data1)){
             cnv_fig1 <- add_trace(cnv_fig1,
                                  x = c(clinvar_data1$PLOT_START[i], clinvar_data1$PLOT_END[i]),  # x0, x1
                                  y = c(i, i),  # y0, y1
                                  mode = "lines",
                                  type = "scatter",
                                  line = list(color = clinvar_data1$color[i], width = 2),
                                  showlegend = F,
                                  hoverinfo = "text",
                                  
                                  # Create custom hover text
                                  text = paste("Type: ", clinvar_data1$Type[i], "<br>",
                                               "Chromosome: ", clinvar_data1$chrom[i], "<br>",
                                               "Position: ", clinvar_data1$start[i], "-", clinvar_data1$end[i], "<br>",
                                               "Size: ", clinvar_data1$Size[i], "<br>",
                                               "% in region:", round(clinvar_data1$percentage_inregion[i],2), "<br>",
                                               "Classification:", clinvar_data1$Classification[i],"<br>",
                                               "Phenotype:", clinvar_data1$Phenotype[i])
             )}
      
      cnv_fig1 = cnv_fig1  %>% layout(xaxis = list(title = 'Genomic Position (Mb)',
                             range = c(ranges$x[1],ranges$x[2])),
                yaxis = list(title = "",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             fixedrange = TRUE),
                 margin=list(t = 50, b = 20),
                 font = list(family = "Arial"),
                 height= 100+(nrow(clinvar_data1) * 5),
                 dragmode = "select",
                 selectdirection = "h") %>%
        event_register(event = "plotly_brushed") %>%
        config(displaylogo = FALSE, displayModeBar = FALSE)
           
      toWebGL(cnv_fig1)

     }
   )
   
   output$clinvar_plot2 = renderPlotly(
     
     if(!is.null(clinvar_data_list$clinvar_data2)){
       
       clinvar_data2 = clinvar_data_list$clinvar_data2
       validate(need(nrow(clinvar_data2) < 200, "There are too many ClinVar CNVs in this region to display. Please select a smaller region or filter the CNVs."))
       validate(need(nrow(clinvar_data2) > 0, "No ClinVar CNVs in this region."))
      
       cnv_fig2 <- plot_ly()
       for (i in 1:nrow(clinvar_data2)){
         cnv_fig2 <- add_trace(cnv_fig2,
                               x = c(clinvar_data2$PLOT_START[i], clinvar_data2$PLOT_END[i]),  # x0, x1
                               y = c(i, i),  # y0, y1
                               mode = "lines",
                               type = "scatter",
                               line = list(color = clinvar_data2$color[i], width = 2),
                               showlegend = F,
                               hoverinfo = "text",
                               
                               # Create custom hover text
                               text = paste("Type: ", clinvar_data2$Type[i], "<br>",
                                            "Chromosome: ", clinvar_data2$chrom[i], "<br>",
                                            "Position: ", clinvar_data2$start[i], "-", clinvar_data2$end[i], "<br>",
                                            "Size: ", clinvar_data2$Size[i], "<br>",
                                            "% in region:", round(clinvar_data2$percentage_inregion[i],2), "<br>",
                                            "Classification:", clinvar_data2$Classification[i],"<br>",
                                            "Phenotype:", clinvar_data2$Phenotype[i])
         )}
       
       cnv_fig2 = cnv_fig2  %>% 
         layout(xaxis = list(title = 'Genomic Position (Mb)',
                             range = c(ranges$x[1],ranges$x[2])),
                yaxis = list(title = "",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             fixedrange = TRUE),
                margin=list(t = 50, b = 20),
                font = list(family = "Arial"),
                height= 100+(nrow(clinvar_data2) * 5),
                dragmode = "select",
                selectdirection = "h") %>%
         event_register(event = "plotly_brushed") %>%
         config(displaylogo = FALSE, displayModeBar = FALSE)
       
       toWebGL(cnv_fig2)
       
     }
   )
   
   output$clinvar_plot3 = renderPlotly(
     
     if(!is.null(clinvar_data_list$clinvar_data3)){
       
       clinvar_data3 = clinvar_data_list$clinvar_data3
       validate(need(nrow(clinvar_data3) < 200, "There are too many ClinVar CNVs in this region to display. Please select a smaller region or filter the CNVs."))
       validate(need(nrow(clinvar_data3) > 0, "No ClinVar CNVs in this region."))

       cnv_fig3 <- plot_ly()
       for (i in 1:nrow(clinvar_data3)){
         cnv_fig3 <- add_trace(cnv_fig3,
                               x = c(clinvar_data3$PLOT_START[i], clinvar_data3$PLOT_END[i]),  # x0, x1
                               y = c(i, i),  # y0, y1
                               mode = "lines",
                               type = "scatter",
                               line = list(color = clinvar_data3$color[i], width = 2),
                               showlegend = F,
                               hoverinfo = "text",
                               # Create custom hover text
                               text = paste("Type: ", clinvar_data3$Type[i], "<br>",
                                            "Chromosome: ", clinvar_data3$chrom[i], "<br>",
                                            "Position: ", clinvar_data3$start[i], "-", clinvar_data3$end[i], "<br>",
                                            "Size:", clinvar_data3$Size[i],
                                            "% in region:", round(clinvar_data3$percentage_inregion[i],2), "<br>",
                                            "Classification:", clinvar_data3$Classification[i],"<br>",
                                            "Phenotype:", clinvar_data3$Phenotype[i])
         )}
       
       cnv_fig3 = cnv_fig3  %>% 
         layout(xaxis = list(title = 'Genomic Position (Mb)',
                             range = c(ranges$x[1],ranges$x[2])),
                yaxis = list(title = "",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             fixedrange = TRUE),
                margin=list(t = 50, b = 20),
                font = list(family = "Arial"),
                height= 100+(nrow(clinvar_data3) * 5),
                dragmode = "select",
                selectdirection = "h") %>%
         event_register(event = "plotly_brushed") %>%
         config(displaylogo = FALSE, displayModeBar = FALSE)
       
       toWebGL(cnv_fig3)
       
     }
   )
   
   output$clinvar_table = renderDataTable(
         
     if(!is.null(clinvar_data_list$clinvar_data)){
          
       clinvar_data <-  clinvar_data_list$clinvar_data
       clinvar_data <- clinvar_data[order(clinvar_data$percentage_inregion, decreasing = T),]
       #clinvar_data$Length = clinvar_data$end - (clinvar_data$start -1)
       #clinvar_data$Size <- paste0(round(clinvar_data$Length/1000000,2), " Mb")
       clinvar_data$OtherIDs <- gsub(",",", ",clinvar_data$OtherIDs)
    
       
       clinvar_data %>%
         dplyr::select(chrom, start, end, Size, percentage_inregion, Type, Classification, Origin, Phenotype,  VariationID, OtherIDs) %>%
         mutate(percentage_inregion = round(percentage_inregion ,2)) %>%
         dplyr::rename(Chromosome = chrom) %>%
         dplyr::rename(Start = start) %>%
         dplyr::rename(End = end) %>%
         dplyr::rename(`% in region` = percentage_inregion) %>%
         mutate(VariationID = paste0("<a href='", "https://www.ncbi.nlm.nih.gov/clinvar/variation/",VariationID, "/","' target='_blank'>",VariationID,"</a>"))
         

         }, escape = FALSE, 
         filter = 'top',
         rownames = FALSE,
         options = list(paging = FALSE, scrollY= "250px", scrollCollapse = TRUE, dom = 't',
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                        )
   )
  
   
   output$clinvar_cnv_number <- renderText({
     paste0("<b>ClinVar CNVs (n= ",nrow(clinvar_data_list$clinvar_data),")</b>")
   })
   
   output$clinvar_cnv_number_1 <- renderText({
    paste0("Pathogenic/likely pathogenic CNVs (n= ",nrow(clinvar_data_list$clinvar_data1) ,")")
   })

   output$clinvar_cnv_number_2 <- renderText({
     paste0("CNVs of uncertain significance (n= ",nrow(clinvar_data_list$clinvar_data2) ,")")
   })

   output$clinvar_cnv_number_3 <- renderText({
     paste0("Benign/likely benign CNVs (n= ",nrow(clinvar_data_list$clinvar_data3) ,")")
   })

   
   output$clinvar_filter_ui <- renderUI({
       tagList(
         column(width=2,
                selectizeInput("clinvarfilter_classification", p("Classification filter:", style = "color:grey"),
                               choices = c("Pathogenic", "Likely pathogenic", "Uncertain significance",  "Likely benign",  "Benign/Likely benign" ,"Benign",  "conflicting data from submitters", "drug response","not provided" ),
                               multiple = TRUE,
                               selected = NULL,
                               options = list(plugins = list('drag_drop', 'remove_button'))
                )),
         column(width=2,
                selectizeInput("clinvarfilter_type", p("Type filter:", style = "color:grey"),
                               choices = c("deletion", "duplication"),
                               multiple = TRUE,
                               selected = NULL,
                               options = list(plugins = list('drag_drop', 'remove_button'))
                ))
         )
       }) 
     
     
    ##### UKB summary ####
   ukb_data_summary <- reactive({ 
     req(!is.null(ranges$chr))

     ukb_data_summary_data = data$ukb_freq_list[[ranges$chr]]
     ukb_data_summary_data = intersect_table(ukb_data_summary_data, ranges)
       
     return(as.data.frame(ukb_data_summary_data))
   })
   
   output$ukb_plot_summary <-  renderPlotly({ 
     
     req(!is.null(ranges$chr))
     validate(need(ranges$chr != "chrX" & ranges$chr != "chrY", "No data available for the selected chromosome."))
     
       plot <- plot_ly(ukb_data_summary(), x = ~position, y = ~deletions_freq, 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(205, 12, 24)', 
                                   width = 2, 
                                   shape = "spline"),
                       text = paste0(signif(ukb_data_summary()$deletions_freq, digits=3), " (n= ",round(ukb_data_summary()$deletions_freq*482734,0),")"),
                       hoverinfo = 'text',
                       name = "Deletions") %>% 
         add_trace(y = ~duplications_freq, 
                   line = list(color = 'rgb(22, 96, 167)', 
                               width = 2),
                   text = paste0(signif(ukb_data_summary()$duplications_freq, digits=3), " (n= ",round(ukb_data_summary()$duplications_freq*482734,0),")"),
                   hoverinfo = 'text',
                   name = "Duplications") %>% 
         layout(xaxis = list(title = 'Genomic Position (Mb)',
                             range = c(ranges$x[1],ranges$x[2])),
                yaxis = list(title = "CNV allele frequency in 200kb region",
                             zeroline = TRUE,
                             showline = TRUE,
                             showexponent = "all",
                             exponentformat = "e",
                             showticklabels = TRUE,
                             showgrid = TRUE,
                             fixedrange = TRUE),
                font = list(family = "Arial"),
                hovermode = "x unified",
                margin=list(t = 50, b = 20),
                showlegend = FALSE,
                dragmode = "select",
                selectdirection = "h") %>%
         event_register(event = "plotly_brushed") %>%
         config(displaylogo = FALSE, displayModeBar = FALSE)
       
       toWebGL(plot) 
   })
   
    ##### GnomAD summary ####
   gnomad_data_summary <- reactive({ 
     req(!is.null(ranges$chr))
     gnomad_data_summary_data = data$gnomad_freq_list[[ranges$chr]]
     gnomad_data_summary_data = intersect_table(gnomad_data_summary_data, ranges)
     return(as.data.frame(gnomad_data_summary_data))
   })
   
   output$gnomad_plot_summary <-  renderPlotly({ 
     req(!is.null(ranges$chr))
     plot <- plot_ly(gnomad_data_summary(), x = ~position, y = ~deletions_freq, 
                     type = 'scatter', mode = 'lines', 
                     line = list(color = 'rgb(205, 12, 24)', 
                                 width = 2, 
                                 shape = "spline"),
                     text = paste0(signif(gnomad_data_summary()$deletions_freq, digits=3), " (n= ",round(gnomad_data_summary()$deletions_freq*14891,2), ")"),
                     hoverinfo = 'text',
                     name = "Deletions") %>% 
       add_trace(y = ~duplications_freq, 
                 line = list(color = 'rgb(22, 96, 167)', 
                             width = 2),
                 text = paste0(signif(gnomad_data_summary()$duplications_freq, digits=3), " (n= ",round(gnomad_data_summary()$duplications_freq*14891,2), ")"),
                 hoverinfo = 'text',
                 name = "Duplications") %>% 
       layout(xaxis = list(title = 'Genomic Position (Mb)',
                      range = c(ranges$x[1],ranges$x[2])),
         yaxis = list(title = "CNV allele frequency in 200kb region",
                      zeroline = TRUE,
                      showline = TRUE,
                      showexponent = "all",
                      exponentformat = "e",
                      showticklabels = TRUE,
                      showgrid = TRUE,
                      fixedrange = TRUE),
         font = list(family = "Arial"),
         hovermode = "x unified",
         margin=list(t = 50, b = 20),
         showlegend = FALSE,
         dragmode = "select",
         selectdirection = "h") %>%
       event_register(event = "plotly_brushed") %>%
       config(displaylogo = FALSE, displayModeBar = FALSE)
     
     toWebGL(plot) 
   })
  
  ##### TABLES ######
   
   observeEvent(input$gene_table_help, {
     showModal(modalDialog(
       HTML(paste("<b>Description of scores:</b>","",
                  
                  "<b>LOEUF:</b> Loss-of-function observed/expected upper bound fraction",
                  "Range: 0 < LOEUF < Inf",
                  "Low LOEUF scores indicate strong selection against predicted loss-of-function (pLoF) variation in a given gene",
                  "High LOEUF scores suggest a relatively higher tolerance to inactivation",
                  paste("LOEUF < 0.35 is used as a threshold for loss-of-function intolerance", 
                        tags$a(href="https://www.nature.com/articles/s41586-020-2308-7", "(Karczewski et al., 2020)", target="_blank")), "",
                  
                  "<b>pLI:</b> Probability of loss of function intolerance", 
                  "Range: 0 < pLI < 1",
                  "Genes with low pLI scores are considered relatively tolerant to protein truncating variations (frameshift, splice donor, splice acceptor, and stop-gain variants).",
                  "Genes with high pLI score are considered more intolerant to protein truncation variations",
                  paste("pLI > 0.9 is used as a threshold for loss-of-function intolerance", 
                        tags$a(href="https://www.nature.com/articles/nature19057", "(Lek et al., 2016)", target="_blank")), "",
                  
                  "<b>pHI:</b> Probability of haploinsufficiency",
                  "Range: 0 < pHI < 1",
                  "Genes with low pHI scores are considered less likely to be haplosensitive",
                  "Genes with high pHI scores are considered more likely to be haplosensitive",
                  paste("pHI > 0.84 is used as a threshold for haploinsufficient genes", 
                        tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "",
                  
                  "<b>pTS:</b> Probability of triplosensitivity",
                  "Range: 0 < pTS < 1",
                  "Genes with low pTS scores are considered less likely to be triplosensitive",
                  "Genes with high pTS scores are considered more likely to be triplosensitive",
                  paste("pTS > 0.993 is used as a threshold for triplosensitive genes", 
                        tags$a(href="https://www.medrxiv.org/content/10.1101/2021.01.26.21250098v1", "(Collins et al., 2021)", target="_blank")), "",
                  
                  "<b>%HI:</b> Haploinsufficiency score",
                  "Range: 0% < %HI < 100% (Percentages refer to genome-wide percentiles of genes ranked according to their haploinsufficient score.)",
                  "Genes with high ranks (e.g. 0-10%) indicate a gene is more likely to exhibit haploinsufficiency",
                  "Genes with low ranks (e.g. 90%-100%) indicate a gene is more likely to not exhibit haploinsufficiency",
                  paste("%HI < 10% is used as a threshold for haploinsufficient genes", 
                        tags$a(href="https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1001154", "(Huang et al., 2021)", target="_blank")), "",
                  
                  paste("<b>HI Score ClinGen:</b> Haploinsufficiency score given by the",
                        tags$a(href="https://www.clinicalgenome.org/curation-activities/dosage-sensitivity/", "ClinGen Dosage Sensitivity Curation task team.", target="_blank")),
                  "Possible scores are: 0 (No Evidence), 1 (Little Evidence), 2 (Emerging Evidence), 3 (Sufficient Evidence), 30 (Autosomal Recessive), 40 (Dosage Sensitivity Unlikely)", "",
                  
                  paste("<b>TS Score ClinGen:</b Triplosensitivity score given by the",
                        tags$a(href="https://www.clinicalgenome.org/curation-activities/dosage-sensitivity/", "ClinGen Dosage Sensitivity Curation task team.", target="_blank")),
                  "Possible scores are: 0 (No Evidence), 1 (Little Evidence), 2 (Emerging Evidence), 3 (Sufficient Evidence), 40 (Dosage Sensitivity Unlikely)",
                  
                  sep = "<br>")),
       footer = tagList(
         modalButton("OK"),
       )
     ))
   })
   
   output$gene_table <- DT::renderDataTable(DT::datatable({
     
     validate(need(!is.null(ranges$x), "Please select CNV of interest in table."))
     validate(need(nrow(filtered_genes()) > 0, "No genes are intersecting the selected CNV/ genomic region."))
     
     filtered_genes = filtered_genes()
     filtered_genes$pLI = as.numeric(filtered_genes$pLI)
     filtered_genes$loeuf = as.numeric(filtered_genes$loeuf)
     filtered_genes <- filtered_genes[order(filtered_genes$loeuf),]
     
     filtered_genes = filtered_genes %>%
       dplyr::select(gene_name, transcript, chrom, start, end, percentage_inregion, loeuf, pLI, pHI, pTS, '%HI', 'HI Score ClinGen', 'TS Score ClinGen', OMIM_links) %>%
       mutate(pLI = round(pLI ,5)) %>%
       dplyr::rename(Gene = gene_name) %>%
       dplyr::rename(Transcript = transcript) %>%
       dplyr::rename(`% in region` = percentage_inregion) %>%
       dplyr::rename(OMIM = OMIM_links) %>%
       dplyr::rename(Chromosome = chrom) %>%
       dplyr::rename(Start = start) %>%
       dplyr::rename(End = end)
     
     filtered_genes
     
   }, escape = FALSE, 
   rownames = FALSE,
   filter = 'top',
   options = list(paging = FALSE, scrollY= "500px", scrollCollapse = TRUE, dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                     if (parseFloat(aData[6]) < 0.35)
                                                     $("td:eq(6)", nRow).css("background-color", "aliceblue");
                                                     if (parseFloat(aData[7]) > 0.95)
                                                     $("td:eq(7)", nRow).css("background-color", "aliceblue");
                                                     if (parseFloat(aData[8]) > 0.84)
                                                     $("td:eq(8)", nRow).css("background-color", "aliceblue");
                                                     if (parseFloat(aData[9]) > 0.993)
                                                     $("td:eq(9)", nRow).css("background-color", "aliceblue");
                                                     if (parseFloat(aData[10]) < 10)
                                                     $("td:eq(10)", nRow).css("background-color", "aliceblue");
                                                                               }')
   ),
   caption = paste0("All genes (n=", nrow(filtered_genes),") in selected region")))
   
   filtered_genes_download = reactive({
     
     filtered_genes = filtered_genes()
     filtered_genes$pLI = as.numeric(filtered_genes$pLI)
     filtered_genes$loeuf = as.numeric(filtered_genes$loeuf)
     filtered_genes <- filtered_genes[order(filtered_genes$loeuf),]
     
     filtered_genes = filtered_genes %>%
       dplyr::select(gene_name, transcript, chrom, start, end, percentage_inregion, loeuf, pLI, pHI, pTS, '%HI', 'HI Score ClinGen', 'TS Score ClinGen', 
                     "OMIM ID", "NCBI gene ID", "HGNC ID", "UniProt accession", "Orphanet ID") %>%
       mutate(pLI = round(pLI ,5)) 
     
   })
   
   output$download_genetable <- downloadHandler(
     req(!is.null(filtered_genes_download())),
     
     filename = function() {
       paste0('gene_table_',ranges$chr,"_",ranges$x[1],"-",ranges$x[2],'.tsv')
     },
     content = function(con) {
       write.table(filtered_genes_download(), con, quote = FALSE, sep = "\t", row.names = FALSE)
     }
   )
   
   observeEvent(input$gene_disease_table_help, {
     showModal(modalDialog(
       HTML(paste("The ClinGen Gene Curation working group has developed a framework to standardize the approach to determine the clinical validity for a gene-disease pair.",
                  "The ClinGen Gene-Disease Clinical Validity curation process involves evaluating the strength of evidence supporting or refuting a claim that variation in a particular gene causes a particular disease.",
                  "Classifications derived with this framework are reviewed and confirmed or adjusted based on clinical expertise of appropriate disease experts.",
                  "Possible classifications are: Definitive > Strong > Moderate > Limited > No known Disease Relationship > Disputed Evidence > Refuted Evidence.",
                  paste("More information can be found", tags$a(href="https://clinicalgenome.org/curation-activities/gene-disease-validity/", "here", target="_blank"), "or",
                        tags$a(href="https://pubmed.ncbi.nlm.nih.gov/28552198/", "here.", target="_blank")),
                  sep = "<br>")),
       footer = tagList(
         modalButton("OK"),
       )
     ))
   })
   
   output$clingen_table <- DT::renderDataTable(DT::datatable({
     
     validate(need(!is.null(ranges$x), "Please select CNV of interest in table."))
     validate(need(nrow(filtered_clingen_gene_disease()) > 0, "No genes in your selected region are associated to a disease in ClinGen."))
     
     filtered_clingen_gene_disease = filtered_clingen_gene_disease()
     
     filtered_clingen_gene_disease = filtered_clingen_gene_disease %>%
       dplyr::rename(Gene= "GENE SYMBOL") %>%
       dplyr::rename(Disease= "DISEASE LABEL") %>%
       dplyr::rename("Disease ID (MONDO)" = "DISEASE ID (MONDO)") %>%
       dplyr::rename("Mode of inheritance" = "MOI") %>%
       dplyr::rename("Classification" = "CLASSIFICATION") %>%
       dplyr::rename("Online_report" = "ONLINE REPORT") %>%
       mutate("Online_report" = paste0("<a href='", Online_report,"' target='_blank'>", "link","</a>")) %>%
       dplyr::rename("Online report" = "Online_report") %>%
       dplyr::select("Gene", "Disease", "Disease ID (MONDO)", "Mode of inheritance", "Classification", "Date of classification", "Online report")
     
     filtered_clingen_gene_disease
     
   }, escape = FALSE, 
   rownames = FALSE,
   filter = 'top',
   options = list(paging = FALSE, scrollY= "500px", scrollCollapse = TRUE, dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
   )
   ),
   caption = "Expert-curated gene disease associations from ClinGen.")
   
   
   observeEvent(input$clingen_region_table_help, {
     showModal(modalDialog(
       HTML(paste("The ClinGen Dosage Sensitivity curation process collects evidence supporting/refuting the haploinsufficiency and triplosensitivity of genomic regions.",
                  "Classifications derived with this framework are reviewed and confirmed or adjusted based on clinical expertise of appropriate disease experts.",
                  "Possible scores for the Haploinsuficiency score (HI Score) and Triplosensitivity score (TS Score) are: 
                   0 (No Evidence), 1 (Little Evidence), 2 (Emerging Evidence), 3 (Sufficient Evidence), 40 (Dosage Sensitivity Unlikely)",
                  paste("More information can be found", tags$a(href="https://www.clinicalgenome.org/curation-activities/dosage-sensitivity/", "here.", target="_blank")),
                  sep = "<br>")),
       footer = tagList(
         modalButton("OK"),
       )
     ))
   })
   
   output$clingen_region_table <- DT::renderDataTable(DT::datatable({
     
     validate(need(!is.null(ranges$x), "Please select CNV of interest in table."))
     validate(need(nrow(filtered_clingen_regions())>0, "There are no classified regions by ClinGen that intersect your selected CNV/region."))
     
     filtered_clingen_regions = filtered_clingen_regions()
     filtered_clingen_regions = filtered_clingen_regions[,1:7]
     
     filtered_clingen_regions = filtered_clingen_regions %>%
       dplyr::rename(Chromosome= chrom) %>%
       dplyr::rename(Start= start) %>%
       dplyr::rename(End= end) %>%
       dplyr::rename('Last Eval. by ClinGen'= 'Last Eval.')
     
     filtered_clingen_regions
     
   }, escape = FALSE, 
   rownames = FALSE,
   filter = 'top',
   options = list(paging = FALSE, scrollY= "500px", scrollCollapse = TRUE, dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
   )
   ),
   caption = "Expert-curated genomic regions from ClinGen.")
   
   
   observeEvent(input$decipher_syndromes_table_help, {
     showModal(modalDialog(
       HTML(paste("The table shows expert-curated microdeletion and microduplication syndromes involved in developmental disorders from DECIPHER that intersect the selected region.",
                  paste("All syndromes and more information can be found at", tags$a(href="https://www.deciphergenomics.org/disorders/syndromes/list", "DECIPHER.", target="_blank")),
                  sep = "<br>")),
       footer = tagList(
         modalButton("OK"),
       )
     ))
   })
   
   output$decipher_syndromes <- DT::renderDataTable(DT::datatable({
     
     validate(need(!is.null(ranges$x), "Please select CNV of interest in table."))
     validate(need(nrow(filtered_decipher_syndromes())>0, "The selected region is not associated with expert-curated microdeletion and microduplication syndromes from DECIPHER."))
     
     filtered_decipher_syndromes = filtered_decipher_syndromes()
     filtered_decipher_syndromes = filtered_decipher_syndromes[,1:6]
     
     filtered_decipher_syndromes = filtered_decipher_syndromes %>%
       dplyr::rename(Chromosome= chrom) %>%
       dplyr::rename(Start= start) %>%
       dplyr::rename(End= end)
     
   }, escape = FALSE, 
   rownames = FALSE,
   filter = 'top',
   options = list(paging = FALSE, scrollY= "500px", scrollCollapse = TRUE, dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
   )
   ),
   caption = "Intersecting developmental syndromes from DECIPHER.")
   
  ##### GSEA #####
    
    observeEvent(input$gsea_help, {
      showModal(modalDialog(
        HTML(paste("Enrichment analysis is a computational method for inferring knowledge about an input gene set by comparing it to annotated gene sets representing prior biological knowledge.",
                   "Here, all genes from the selected region are used as input to analyze whether the genes significantly overlap with an annotated gene set from the libraries.
                                                    Libraries can be changed using the dropdown menu.", 
                   paste("More information about enrichment analyses can be found", tags$a(href="https://pubmed.ncbi.nlm.nih.gov/19033363/", "here", target="_blank"),
                         "and descriptions of the available libraries can be found", tags$a(href="https://maayanlab.cloud/Enrichr/#libraries", "here.", target="_blank")),
                   sep = "<br>")),
        footer = tagList(
          modalButton("OK"),
        )
      ))
    })
    
    enriched1 <- reactive({
      req(!is.null(filtered_genes()))
        db1 <- input$enrichr_db1
        enriched <- enrichr(genes = filtered_genes()$gene_name, databases = db1)
        enriched <- as.data.frame(enriched[[1]])
        enriched$log10_p <- -log10(enriched$Adjusted.P.value)
        return(as.data.frame(enriched))
    })
    
    enriched2 <- reactive({
      req(!is.null(filtered_genes()))
        db2 <- input$enrichr_db2
        enriched <- enrichr(genes = filtered_genes()$gene_name, databases = db2)
        enriched <- as.data.frame(enriched[[1]])
        enriched$log10_p <- -log10(enriched$Adjusted.P.value)
        return(as.data.frame(enriched))
    })
    
    output$enrichR_output_1 <- renderPlotly({
      req(!is.null(enriched1()))
        enriched1 = enriched1()
        enriched1$color = ifelse(enriched1$Adjusted.P.value < 0.05, "#FF0000", "#D3D3D3")
        fig1 <- plot_ly(data = enriched1, x= ~log10_p, y=~Odds.Ratio,
                        type = "scatter",
                        mode = "markers",
                        marker = list(color = ~color),
                        hoverinfo = "text",
                        text = ~Term,
                        showlegend = F
        ) %>%
          layout(title = "",
                 xaxis = list(title = '-log10(p)'),
                 yaxis = list(title = "Odds Ratio",
                              range = max(max(enriched1$Odds.Ratio), max(enriched2()$Odds.Ratio))),
                 font = list(family = "Arial"),
                 shapes = list(list(
                   type = "line",
                   x0 = 1.3,
                   x1 = 1.3,
                   xref = "paper",
                   y0 = 0,
                   y1 = 1,
                   line = list(color = "red", dash="dot"))
                 )) %>% 
          config(displaylogo = FALSE, displayModeBar = FALSE)
        
        toWebGL(fig1)
    })
    
    output$enrichR_output_2 <- renderPlotly({
      req(!is.null(enriched2()))
        enriched2 = enriched2()
        enriched2$color = ifelse(enriched2$Adjusted.P.value < 0.05, "#FF0000", "#D3D3D3")
        fig1 <- plot_ly(data = enriched2, x= ~log10_p, y=~Odds.Ratio,
                        type = "scatter",
                        mode = "markers",
                        marker = list(color = ~color),
                        hoverinfo = "text",
                        text = ~Term,
                        showlegend = F
        ) %>%
          layout(title = "",
                 xaxis = list(title = '-log10(p)'),
                 yaxis = list(title = "Odds Ratio",
                              range = max(max(enriched2$Odds.Ratio), max(enriched1()$Odds.Ratio))),
                 font = list(family = "Arial"),
                 shapes = list(list(
                   type = "line",
                   x0 = 1.3,
                   x1 = 1.3,
                   xref = "paper",
                   y0 = 0,
                   y1 = 1,
                   line = list(color = "red", dash="dot")))
          ) %>% 
          config(displaylogo = FALSE, displayModeBar = FALSE)
        
        toWebGL(fig1)
    })
    
    #enrichr table
    output$enrichR_table_1 <- DT::renderDataTable(DT::datatable({
      req(!is.null(enriched1()))
        enriched <- enriched1()
        enriched$Genes <- gsub(";", ", ", enriched$Genes)
        small_enriched <- cbind(Term = enriched$Term, 
                                'Overlap of genes' = enriched$Overlap,
                                'Adjusted p-value' = format(enriched$Adjusted.P.value,scientific = T, digits =3),
                                'Odds Ratio' = round(enriched$Odds.Ratio,3),
                                Genes = enriched$Genes
        )
        small_enriched
    }, escape = FALSE, 
    rownames = FALSE,
    options = list("pageLength" = 5, 
                   autoWidth = TRUE,
                   rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                      // Bold and green cells for conditions
                                      if (parseFloat(aData[2]) < 0.05)
                                      $("td:eq(2)", nRow).css("font-weight", "bold");
                                      if (parseFloat(aData[2]) < 0.05)
                                      $("td:eq(2)", nRow).css("background-color", "#FF0000");}')
    ),
    caption = " "))
    
    output$enrichR_table_2 <- DT::renderDataTable(DT::datatable({
      req(!is.null(enriched2()))
        enriched <- enriched2()
        enriched$Genes <- gsub(";", ", ", enriched$Genes)
        small_enriched <- cbind(Term = enriched$Term, 
                                'Overlap of genes' = enriched$Overlap,
                                'Adjusted p-value' = format(enriched$Adjusted.P.value,scientific = T, digits =3),
                                'Odds Ratio' = round(enriched$Odds.Ratio,3),
                                Genes = enriched$Genes
        )
        small_enriched
    }, escape = FALSE,
    rownames = FALSE,
    options = list("pageLength" = 5,
                   autoWidth = TRUE,
                   rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                      // Bold and green cells for conditions
                                      if (parseFloat(aData[2]) < 0.05)
                                      $("td:eq(2)", nRow).css("font-weight", "bold");
                                      if (parseFloat(aData[2]) < 0.05)
                                      $("td:eq(2)", nRow).css("background-color", "#FF0000");}')
    ),
    caption = " "))

      
  ##### ABOUT #####
    
    output$data_sources = renderTable({
      data_sources <- data$data_sources
      data_sources <- data_sources[,-ncol(data_sources)]
      data_sources
    }, sanitize.text.function = function(x) x)
    
    output$gene_scores = renderTable({
      gene_scores <- data$gene_scores 
      gene_scores
    }, sanitize.text.function = function(x) x)
    
    output$example_cnvs2 = renderTable({
      example_cnvs = as.data.frame(data$example_cnvs)
      example_cnvs[,6] = as.integer(example_cnvs[,6])
      example_cnvs[,7] = as.integer(example_cnvs[,7])
      head(example_cnvs)[,1:7]
    })
    
    output$download_example2 <- downloadHandler(
      filename = function() {
        paste0('clincnv_example_data', '.bed')
      },
      content = function(con) {
        write.table(data$example_cnvs, con, quote = FALSE, sep = "\t", row.names = FALSE)
    })
    
    output$download_example_xlsx2 <- downloadHandler(
      filename = function() {
        paste0('clincnv_example_data', '.xlsx')
      },
      content = function(con) {
        writexl::write_xlsx(data$example_cnvs, con)
    })
    
    
  } #close server



  
  
  
  
  
  
  
  
  
  
