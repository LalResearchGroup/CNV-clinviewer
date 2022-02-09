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

jscode <- "
  shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
  }
  "

setEnrichrSite("Enrichr")
databases <- listEnrichrDbs()

#18BC9C,#2C3E50,#F39C12,#E74C3C,#3498DB,#18BC9C,#2C3E50,#F39C12

ui = fluidPage(
  tags$head(tags$style(HTML("li>a {color: gray;} .nav-tabs>li.active>a {color: gray;}"))),
  useShinyjs(), 
  extendShinyjs(text = jscode , functions = c("collapse")), 
  tags$style(HTML("
              .box.box-solid.box-navy>.box-header {
                color:#fff;
                background:#2C3E50
              }
              .box.box-solid.box-navy{
              border-style: solid;
              border-bottom-color:#2C3E50;
              border-left-color:#2C3E50;
              border-right-color:#2C3E50;
              border-top-color:#2C3E50;
              }")),
  tags$style(HTML("
              .box.box-solid.box-primary{
              border-style: solid;
              border-width: 4px;
              }")),
  tags$head(tags$style("#modal1 .modal-dialog {width: 1100px;}")),
  tags$head(tags$style("#modal2 .modal-dialog {width: 800px;}")),
  navbarPage(theme = shinytheme("flatly"), 
               id = "inTabset",
               selected = "panel1",
               title = p(icon("dna"), "CNV-clinviewer"),
               header = tagList(useShinydashboard()),
               tags$style(
                 type = 'text/css', 
                 '.primary {background-color: #005CB9!important; }'
               ),
               
               ########################### PANEL 1  ########################### 
               
               tabPanel("Home", value = "panel1", #style = "background-color:#F0F8FF;",
                        fluidRow(style = "background-color:#2C3E50;",
                          column(width = 6, offset = 1,
                                 br(), br(), 
                                 p("CNV-clinviewer", 
                                   style='padding:4px; font-size:600%; color:white; font-weight: light'),# font-family:"Lucida" Grande, sans-serif;'),
                                 p("CNV-clinviewer is a user-friendly web-application for the interactive visualization, 
                                   genomic exploration and standardized clinical significance interpretation of large copy number variants (CNVs).", 
                                   style='font-size:200%; color:white; line-height: 40px; font-family:arial; font-weight: lighter'),
                                 br(), 
                                 div(style="display: inline",
                                     actionButton("submit_example", span(icon("play"),"Run example")),
                                     tags$head(tags$style(HTML('#submit_example{background-color:#EC7C25; font-size:150%}'))),
                                     actionButton("go_to_about", "About"),
                                     tags$head(tags$style(HTML('#go_to_about{background-color:#233240; font-size:150%}'))),
                                     actionButton("go_to_contact", "Contact"),
                                     tags$head(tags$style(HTML('#go_to_contact{background-color:#233240; font-size:150%}')))
                                 ),
                                 br(), br(), br(), br()
                                 )), 
                          column(width = 8, offset = 2,
                                 br(),br(),br(),
                                  box(width = 12, solidHeader = TRUE, status = "primary",
                                     title = p(span(icon("upload"), strong("Upload your CNVs (GRCh37/hg19)")), actionButton("upload_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                                     fluidRow(
                                       column(width = 5,
                                              textAreaInput("file_paste", 
                                                           label=p("Copy paste CNV(s)",style="font-size:120%"),
                                                           value=paste("chr1 50533376 65883376 DUP Sample1","chr1 155233376 160983376 DEL Sample2", sep="\n"), 
                                                           width = "400px",
                                                           height = "90px"),
                                              actionButton("submit_data_paste", icon= icon("paper-plane"), "Submit CNV(s)", class="btn-primary"),
                                              awesomeCheckbox("agree_terms", value = TRUE, width = NULL,
                                                              HTML("I agree to the ", 
                                                                   as.character(actionLink(inputId = 'terms_use', label = "Terms of Use.", style='padding:4px;color:#233240;font-weight:bold')))
                                                              )
                                              ),
                                       column(width = 1, 
                                              br(),br(),br(),br(),
                                              p("OR",  style="color:#233240;font-weight:bold;font-size:120%")
                                              ),
                                       column(width = 6,  
                                           fluidRow(
                                             column(width = 8,
                                                    fileInput("file_upload", p("Upload file (.bed/.xlsx; see help)",style="font-size:120%"), multiple = FALSE)
                                                    )),
                                           fluidRow(
                                             column(width = 6,
                                                    actionButton("submit_data_upload", icon= icon("paper-plane"), "Submit file", class="btn-primary"),
                                                    awesomeCheckbox("agree_terms2",value = TRUE, width = NULL,
                                                                    HTML("I agree to the ",
                                                                         as.character(actionLink(inputId = 'terms_use2', label = "Terms of Use.",style='padding:4px;color:#233240;font-weight:bold')))
                                                                    ),
                                                    tags$head(tags$style(HTML('#checkbox :after, #checkbox :before{
                                                                              background-color:#bff442;
                                                                              }')))
                                                  ))
                                       )),
                                     column(width = 12,
                                            div(style = "display:inline-block; float:right", 
                                                p("CNVs not in hg19? Lift over", a("here", href="https://genome.ucsc.edu/cgi-bin/hgLiftOver", target="_blank", style="color:#3c8dbc;font-weight:bold")),
                                            ))
                                ) #close box
                                ), #close column
                        column(width =12,
                               br(),
                               tags$hr(),
                               fluidRow(p("The content on this website is based on version January 27, 2022", align = "center")))
    ), #close tabpanel
    
    ########################### PANEL 2  ########################### 
    
    tabPanel("Analysis", value = "panel2", #style = "background-color:#F0F8FF;",
             br(),
             
             tags$style(".small-box{border-radius: 10px}"),
             valueBoxOutput("box1", width=3),
             valueBoxOutput("box2", width=3),
             valueBoxOutput("box3", width=3),
             valueBoxOutput("box4", width=3),
             
             box(solidHeader = TRUE, status = "primary", width = 12, 
                 title = p("Classification of CNVs", actionButton("interpretation_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                 collapsible = TRUE,
                 collapsed = FALSE,
                 column(width = 12, #offset = 1,
                        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #f5bd92 !important;}')),
                        DT::dataTableOutput("classify_table"),
                        downloadButton("download_classification", label = "Download score sheet", class = "btn-s", color="primary")
                        ),
                 column(width=12, align= "center",
                        br(),
                        div(style='color:#EC7C25; font-weight: bold; font-size:110%; display: inline',
                            icon("info-circle", style="display: inline"),
                            p("Please click on CNV of interest in table above to see specifics below.",  style="display: inline"))
                 )
             ), #close box
             box(solidHeader = TRUE, status = "navy", width = 12, id = "report_box",
                 title = p("Report of selected CNV", actionButton("report_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 column(width = 6, align= "center", offset=3,
                 column(width = 8, align= "center",
                        wellPanel(htmlOutput("selected_cnv"), style= "border-color: #ffffff; background-color: #3c8dbc; border-radius: 10px; color: white; font-size: 110%"),
                        # tags$head(tags$style("#selected_cnv{color: black;
                        #          font-size: 20px;
                        #          }"
                        # ))
                        ),
                 column(width = 4, align= "center",
                        # actionButton("view_report", "View report",  icon = icon("eye"),  class = "btn-l", color = "primary"),
                        # bsModal("modal_report", "", "view_report", size = "large",
                        #         htmlOutput("document")
                        # ),
                        br(),br(),br(),
                        downloadButton("download_report", "Download html report",  icon = icon("download"), class = "btn-l", color = "primary"),
                        tags$head(tags$style(HTML('#download_report{background-color:#EC7C25}')))
                        #,
                        #br(),br(),
                        #downloadButton("download_report_pdf", "Download pdf report",  icon = icon("download"), class = "btn-l", color = "primary")
                        ))
                 ),#close box
             box(solidHeader = TRUE, status = "navy", width = 12, id = "viewer_box",
                 title = p("Viewer", actionButton("viewer_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                 collapsible = TRUE,
                 collapsed = TRUE,
                     column(width=12,
                              div(style = "display:inline-block; float:right",  
                                  actionLink("moveleft_more", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                  actionLink("moveleft", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                  actionLink("moveright", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                  actionLink("moveright_more", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                  actionLink("zoom_in", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                  actionLink("zoom_out", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                  actionLink("zoom_back", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                            )),
                     column(width = 12,
                            textOutput("selected_region_ideogram"),
                            tags$head(tags$style("#selected_region_ideogram{font-size: 16px;font-weight: bold; color: #3c8dbc}")),
                            withSpinner(plotOutput("ideogram", height = 100, brush = brushOpts(id = "ideogram_brush", resetOnNew = TRUE)))
                            ),
                 #box(width=12,
                     column(width = 12,
                            br(),
                            fluidRow(
                              box(title = textOutput("number_genes"),
                                  width = 12, solidHeader = F, #status = "primary", #status = "navy", 
                                  collapsible=FALSE,
                              tags$head(tags$style("#number_genes{font-size: 18px;font-weight: bold}")),
                              wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%", 
                                        column(width=12,
                                               div(style = "display:inline-block; float:right",  
                                                   actionLink("moveleft_more2", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                   actionLink("moveleft2", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                   actionLink("moveright2", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                   actionLink("moveright_more2", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                   actionLink("zoom_in2", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                   actionLink("zoom_out2", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                   actionLink("zoom_back2", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                               )),
                                        br(),
                                        withSpinner(plotlyOutput("gene_plotly", height = '100%')), #, height = "auto")), #height = textOutput("gene_rows"))),#"300px")),
                                        fluidRow(
                                        column(width=4, offset=8,  align = "right",
                                               plotOutput("gene_legend", height = 50)),
                                        column(width=12,
                                               column(width=2,
                                              selectInput("gene_coloring", 
                                                          p("Gene coloring:",style = "color:grey", actionLink("viewer_help_color", "", icon = icon("question"), class = "btn-xs", title = "Help", style = "color:grey")),
                                                          c("All scores combined" = "scores_combined",
                                                            "LOEUF" = "loeuf",
                                                            "pLI" = "pLI",
                                                            "pHI" = "pHI",
                                                            "pTS" = "pTS",
                                                            "%HI" = "%HI"),
                                                          #width = '15%',
                                                          selected = "scores_combined"
                                                   ))))
                                        )), #close well panel and box
                              box(title =  textOutput("number_cnvs"),
                                  width = 12, solidHeader = F, #status = "primary", #status = "navy", 
                                  collapsible=FALSE,
                                 tags$head(tags$style("#number_cnvs{font-size: 18px;font-weight: bold}")),
                                   wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%", 
                                             column(width=12,
                                                    div(style = "display:inline-block; float:right",  
                                                        actionLink("moveleft_more3", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                        actionLink("moveleft3", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                        actionLink("moveright3", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                        actionLink("moveright_more3", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                        actionLink("zoom_in3", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                        actionLink("zoom_out3", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                        actionLink("zoom_back3", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                                    )),
                                             br(),
                                          withSpinner(plotlyOutput("upload_cnv_plotly", height = "100%")),
                                          fluidRow(
                                            column(width=2, offset=10,  align = "right",plotOutput("cnv_legend", height = 50)),
                                            column(width=12, uiOutput("upload_cnv_filter_ui"))
                                            )
                                        )), #close well panel and box
                              #br(),
                              #hr(),
                              #strong("CNVs in ClinVar", style = "font-size: 18px"),
                              #wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%",
                                        
                              box(title = strong("CNVs in ClinVar", style = "font-size: 18px"),
                                  width = 12, solidHeader = T, status = "primary", #status = "navy", 
                                  collapsed = FALSE, collapsible=TRUE,
                                  wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%",
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Summary of pathogenic/ likely pathogenic CNVs",
                                                             column(width=12,
                                                                    div(style = "display:inline-block; float:right",  
                                                                        actionLink("moveleft_more4", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveleft4", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveright4", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveright_more4", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("zoom_in4", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                                        actionLink("zoom_out4", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                                        actionLink("zoom_back4", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                                                    )),
                                                             br(),
                                                             withSpinner(plotlyOutput("clinvar_plot_summary", height= 250)),
                                                             column(width=2, offset=10,  align = "right",plotOutput("cnv_legend2", height = 50)),
                                                             p("ClinVar version: 16 January 2022")
                                                             ),
                                                    tabPanel("CNV plot and table",
                                                             br(),
                                                             column(width=12,
                                                                    div(style = "display:inline-block; float:right",  
                                                                        actionLink("moveleft_more5", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveleft5", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveright5", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("moveright_more5", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                                        actionLink("zoom_in5", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                                        actionLink("zoom_out5", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                                        actionLink("zoom_back5", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                                                    )),
                                                             br(),
                                                             htmlOutput("clinvar_cnv_number"),
                                                             br(),
                                                             textOutput("clinvar_cnv_number_1"),
                                                             withSpinner(plotlyOutput("clinvar_plot1", height= "100%")),
                                                             textOutput("clinvar_cnv_number_2"),
                                                             withSpinner(plotlyOutput("clinvar_plot2", height= "100%")),
                                                             textOutput("clinvar_cnv_number_3"),
                                                             withSpinner(plotlyOutput("clinvar_plot3", height= "100%")),
                                                             column(width=2, offset=10,  align = "right",plotOutput("cnv_legend3", height = 50)),
                                                             br(),
                                                             uiOutput("clinvar_filter_ui", height= "100%"),
                                                             br(),
                                                             #hr(),
                                                             withSpinner(dataTableOutput("clinvar_table", height= "100%")),
                                                             p("ClinVar version: 16 January 2022"),
                                                    ))
                                        )), #close well panel and box
                              #hr(),
                              #strong("CNV allele frequency from 482,734 individuals in the UK Biobank (filtered for CNVs > 50kb)", style = "font-size: 18px"),
                              #wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%",
                                        
                              box(title = strong("CNV allele frequency from 482,734 individuals in the UK Biobank (filtered for CNVs > 50kb)", style = "font-size: 18px"),
                                  width = 12, solidHeader = T, status = "primary", #status = "navy", 
                                  collapsed = FALSE, collapsible=TRUE,
                                  wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%",
                                            column(width=12,
                                                   div(style = "display:inline-block; float:right",  
                                                       actionLink("moveleft_more6", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveleft6", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveright6", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveright_more6", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("zoom_in6", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                       actionLink("zoom_out6", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                       actionLink("zoom_back6", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                                   )),
                                            br(),
                                        withSpinner(plotlyOutput("ukb_plot_summary", height= 250)),
                                        column(width=2, offset=10,  align = "right",plotOutput("cnv_legend4", height = 50)),
                                        br(),br()
                                        )), #close well panel and box
                              #hr(),
                              #strong("CNV allele frequency from 14,891 genomes (gnomAD; filtered for CNVs > 50kb)", style = "font-size: 18px"),
                              box(title = strong("CNV allele frequency from 14,891 genomes (gnomAD; filtered for CNVs > 50kb)", style = "font-size: 18px"),
                                  width = 12, solidHeader = T, status = "primary", #status = "navy", 
                                  collapsed = FALSE, collapsible=TRUE,
                                  
                                  wellPanel(style= "border-color: #aaaaaa; background-color: #ffffff; border-radius: 5px; font-size: 110%",
                                            column(width=12,
                                                   div(style = "display:inline-block; float:right",  
                                                       actionLink("moveleft_more7", "", icon = icon("angle-double-left"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveleft7", "", icon = icon("angle-left"), "50%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveright7", "", icon = icon("angle-right"),"50%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("moveright_more7", "", icon = icon("angle-double-right"), "95%", style='padding:4px; font-size:90%; color:black'),
                                                       actionLink("zoom_in7", "Zoom in", icon = icon("search-plus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                       actionLink("zoom_out7", "Zoom out", icon = icon("search-minus"), "2x", style='padding:4px; font-size:100%; color:black'),
                                                       actionLink("zoom_back7", "Back to CNV", icon = icon("undo-alt"), style='padding:4px; font-size:100%; color:black')
                                                   )),
                                            br(),
                                        withSpinner(plotlyOutput("gnomad_plot_summary", height= 250)),
                                        column(width=2, offset=10,  align = "right",plotOutput("cnv_legend5", height = 50)),
                                        br(), br()
                                        ))  #close well panel and box
                            ))
                # )#close box
             ),#close box
             box(solidHeader = TRUE, status = "navy", width = 12, id = "gene_box",
                 title = p("Intersecting genes, regions and associated disease"),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 tabsetPanel(type = "tabs",
                             tabPanel(p("Gene table",actionButton("gene_table_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                                      column(width = 12,
                                             br(),
                                             DT::dataTableOutput("gene_table"),
                                             br(),
                                             downloadButton("download_genetable", label = "Download gene table", class = "btn-s")
                                      )),
                             tabPanel(p("ClinGen gene disease table", actionButton("gene_disease_table_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                                      column(width = 12,
                                             br(),
                                             DT::dataTableOutput("clingen_table"), 
                                             br())
                             ),
                             tabPanel(p("ClinGen region table", actionButton("clingen_region_table_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                                      column(width = 12,
                                             br(),
                                             DT::dataTableOutput("clingen_region_table"),
                                             br(), br())
                             ),
                             tabPanel(p("DECIPHER CNV syndromes", actionButton("decipher_syndromes_table_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                                      column(width = 12,
                                             br(),
                                             DT::dataTableOutput("decipher_syndromes"),
                                             br(), br())
                             )
                 )),
             box(solidHeader = TRUE, status = "navy", width = 12, 
                 title = p("Gene set enrichment analysis", actionButton("gsea_help", "", icon = icon("question"), class = "btn-xs", title = "Help")),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 column(width = 12,
                                 column(width = 6,
                                        wellPanel(
                                          selectInput("enrichr_db1", "Select library:",
                                                      databases$libraryName, 
                                                      selected = databases$libraryName[22],
                                                      width = '60%'),
                                          bsPopover(id = "enrichr_db1",
                                                    title= "Libraries",
                                                    content= paste(p("Information on libraries"), a("here", href="https://maayanlab.cloud/Enrichr/#libraries", target="_blank")), 
                                                    placement= "right",
                                                    trigger="focus",
                                                    options = list(container = "body")), 
                                          withSpinner(plotlyOutput("enrichR_output_1")),
                                          DT::dataTableOutput("enrichR_table_1"))),
                                 column(width = 6,
                                        wellPanel(
                                          selectInput("enrichr_db2", "Select library:",
                                                      databases$libraryName, 
                                                      selected = databases$libraryName[142],
                                                      width = '60%'),
                                          bsPopover(id = "enrichr_db2",
                                                    title= "Libraries",
                                                    content= paste(p("Information on libraries"), a("here", href="https://maayanlab.cloud/Enrichr/#libraries", target="_blank")), 
                                                    placement= "right",
                                                    trigger="focus",
                                                    options = list(container = "body")), 
                                          withSpinner(plotlyOutput("enrichR_output_2")),
                                          DT::dataTableOutput("enrichR_table_2")))
                        )
             )
          ), #close tabpanel
    
    ########################### PANEL 3  ########################### 
             
    #tabPanel("Tutorial", value = "panel3"), #style = "background-color:#F0F8FF;"),
    
    ########################### PANEL 4  ########################### 
             
    tabPanel("About", value = "panel4", #style = "background-color:#F0F8FF;",
             
            column(width=10, offset=1,
                   
             box(solidHeader = TRUE, status = "primary", width=12,
                 title = span(icon("info"), "About"),
                 collapsible = TRUE,
                 collapsed = FALSE,
                 tabsetPanel(
                 tabPanel("Abstract",
                          br(),
                          HTML(paste0(
                          "<b>Purpose</b>","<br>",
                          "To develop a user-friendly web-application for the visualization, genomic exploration and standardized clinical significance interpretation of large copy number variants (CNVs).",
                          "<br>","<br>",
                          "<b>Methods</b>","<br>",
                          "We aggregated CNV data of >480,000 individuals from the UK Biobank and gnomAD as well as >10,000 patient CNVs from ClinVar.
                          In addition, we identified and integrated ten genomic annotations such as gene dosage sensitivity scores and two CNV bioinformatic tools (ClassifyCNV, enrichr). 
                          All data and tools were integrated into a novel R Shiny based interface and was deployed in the google cloud as web-application",
                          "<br>","<br>",
                          "<b>Results</b>","<br>",
                          "We present the CNV-clinviewer, an interactive visualization and interpretation tool that enables intuitive real-time exploration of CNV data online. 
                          After upload of CNV data CNV-clinviewer enables","<br>",
                          "a) automated and manual CNV classification based on the 2019 ACMG/ClinGen Technical Standards for CNVs, ","<br>",
                          "b) generation of comprehensive reports including CNV classification details, and overlap with dosage-sensitive and clinically relevant genes and genomic region,","<br>",
                          "c) visualization and dynamic filtering of uploaded CNVs and their exploration in context to CNVs and known disease from publicly available databases,","<br>",
                          "d) evaluation of the gene content by various gene dosage sensitivity scores and clinical annotations,","<br>",
                          "e) gene set enrichment analyses to infer knowledge about genes from a selected genomic region.",
                          "<br>","<br>",
                          "<b>Conclusion</b>","<br>",
                          "We developed the CNV-clinviewer, a web-application that facilitates consistent and transparent evaluation of CNVs. 
                          The application is a user-friendly and publicly available at www.cnv-clinviewer.broadinstitute.org.",
                          sep = "<br>")
                 )),
                 tabPanel("Team",
                          br(),
                          p("- coming soon -")
                 ))
             ), 
             box(solidHeader = TRUE, status = "navy", width=12,
                 title = span(icon("hand-point-right"), "Step 1: Data upload"),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 br(),
                 HTML(paste(
                   "<b>You can upload CNVs in a bed or tab-delimited text file, or in an excel file with the following columns:</b>", "<br>",
                   "<br>",
                   "<b>Required columns:</b>", "<br>",
                   "1. CHR: chromosome, for example 'chr1'", "<br>",
                   "2. START: genomic start coordiante of CNV (hg19)", "<br>",
                   "3. END: genomic end coordiante of CNV (hg19)","<br>",
                   "4. TYPE: type of CNV ('DEL' or 'DUP')","<br>",
                   "<br>",
                   "<b>Optional columns:</b>", "<br>",
                   "5. ID: sample ID/ identifier (CNVs with the same sample ID will be visualized in the same row; if you do not provide sample IDs each CNV will be given a unique ID)", "<br>",
                   "6. POINTS: The CNVs are automatically classified based on the",
                   paste0("<a href='", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7313390/", "' target='_blank'>", "2019 ACMG/ClinGen Technical Standards for CNVs.","</a>"),
                   "Evaluated evidence categories for copy number losses are: 1A/B, 2A-H, 3A-C, 4O, and for copy number gains: 1A/B, 2A-H, 2J-L, 3A-C, 4O.
                   If you have further information e.g. about the family history or the 'de novo' status you can give the total score of the non-evaluated evidence categories in the 'POINTS' column as numeric values (eg. -1 or 0.9).", "<br>",
                   "7. FILTER_'name' : additional binary variables (with values 1 (='yes') and 0 (='no')) for filtering. The name of the variable should be provided after 'FILTER_'.", "<br>","<br>"
                 )),
                 hr(),
                 div(tableOutput("example_cnvs2"), style = "font-size:80%", align= "center"),
                 hr(),
                 downloadLink('download_example2', 'Download example (.bed)'),
                 br(),
                 downloadLink('download_example_xlsx2', 'Download example (.xlsx)'),
                 br()
             ), 
             box(solidHeader = TRUE, status = "navy", width=12,
                 title = span(icon("hand-point-right"), "Step 2: Classification"), 
                 collapsible = TRUE,
                 collapsed = TRUE, 
                 br(),
                 HTML(paste0("After submitting your CNVs you will get directed to the Analysis page. On top of the page you will find your CNVs in a table with annotated classification. The uploaded CNVs are classified based on the",
                             paste0("<a href='", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7313390/", "' target='_blank'>", " 2019 ACMG/ClinGen Technical Standards for CNVs ","</a>"),
                             "using the",
                             paste0("<a href='", "https://www.nature.com/articles/s41598-020-76425-3", "' target='_blank'>", " ClassifyCNV ","</a>"),"tool. ",
                            
                             "The final classification results from the score from ClassifyCNV and the additional score from manually evaluated evidence categories by the user (if given as column in the input file).","<br>",
                             
                             "A description of the scoring algorithm can be found", 
                             paste0("<a href='", "https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-020-76425-3/MediaObjects/41598_2020_76425_MOESM1_ESM.pdf", "' target='_blank'>", " here ","</a>"),
                             "and details about the automatically evaluated evidence categories can be found in the help section of the classification track on the Analysis page. ","<br>",
                             
                              "Scoring: Pathogenic: 0.99 or more points, Likely Pathogenic: 0.90 to 0.98 points, Variant of Uncertain Significance: 0.89 to −0.89 points, Likely Benign: −0.90 to −0.98 points, Benign: −0.99 or fewer points.",
                            
                             "<br>","<br>",
          
                             "Automatically evaluated evidence categories by ClassifyCNV:", "<br>",
                             "- for copy number losses: 1A/B, 2A-H, 3A-C, 4O","<br>",
                             "- for copy number gains: 1A/B, 2A-H, 2J-L, 3A-C, 4O",
          
                             "<br>","<br>"))
             ), 
             box(solidHeader = TRUE, status = "navy", width=12,
                 title = span(icon("hand-point-right"), "Step 3: Report of selected CNV of interest"),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 br(),
                 p("After selecting a CNV of interest in the table above a report in html format can be downloaded. 
                   The report contains summary information of the region, the classification of the CNV 
                   as well as the specific scores given in the evaluated evidence categories from the 2019 ACMG/ClinGen Technical Standards for CNVs."),
                 br()
             ), 
             box(solidHeader = TRUE, status = "navy", width=12,
                 title = span(icon("hand-point-right"), "Step 4: Exploration and visualization of uploaded CNVs and genomic region"), 
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 tabsetPanel(type = "tabs",
                             tabPanel("Viewer",
                                      br(),
                                      HTML(paste("<b>Ideogram</b>",
                                                 "The ideogram shows the whole chromosome of the selected CNV with its chromosome bands.",
                                                 "To change the selected region you can select a region by click-and-drag on the plot.",
                                                 "<hr>",
                                                 "<b>Gene track</b>",
                                                 "In the gene track all protein-coding genes are shown. When hovering over the genes you get more information about the gene.
                                                  Genes that are dosage-sensitive, i.e. genes that are likely to cause a phenotypic effect, are shown in orange.",
                                                 "There are several dosage-sensitivity scores to choose from: loeuf, pLI, pHI, pTS, %HI, HI/TS Score ClinGen.",
                                                 sep = "<br>")),
                                      br(),br(),
                                      div(tableOutput("gene_scores"), style = "font-size:80%", align= "center"),
                                      HTML(paste("<hr>",
                                                 "<b>Uploaded CNVs</b>",
                                                 "In the 'Uploaded CNV' track all uploaded CNVs that intersect the selected region are shown. Deletions are visualized with red bars and duplications with 
                                                  blue bars while each row represents one sample/ID. 
                                                  When hovering over the start or end of the CNV a tooltip with information about the CNV is shown.",
                                                  "The uploaded CNVs can be filtered/ selected based on the Classification and their ID. 
                                                  In case binary phenotype variables were uploaded the CNVs can also be filtered based on those.",
                                                 "To change the selected region you can select a region by click-and-drag on the plot or use use the move/ zoom-in/ zoom-out buttons.",
                                                 "<hr>",
                                                 "<b>ClinVar CNVs</b>",
                                                 "ClinVar summary tab:",
                                                 paste("In the ClinVar summary track the number of interescting pathogenic/ likely pathogenic deletions and duplications from", 
                                                       tags$a(href="https://www.ncbi.nlm.nih.gov/clinvar/", "ClinVar", target="_blank"),
                                                       "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                                                 "When hovering over the plot the number of deletions and duplications are shown.",
                                                 "To change the selected region you can select a region by click-and-drag on the plot or use use the move/ zoom-in/ zoom-out buttons.",
                                                 "",
                                                 "ClinVar plot and table tab:",
                                                 paste("In the linVar plot and table tab track the individual CNVs from", 
                                                       tags$a(href="https://www.ncbi.nlm.nih.gov/clinvar/", "ClinVar", target="_blank"),
                                                       "are shown in a plot and in a table."),
                                                 "The plot is splitted in three subpolots, one visualizes the pathogenic/likely pathogenic CNVs, one visualizes CNVs of uncertain significance 
                                                 and one plot visualizes benign/likely benign CNVs. More information about the CNVs can be found in the table or by hovering over the CNVs in the plot.
                                                 The CNVs can be filtered by their clinical significance and type.",
                                                 "<hr>",
                                                 "<b>UK Biobank CNVs</b>",
                                                 paste("In 'UK Biobank' track the combined allele frequency of interescting deletions and duplications from", 
                                                       tags$a(href="https://doi.org/10.1016/j.ajhg.2019.07.001", "the UK Biobank", target="_blank"),
                                                       "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                                                 "To remove small CNVs/ structural variants, all CNVs < 50kb were removed prior to the calculation.",
                                                 "When hovering over the plot the allele frequency of deletions and duplications are shown.",
                                                 "To change the selected region you can select a region by click-and-drag on the plot or use the move/ zoom-in/ zoom-out buttons.",
                                                 "<hr>",
                                                 "<b>GnomAD CNVs</b>",
                                                 paste("In 'GnomAD' track the combined allele frequency of interescting deletions and duplications from", 
                                                       tags$a(href="https://doi.org/10.1016/j.ajhg.2019.07.001", "GnomAD", target="_blank"),
                                                       "in 200 kb regions are shown. Numbers were calculated every 100 kb for the region 100 kb up- and downstream."),
                                                 "To remove small CNVs/ structural variants, all CNVs < 50kb were removed prior to the calculation.",
                                                 "When hovering over the plot the allele frequency of deletions and duplications are shown.",
                                                 "To change the selected region you can select a region by click-and-drag on the plot or use use the move/ zoom-in/ zoom-out buttons.",
                                                 sep = "<br>")),
                                      br()
                             ),
                             tabPanel("Tables",
                                      br(),
                                      HTML(paste("<b>Gene table</b>",
                                                 "The gene table contains all genes, their genomic coordinates, OMIM links, and gene dosage sensitivity scores.", sep = "<br>")),
                                      hr(),
                                      HTML(paste("<b>Clingen gene disease table</b>",
                                                 "The ClinGen Gene Curation working group has developed a framework to standardize the approach to determine the clinical validity for a gene-disease pair.",
                                                 "The ClinGen Gene-Disease Clinical Validity curation process involves evaluating the strength of evidence supporting or refuting a claim that variation in a particular gene causes a particular disease.",
                                                 "Classifications derived with this framework are reviewed and confirmed or adjusted based on clinical expertise of appropriate disease experts.",
                                                 "Possible classifications are: Definitive > Strong > Moderate > Limited > No known Disease Relationship > Disputed Evidence > Refuted Evidence.",
                                                 paste("More information can be found", tags$a(href="https://clinicalgenome.org/curation-activities/gene-disease-validity/", "here", target="_blank"), "or",
                                                       tags$a(href="https://pubmed.ncbi.nlm.nih.gov/28552198/", "here.", target="_blank")),
                                                 sep = "<br>")),
                                      hr(),
                                      HTML(paste("<b>Clingen regions table</b>",
                                                 "The ClinGen Dosage Sensitivity curation process collects evidence supporting/refuting the haploinsufficiency and triplosensitivity of genomic regions.",
                                                 "Classifications derived with this framework are reviewed and confirmed or adjusted based on clinical expertise of appropriate disease experts.",
                                                 "Possible scores for the Haploinsuficiency score (HI Score) and Triplosensitivity score (TS Score) are: 0 (No Evidence), 1 (Little Evidence), 2 (Emerging Evidence), 3 (Sufficient Evidence), 40 (Dosage Sensitivity Unlikely)",
                                                 paste("More information can be found", tags$a(href="https://www.clinicalgenome.org/curation-activities/dosage-sensitivity/", "here.", target="_blank")),
                                                 sep = "<br>")),
                                      hr(),
                                      HTML(paste("<b>CNV syndromes</b>",
                                                 "The table shows expert-curated microdeletion and microduplication syndromes involved in developmental disorders from DECIPHER that intersect the selected region.",
                                                 paste("All syndromes and more information can be found at", tags$a(href="https://www.deciphergenomics.org/disorders/syndromes/list", "DECIPHER.", target="_blank")),
                                                 sep = "<br>")),
                                      hr()
                                      
                             ),
                             tabPanel("Gene set enrichment analysis",
                                      br(),
                                      HTML(paste("<b>Enrichment analysis</b>",
                                                "Gene set enrichment analysis is a computational method for inferring knowledge about an input gene set by comparing it to annotated gene sets representing prior biological knowledge.",
                                                 "Here, all genes from the selected region are used as input to analyze whether the genes significantly overlap with an annotated gene set from the libraries.
                                                  Libraries can be changed using the dropdown menu.", 
                                                 paste("More information about enrichment analyses can be found", tags$a(href="https://pubmed.ncbi.nlm.nih.gov/19033363/", "here", target="_blank"),
                                                       "and descriptions of the available libraries can be found", tags$a(href="https://maayanlab.cloud/Enrichr/#libraries", "here.", target="_blank")),
                                                 sep = "<br>")),
                                      br(), br()
                             )
                 )
             ), 
             box(solidHeader = TRUE, status = "primary", width=12,
                 title = span(icon("database"), "Data"),
                 collapsible = TRUE,
                 collapsed = FALSE,
                 div(tableOutput("data_sources"), style = "font-size:80%", align= "center")
                 #tableOutput("datatable_about")
                 #p("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc,")
             )),
            column(width=12,
             hr(),
             fluidRow(align = "center",
             p(HTML("<b>Impressum</b>")),
              p("We object to any commercial use and disclosure of data. 
                Copyright and use: The authors grants you the right of use to make a private copy for personal purposes. 
                However, you are not entitled to change and/or pass on the materials or publish them yourself. 
                Upon request, you will receive free information about the personal data stored about you. 
                To do so, please contact the administrator. 
                No liability: The contents of this web project have been carefully checked and created to the best of our knowledge. 
                But for the information presented here is no claim to completeness, timeliness, quality and accuracy. 
                No responsibility can be accepted for any damage caused by reliance on or use of the contents of this website.")
             ))
            ), #close tabpanel

) #close navbar page
) #close fluidpage










