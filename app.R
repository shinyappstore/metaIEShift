#
# Shiny app for examining all models & outcomes
#

#library(shiny)
library(plotly)
library(tidyverse)
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(readr)
library(RColorBrewer)
library(shinyWidgets)
library(shinydashboard)
library(metafor)
library(Hmisc)
library(DT)

data <- read_csv("5. Kurdi et al. (2022) Meta-Analytic Database.csv")

data <- data %>%
  mutate(impMeasureAgg = 
           case_when(
             impMeasure %in% c("Brief IAT", "Go/No-Go Association Task",
                               "Implicit Association Test", "Personalized IAT",
                               "Single-Category IAT" ,
                               "Implicit Relational Assessment Procedure",
                               "Sorting Paired Features Task") ~ "IAT Family",
             impMeasure %in% c("Affect Misattribution Procedure",
                               "Semantic Misattribution Procedure") ~ "AMP Family",
             impMeasure %in% c("Evaluative Priming Task", "Sequential priming") ~ "EPT Family",
             impMeasure %in% c("Savings in relearning", 
                              "Speeded self-report") ~ "Other"
           ))

publish_choices <- c("All", "Published", "Unpublished")
modVar <- c("Publication Status (e.g., Published)", 
           "Sample Type (e.g., Students)", "Domain (e.g., Novel individuals)")
modNames <- c("pubStat", "studySample", "domain")

modStructure <- c("Effect of Moderator","Effect of Moderator + Measure Type", "Moderator * Measure Type")
### Clean for valence analyses ####

### Start building app ####

# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Helvetica"};'))),
   # Application title
   titlePanel("How do implicit and explicit evaluations shift?"),
   h4("A preregistered meta-analysis of the effects of co-occurrence and relational information"),
  #div(p("For the best experience with this app, please use full screen."), style = "font-size: 80%;"),
   br(),
   fluidRow(
     column(3, style = "background-color:#F7F6F4;padding-left:20px",
            h4('What would you like to explore?'),
            div(p("Specify the inclusion criteria using the inputs below."), style = "font-size: 90%;"),
            
       pickerInput("compare",
                   "Choose comparison:",
                   choices = sort(rownames(table(data$comparison))),
                   selected = c("AB", "AC", "DB", "DC") , #inline = T, 
                   multiple = T,
                   options = list(
                     `actions-box` = TRUE )
       ),
       tags$div(align = 'left',
                prettyCheckboxGroup("iMeasure",
                          "Choose implicit measure:",
                          choices = sort(rownames(table(data$impMeasureAgg))),
                          status = "info", shape = "round",
                          icon = icon("check"),
                          selected = c("AMP Family", "IAT Family", "EPT Family", "Other") , inline = T,  
       )),
       pickerInput("typeRel",
                   "Choose type of relational information:",
                   choices = sort(rownames(table(data$typeRelational))),
                   selected = c("Causal information", "Validity information", 
                                "Co-occurrence information","Diagnosticity information",
                                "Information on logical relations", "Social role information",
                                "Narrative information",
                                "Spatiotemporal information",
                                "Social role information") , #inline = T, 
                   multiple = T,
                   options = list(
                     `actions-box` = TRUE)
       ),
       pickerInput("typeCo",
                   "Choose type of co-occurence information:",
                   choices = sort(rownames(table(data$typeCoOcc))),
                   selected = c("Statements", "Sounds", "Statements", 
                                "Images", "Narratives","Odors", "Shock",
                                "Wins/losses", "Words"), #inline = T,
                   multiple = T,
                   options = list(
                     `actions-box` = TRUE)),
       div(p("Want to customize further? See 'Additional Specifications' below."), style = "font-size: 90%;"),
       hr()
     ),

       column(8,
       tabsetPanel(
         tabPanel("Visualize Data", 
                  br(),
                  br(),
                  tags$div(align = 'center',
                           h4("Effect Size Distributions by Dependent Measure")),
                  tags$div(align = 'center',
                           h5("Use the inputs on the left to change the plot")),
                  plotlyOutput("plotImplicit", height = '350px'),
                  br(),
                  br()
                  ),
         tabPanel("Explore Meta-Analytic Database",

                  br(),
                  p("Insert information about the interactive meta-analytic database"),
                  hr(),
                  div(DT::dataTableOutput("table"),
                      style = "font-size:80%"),
                  br()),
         tabPanel("Moderator Analyses",
                  column(8, 
                         h5("Explore the model output below (scroll to the right to see full output):"),
                         div(style="width:95%;padding-left:10px;",fluidRow(verbatimTextOutput("modImp", placeholder = FALSE)))),
                  column(4, style =  "background-color:#F3F8FF;", ##E5FFFD ## CCEEF9
                         h4("Moderator analyses"),
                         br(),
                         pickerInput("modVar",
                                     "Choose a moderator variable*:",
                                     choices = colnames(data)[c(8:12, 14:22)],
                                     multiple = F),
                         div(p("*Levels have not been recoded."), style = "font-size: 80%;"),
                         pickerInput("modStr",
                                     "Specify model:",
                                     choices = modStructure,
                                     multiple = F),
                         div(p("By default, your inputs (left, bottom) will be used to filter the dataset. You can change that below."), style = "font-size: 80%;"),
                         prettySwitch("modData",
                                      "Use Full Dataset", slim = TRUE,
                                      bigger = FALSE,
                                      status = "info")
                         )),
                         
         tabPanel("More Information",
                  br(),
                  #div(p("The information below will be updated to reflect the final version of the manuscript."), style = "font-size: 90%;"),
                  h4("Paper Citation:"),
                  p("[TO BE UPDATED]"),
                  h4("OSF Site:"),
                  tags$a(href="https://osf.io/42re5/?view_only=d58c23131f994584937d573a84858302", "https://osf.io/42re5/"),
                  h4("Abstract:"),
                  p("Based on 660 effect sizes obtained from 23,255 adult participants across 51 reports of experi-
mental studies, this meta-analysis investigates whether and when explicit (self-reported) and im-
plicit (indirectly revealed) evaluations reflect relational information (how stimuli are related to

each other) over and above co-occurrence information (the fact that stimuli have been paired
with each other). Using a mixed-effects meta-regression, relational information was found to
dominate over contradictory co-occurrence information in shifting both explicit (mean Hedges’ g
= 0.84, 95-percent CI: [0.73; 0.94], 95-percent PI: [0.11; 1.57]) and implicit evaluations (g =

0.35, 95-percent CI: [0.22; 0.48], 95-percent PI: [-0.39; 1.08]). However, considerable heterogeneity in relational effects on implicit evaluation made moderator analyses necessary. Implicit

evaluations were particularly sensitive to relational information (a) in between-participant (rather
than within-participant) designs; when (b) co-occurrence information was held constant (rather

than manipulated); (c) targets were novel (rather than known); implicit evaluations were meas-
ured (d) first (rather than last) and (e) using an AMP (rather than an IAT or EPT); and (f) rela-
tional and co-occurrence information were presented in temporal proximity (rather than far apart

in time). Overall, the present findings suggest that both implicit and explicit evaluations emerge

from a combination of co-occurrence information and relational information, with relational in-
formation usually playing the dominant role. Critically, variability in these effects highlights a

need to refocus attention from existence proof demonstrations toward theoretical and empirical

work on the determinants and boundary conditions of the influences of co-occurrence and rela-
tional information on explicit and implicit evaluation."),
                  hr()
                  )
       )),
     column(12, style = "background-color:#DBF3FA;"),
     column(4, style = "height:450px;background-color:#F5FCFF;padding-left:20px", ##  COLUMN 1
            br(),
            h4("Additional Specifications:"),
            selectInput("clust",
                        "Publication Status:",
                        choices = publish_choices,
                        selected = "All"),
            sliderInput("sampleSize", "Minimum Sample Size:",
                        min = 0, max = 600, ticks = FALSE,
                        value = 9),
            prettyCheckboxGroup("design",
                                "Within- or Between-Subjects:",
                                choices = sort(rownames(table(data$withinBetween))),
                                status = "info", shape = "round",
                                icon = icon("check"),
                                selected = c("Within", "Between"), inline = T),
            prettyCheckboxGroup("measureOrder",
                                "Measure Order:",
                                choices = sort(rownames(table(data$expImpOrder))),
                                status = "info", shape = "round",
                                icon = icon("check"),
                                selected = c("Counterbalanced", "Explicit first",
                                             "Implicit first", "One measure only"), inline = T),
            checkboxGroupButtons(
              inputId = "sampleType", label = "Sample Type:",
              choices =sort(rownames(table(data$studySample))), size = "sm",
              justified = FALSE, status = "primary", individual = TRUE,
              selected = c("Nonstudents", "Students"),
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            br()

     ),
     column(4, style =  "height:450px;background-color:#F5FCFF;", ##  COLUMN 2
            br(),
            br(),
            prettyCheckboxGroup("eval_input",
                                "Measured Construct:",
                                status = "info", shape = "round",
                                icon = icon("check"),
                                choices = sort(rownames(table(data$eval))),
                                selected = c("Evaluative", "Semantic"), inline = T),
            prettyCheckboxGroup("domain_input",
                                "Domain:",
                                choices = sort(rownames(table(data$domain))),
                                status = "info", shape = "round",
                                icon = icon("check"),
                                selected = c("Known concepts", "Novel individuals", "Other", "Known individuals", 
                                             "Known products", "Known social groups", "Novel shapes",
                                             "Novel social groups", "Novel nonwords", "Novel products"), inline = T),
            prettyCheckboxGroup("depend_input",
                               "Learning dependency:",
                               status = "info", shape = "round",
                               icon = icon("check"),
                               choices = sort(rownames(table(data$depend))),
                               selected = c("Relational creates independent learning", "Relational modifies co-occurrence"), inline = T),
            prettyCheckboxGroup("sourceRel",
                                "Source of Relational Information:",
                                status = "info", shape = "round",
                                icon = icon("check"),
                                choices = sort(rownames(table(data$sourceRelational))),
                                selected = c("Language", "Language + Observation", "Observation"), inline = T),
            br(),
            br()
     ),
     column(4, style =  "height:450px;background-color:#F5FCFF;",##  COLUMN 2
            br(),
            br(),
            checkboxGroupButtons(
              inputId = "supra", label = "Co-occurrence presented...",
              choiceValues =sort(rownames(table(data$supralim))), size = "sm",
              choiceNames = c("Sublimally", "Supralimally"),
              justified = FALSE, status = "primary", individual = TRUE,
              selected = c(1, 0),
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            sliderInput("numCo", "Minimum Number of Co-Occurrences:",
                        min = 10, max = 200, ticks = FALSE,
                        value = 10),
            pickerInput(
              inputId = "orderRel", label = "Order of Relational & Co-Occurrence information:", 
              choices =sort(rownames(table(data$coOccRelOrder))),
              selected = c("Co-occurrence first, relational immediately after",
                           "Co-occurrence first, relational second",
                           "Counterbalanced",
                           "Relational first, co-occurrence immediately after",
                           "Relational first, co-occurrence second",
                           "Simultaneous"),
              multiple = T,
              options = list(
                `actions-box` = TRUE)),
            br(),
            br()
     )
    )
   )
     


# Define server logic 
server <- function(input, output) {

  selectedData <- reactive({
    
    compare <- input$compare 
    clust <- input$clust
    imp_measure <- input$iMeasure
    sampleSize <- input$sampleSize
    design <- input$design
    coOcc <- input$typeCo
    typeRel <- input$typeRel
    measureOrder <- input$measureOrder
    sampleType <- input$sampleType
    eval_input <- input$eval_input
    domain_input <- input$domain_input
    depend_input <- input$depend_input
    sourceRel <- input$sourceRel
    numCo <- input$numCo
    orderRel<- input$orderRel
    supra <- input$supra
    modVariable <- input$modVar
    
    if(input$clust != "All"){
      data %>%
        filter(
               comparison %in% compare,
               pubStat == clust, 
               impMeasureAgg %in% imp_measure,
               N >= sampleSize,
               numCoOcc >= numCo,
               withinBetween %in% design,
               typeCoOcc %in% coOcc,
               typeRelational %in% typeRel,
               coOccRelOrder %in% orderRel,
               expImpOrder %in% measureOrder,
               supralim %in% supra,
               studySample %in% sampleType,
               depend %in% depend_input,
               domain %in% domain_input,
               sourceRelational %in% sourceRel,
               eval %in% eval_input
               ) 
    } else{
      data %>%
        filter(
              comparison %in% compare, 
               impMeasureAgg %in% imp_measure, 
               N >= sampleSize,
               numCoOcc >= numCo,
               withinBetween %in% design, 
               typeCoOcc %in% coOcc, 
               supralim %in% supra, 
               typeRelational %in% typeRel, 
               coOccRelOrder %in% orderRel, 
               expImpOrder %in% measureOrder, 
               studySample %in% sampleType,
               sourceRelational %in% sourceRel, 
               depend %in% depend_input,
               domain %in% domain_input,
               eval %in% eval_input
               ) 
    }
      })
  
  #SUMMARY TABLE  
  output$summary<- renderTable({
    selectedData() %>%
      select(-measure, -effType)
  })
  
  output$modImp <- renderPrint(
    {
      if (input$modData == TRUE){
        modVariable <- input$modVar
        predictor <- data %>% 
          filter(comparison %in% c("AC", "DB", "DC")) %>%
          select(modVariable)
        predictor <- as.vector(predictor)
        predictor <- predictor %>% unlist(recursive = FALSE)
        modelData <- data
        
      } else {
        modVariable <- input$modVar
        predictor <- selectedData() %>% 
          filter(comparison %in% c("AC", "DB", "DC")) %>%
          select(modVariable)
        predictor <- as.vector(predictor)
        predictor <- predictor %>% unlist(recursive = FALSE)
        modelData <- selectedData()
      }

      
      if(input$modStr == "Effect of Moderator"){
        print(metafor::rma.mv(yi = effFinalG, V = varG,
                              mods = ~ predictor, random = ~ 1 | paperID,
                              data = modelData[modelData$comparison %in% c("AC", "DB", "DC"), ],
                              method = "ML"))
      }
      else if(input$modStr == "Effect of Moderator + Measure type") {
        print(metafor::rma.mv(yi = effFinalG, V = varG,
                              mods = ~ measure + predictor, random = ~ 1 | paperID,
                              data = modelData[modelData$comparison %in% c("AC", "DB", "DC"), ],
                              method = "ML"))
      } else {
      print(metafor::rma.mv(yi = effFinalG, V = varG,
                             mods = ~ measure * predictor, random = ~ 1 | paperID,
                             data = modelData[modelData$comparison %in% c("AC", "DB", "DC"), ],
                             method = "ML"))
      
    }
  })
  
  #FILTERED DATATABLE
  output$table <- DT::renderDataTable({
    
    DT::datatable(selectedData()[,c(
      "paperID","studyID","cond","effType", "N", "effFinal",
      "withinBetween", "indSamp","pubStat","studySample","domain","typeCoOcc","numCoOcc",
      "supralim","typeRelational", "sourceRelational", "coOccRelOrder",
      "depend","expImpOrder","impMeasure", "eval"
    )], options = list(searching = TRUE, scrollX = T, pageLength = 5))
  })
  

  
   output$plotImplicit <- renderPlotly({
      p <- ggplot(selectedData(), aes(x= effType, y= effFinal, color = comparison)) +
        geom_violin() + stat_summary(fun.data=mean_sdl, mult=1, 
                                     geom="pointrange", color="grey") +
        geom_jitter(alpha = 0.5, aes(text = paste(paperID, ' (Study ',studyID,')<br>', 
                                                  'Condition: ', cond,
                                                  '<br>', effType, ': ', effFinal,
                                                  '<br>N = ', N,  sep = ""))) +
        xlab("Comparison") + ylab("Effect Size") +
        geom_hline(yintercept = 0, lty = 2, color = "lightgrey") +
        theme_classic() +
        theme(axis.text.x=element_text(angle=25,hjust=1),
              legend.position = "none") + 
        scale_colour_brewer(palette = "Set2") 
      ggplotly(p, tooltip='text')
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

### deploy app
#rsconnect::deployApp(appTitle = "Kurdi_et_al_2022") #, "~/1. Kurdi et al. (2022) Shiny App"
