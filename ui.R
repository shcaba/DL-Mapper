library(shiny)
library(plotly)
library(shinyBS)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("'Data-limited' Mapper"),
    h3(strong("Decomposing the word 'data-limited' into data and resource dimensions.")),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3(strong("There are six data attributes and four resource attributes")),
            h4(p(strong("Data limitations are expressed as level of concern or constraints in a particular attribute"))),
            h4(p(strong("Attributes are scored continuously from 0 to 3 with the following interpretations:"))),
            tags$ul(tags$li(h4(p(em("0 = No concern or constraint"))))),
            tags$ul(tags$li(h4(p(em("1 = Mild concern or constraint"))))),
            tags$ul(tags$li(h4(p(em("2 = Moderate concern or constraint"))))),
            tags$ul(tags$li(h4(p(em("3 = High concern or constraint"))))),
            br(),
            
            h4(p(strong("Use the slide bar to score each attribute"))),
            
            br(),
            fluidRow(column(width=12,textInput("Spp_lab","List of fisheries to score. Separate each fishery using a comma (e.g., A,B).", value=NULL))),
            fluidRow(column(width=6,selectizeInput("fishery_choice", label="Choose which fishery to score",choices = NULL)),
                column(width=6,selectizeInput("fishery_compare", label="Choose which fisheries to compare.",choices = NULL, multiple=TRUE))),
            
#            actionBttn(
 #               inputId = "Id104",
  #              label = "Add fishery to comparison list", 
   #             style = "material-flat",
    #            color = "success"
     #       ),
            
            h3(p(strong("Data Attributes"))),
            fluidRow(column(width=6,sliderInput("D_type",
                        "Types",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3)),
            column(width=6,sliderInput("D_prez",
                        "Imprecision",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3))),
            bsTooltip("D_type", "Different types of data available, such as catch, indices of abundnace and/or biological data. The more data, the less constraints on data type. 3 = no available data.",
                      "right", options = list(container = "body")),
            bsTooltip("D_prez", "Level of imprecision based on low sample size, high measurement error or other causes of high varaiance or low signal power.",
                      "right", options = list(container = "body")),

            fluidRow(column(width=6,sliderInput("D_bias",
                        "Bias",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3)),
            column(width=6,sliderInput("D_spp",
                        "Species-specific",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3))),
            bsTooltip("D_bias", "Bias due to general representativeness issues, poorly met assumptions, or other issues.",
                      "right", options = list(container = "body")),
            bsTooltip("D_spp", "Data not collected at the species-specific level.",
                      "right", options = list(container = "body")),
            
            
            fluidRow(column(width=6,sliderInput("D_spatial",
                        "Spatial limitations",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3)),
            column(width=6,sliderInput("D_temp",
                        "Temporal limitations",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3))),
            bsTooltip("D_spatial", "Spatial limitations in the data. For example, some areas are better sampled than others. No data collection would have very bad spatial issues!",
                      "right", options = list(container = "body")),
            bsTooltip("D_temp", "Temporal or time series issues in the data. This can be data snapshots or large data gaps in important years.",
                      "right", options = list(container = "body")),

            br(),
            h3(p(strong("Resource Attributes"))),
            fluidRow(column(width=6,sliderInput("R_time",
                        "Time",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3)),
            column(width=6,sliderInput("R_funds",
                        "Funding",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3))),
 
            bsTooltip("R_time", "Major time constraints in performing data analysis and stock assessment. Such a constraint can limit the number and types of assessments that can be done.",
                      "right", options = list(container = "body")),
            bsTooltip("R_funds", "Major funding constraints that limit the collection of data or ability to support the stock assessment process.",
                      "right", options = list(container = "body")),
            
            fluidRow(column(width=6,sliderInput("R_cap",
                        "Technical capacity",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3)),
            column(width=6,sliderInput("R_an2stocks",
                        "Analysts:Stocks",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = 3))),
            bsTooltip("R_cap", "Technical capacity constraints to conduct stock assessments of varying complexity.",
                      "right", options = list(container = "body")),
            bsTooltip("R_an2stocks", "Ratio of the number of stock assessment analysts to the number of stock needing to be assessed. A small ratio will compromise the number and complexity of stock assessments in the system.",
                      "right", options = list(container = "body")),


br(),
br(),
br(),
fluidRow(column(width=3,numericInput("traindata","Train on data",value=3,min=0,max=3,ste=0.001)),
         column(width=3,numericInput("locknow","Local Knowledge",value=3,min=0,max=3,ste=0.001)),
         column(width=3,numericInput("DoDL","Do DLMs",value=3,min=0,max=3,ste=0.001)),
         column(width=3,numericInput("Docomplex","Do complex",value=3,min=0,max=3,ste=0.001))),
fluidRow(column(width=3,numericInput("DMM","Dynamic MM",value=3,min=0,max=3,ste=0.001)),
         column(width=3,numericInput("Modspecs","Modspecs",value=3,min=0,max=3,ste=0.001)),
         column(width=3,numericInput("Gov","Governamce",value=2.5,min=0,max=3,ste=0.001)))

         
        ),

        # Show a plot of the generated distribution
        mainPanel(
            br(),
            br(),
            br(),
            br(),
            h4(p(strong("Three plots are created in the main panel:"))),
            tags$ul(tags$li(h4(p(em("A lollipop plot detailing attribute scores for a specific fishery"))))),
            tags$ul(tags$li(h4(p(em("A biplot with the average data and resource scores on each axis for comparisons across fisheries"))))),
            tags$ul(tags$li(h4(p(em("A parallel coordinate plot detailing attribute comparisons across fisheries"))))),
            fluidRow(column(width=6,h3("Plot of individual attribute scores", align = "center")),
                     column(width=6,h3("Plot of strategic",tags$a(href="javascript:window.open('Guidance_definitions.html', '_blank','width=600,height=400')","guidance"),"based on attribute scores", align = "center"))),
            fluidRow(column(width=6,plotOutput("LolliPlot")),
                     column(width=6,plotOutput("LolliPlot.principles"))),
            fluidRow(column(width=3,downloadButton('downloadlollipopplots', 'Download DL Attributes plot')),
                     column(width=3,downloadButton('downloadAttScores', 'Download DL Attributes scores')),
                     column(width=3,downloadButton('downloadlollipop.principles', 'Download Guidance plot')),
                     column(width=3,downloadButton('downloadGuideScores', 'Download Guidance scores'))),
#            br(),
            br(),
            br(),
            fluidRow(column(width=4,h3("Biplot of average attribute scores", align = "center")),
                     column(width=8,h3("Comparison of attribute scores across examples fisheries", align = "left"))),
            #fluidRow(column(width=4,h4("")),
            #         column(width=8,h4("Attribute values can be highlighted by dragging the cursor across values", align = "left"))),
            #fluidRow(column(width=4,h4("")),
            #         column(width=8,h4("Only those fisheries that meet all highlighted values will be emphasized", align = "left"))),
            fluidRow(column(width=4,plotlyOutput("QuadPlot")),
                     column(width=8,plotlyOutput("ParCoorPlot")))
#            br(),
#            br(),
#            h3("Comparison of attribute scores across examples fisheries", align = "left"),
#            h4("Attribute values can be highlighted by dragging the cursor across values", align = "left"),
#            h4("Only those fisheries that meet all highlighted values will be emphasized", align = "left"),
 #           plotlyOutput("ParCoorPlot")
        )
    )
))
