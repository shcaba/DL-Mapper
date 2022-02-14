#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinyBS)


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
            fluidRow(column(width=12,textInput("Spp_lab","Species name for label", value=NULL))),
            fluidRow(column(width=12,selectizeInput("fishery_choice", label=NULL,choices = NULL))),
            h3(p(strong("Data Attributes"))),
            fluidRow(column(width=6,sliderInput("D_type",
                        "Types:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0)),
            column(width=6,sliderInput("D_prez",
                        "Imprecision:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0))),
            bsTooltip("D_type", "Different types of data available, such as catch, indices of abundnace and/or biological data. The more data, the less constraints on data type.",
                      "right", options = list(container = "body")),
            bsTooltip("D_prez", "Level of imprecision based on low sample size, high measurement error or other causes of high varaiance or low signal power.",
                      "right", options = list(container = "body")),

            fluidRow(column(width=6,sliderInput("D_bias",
                        "Bias:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0)),
            column(width=6,sliderInput("D_spp",
                        "Species-specific:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0))),
            bsTooltip("D_bias", "Bias due to general representativeness issues, poorly met assumptions, or other issues.",
                      "right", options = list(container = "body")),
            bsTooltip("D_spp", "Data not collected at the species-specific level.",
                      "right", options = list(container = "body")),
            
            fluidRow(column(width=6,sliderInput("D_spatial",
                        "Spatial limitations:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0)),
            column(width=6,sliderInput("D_temp",
                        "Temporal limitations:",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0))),
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
                        step=0.01,
                        value = 0)),
            column(width=6,sliderInput("R_funds",
                        "Funding",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0))),
 
            bsTooltip("R_time", "Major time constraints in performing data analysis and stock assessment. Such a constraint can limit the number and types of assessments that can be done.",
                      "right", options = list(container = "body")),
            bsTooltip("R_funds", "Major funding constraints that limit the collection of data or ability to support the stock assessment process.",
                      "right", options = list(container = "body")),
            
            fluidRow(column(width=6,sliderInput("R_cap",
                        "Technical capacity",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0)),
            column(width=6,sliderInput("R_an2stocks",
                        "Analysts:Stocks",
                        min = 0,
                        max = 3,
                        step=0.01,
                        value = 0))),
            bsTooltip("R_cap", "Technical capacity constraints to conduct stock assessments of varying complexity.",
                      "right", options = list(container = "body")),
            bsTooltip("R_an2stocks", "Ratio of the number of stock assessment analysts to the number of stock needing to be assessed. A small ratio will compromise the number and complexity of stock assessments in the system.",
                      "right", options = list(container = "body"))
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
                     column(width=6,h3("Biplot of average data and resource attribute scores", align = "center"))),
            fluidRow(column(width=6,plotOutput("LolliPlot")),
                     column(width=6,plotlyOutput("QuadPlot"))),
                    downloadButton('downloadlollipopplots', 'Download Lollipop plot'),
            br(),
            br(),
            br(),
            h3("Comparison of attribute scores across examples fisheries", align = "left"),
            h4("Attribute values can be highlighted by dragging the cursor across values", align = "left"),
            h4("Only those fisheries that meet all highlighted values will be emphasized", align = "left"),
            plotlyOutput("ParCoorPlot")
        )
    )
))
