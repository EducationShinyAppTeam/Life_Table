library(shiny)
library(shinydashboard)
library(plotrix)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(V8)
library(png)
library(ggplot2)
library(plotly)
library(shinycssloaders)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Life Table",
                                    tags$li(class = "dropdown",
                                            tags$a(href= "https://shinyapps.science.psu.edu/",
                                                   icon("home",lib = "font-awesome"))),
                                    tags$li(class = "dropdown",
                                            actionLink("info",icon("info"),class = "myClass"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
                        menuItem("Challenge", tabName = "lifetable",icon = icon("cogs"),
                                 menuSubItem("Survival Rate", tabName = "survival_rate"),
                                 menuSubItem("Cohort Population Pyramid", tabName = "cohort"),
                                 menuSubItem("Actual Population Pyramid", tabName = "actual"),
                                 menuSubItem("Fecundity Rate", tabName = "fecundity_rate")
                        ))
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3CBAAD}")),
                        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #3CBAAD}")),
                        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #3CBAAD}")),
                        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {border-color: #3CBAAD}")),
                        tags$style(HTML(
                          '.popover-title{
                          color:black;
                          font-size:18px;
                          background-color: #e9f2f1
                          }'
                        ))),
                      tabItems(
                        #Overview Tab
                        tabItem(tabName = "overview",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will explore various Life Tables with 3 difference countries"),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("Survival Rate: Click the country and sex combination you preferred to compare, and view the distinct Survival Rate for each country and sex. ")),
                                h4(tags$li("Cohort Population Pyramid: View and compare Population Pyramid for difference countries, and also the combination of sex and country. ")),
                                h4(tags$li("Actual Population Pyramid: Click the start button to view the movie of population change by year in United States and United Kingdom. ")),
                                h4(tags$li("Fecundity Rate: Click the country you preferred, and view the difference Fecundity Rate (per 1,000 women). ")),
                                
                                
                                div(style = "text-align:center",
                                    actionButton("go", "G O !", icon("bolt"), size = "medium",style = 'color: #fff; background-color: #3CBAAD',class="circle grow")),
                                #div(style = "text-align: center",bsButton("start","Go to the overview",icon("bolt"),style = "danger",size = "large",class="circle grow")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Yuqing Lei.")
                        ),
                        
                        # Challenge Tab
                        tabItem(tabName = "survival_rate",
                                fluidPage(
                                  tabsetPanel(
                                    tabPanel("Survival Rate", fluid = TRUE,
                                             sidebarLayout(
                                               sidebarPanel(
                                                 checkboxGroupInput("check", "Select the gender(s) and countris you prefer to compare", choices = c("United Kingdom-Male", "United Kingdom-Female", "United States-Male", "United States-Female", "China-Male", "China-Female")),
                                                 verbatimTextOutput(outputId = "res1"),
                                                 bsButton("selectAll_s","Select All",size="small"),
                                                 bsButton("ref1","Show Reference", size="small")),
                                               mainPanel(
                                                 plotOutput("lineChart"),

                                                 bsPopover("lineChart", "Instruction", "This plot shows the Survival Rate to age for comparing 3 countries and sex"),

                                                 br(),br(),br(),br(),
                                                 # span(htmlOutput("refer1"),)
                                                 htmlOutput("refer1", container = tags$div, class = "custom-li-output")
                                               )))))),
                  
                        tabItem(tabName = "cohort",
                                fluidPage(
                                  tabsetPanel(
                                    tabPanel("Country Comparison", fluid = TRUE,
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons("check2", "Select the countris you prefer to compare", choices = c("United Kingdom-United States", "United Kingdom-China", "United States-China")),
                                                 bsButton("ref2","Show Reference", size="small"),
                                                 verbatimTextOutput(outputId = "res2")),
                                               # actionButton("selectAll2","Select All",size="small")),
                                               
                                               mainPanel(
                                                 fluidRow(

                                                     htmlOutput("refer2", container = tags$div, class = "custom-li-output"),
                                                 column(7, offset = 5, uiOutput('title1'))),

                                                 plotlyOutput("pyramid")%>% withSpinner(color="#3CBAAD"), width = '100%', heights = '100%'
                                               ))),
                                    tabPanel("Country-Sex Comparison", fluid = TRUE,
                                             sidebarLayout(
                                               sidebarPanel(
                                                 #choose the left and right randomly
                                                 fluidRow(
                                                   column(width=12, h4("Please select the countries with genders you'd like to show in Pyramid")),
                                                   radioButtons(inputId = "check4", label = "Right", selected = "United Kingdom-Male",
                                                                choices = c("United Kingdom-Male", "United Kingdom-Female", "United States-Male","United States-Female", "China-Male", "China-Female")),
                                                   verbatimTextOutput(outputId = "res3"),
                                                   radioButtons(inputId = "check5", label = "Left", selected = "United Kingdom-Female",
                                                                choices =  c("United Kingdom-Male", "United Kingdom-Female", "United States-Male","United States-Female", "China-Male", "China-Female")),
                                                   bsButton("ref3","Show Reference", size="small"),
                                                   verbatimTextOutput(outputId = "res4")
                                                 )),
                                               mainPanel(
                                                 plotlyOutput("pyramid2")%>% withSpinner(color="#3CBAAD"),
                                                 htmlOutput("refer3", container = tags$div, class = "custom-li-output"))
                                             ))))),
                        tabItem(tabName = "actual",
                                fluidPage(
                                  tabsetPanel(
                                    tabPanel("United States", fluid = TRUE,
                                             fluidRow(
                                               column(4,

                                               sliderInput('check3', 'Year', min = 1900, max = 2010, value = 1900, round = TRUE, sep = "", animate = TRUE),
                                               bsButton("ref4","Show Reference", size="small")),

                                                      
                                               column(8,
                                                      htmlOutput("refer4", container = tags$div, class = "custom-li-output"))
                                             ),
                                             hr(),
                                             plotOutput("intercensal_us")),
                                    # tabPanel("United States", fluid = TRUE,
                                    #          sidebarLayout(
                                    #            sidebarPanel(
                                    #              fluidRow(
                                    #              sliderInput('check3', 'Year', min = 1900, max = 2010, value = 1900, round = TRUE, sep = "", animate = TRUE),
                                    #              bsButton("ref4","Show Reference", size="small"))),
                                    #            
                                    #            mainPanel(
                                    #              htmlOutput("refer4", container = tags$div, class = "custom-li-output"),
                                    #              plotOutput("intercensal_us")))),
                                    tabPanel("United Kingdom", fluid = TRUE,
                                             fluidRow(
                                               column(4,

                                                 sliderInput('check6', 'Year', min = 1991, max = 2018, value = 1991, round = TRUE, sep = "", animate = TRUE),
                                                 bsButton("ref5","Show Reference", size="small")),
                                               column(8,
                                             htmlOutput("refer5", container = tags$div, class = "custom-li-output"))),
                                             hr(),
                                             plotOutput("intercensal_uk"))
                                             
                                    ))),
                        
                        tabItem(tabName = "fecundity_rate",
                                fluidPage(
                                  tabsetPanel(
                                    tabPanel("Fecundity Rate", fluid = TRUE,
                                             sidebarLayout(
                                               sidebarPanel(
                                                 checkboxGroupInput("check7", "Select the countris you prefer to compare", choices = c("United Kingdom", "United States", "China")),
                                                 verbatimTextOutput(outputId = "res5"),
                                                 bsButton("selectAll_f","Select All",size="small"),
                                                 bsButton("ref6","Show Reference", size="small")),
                                               mainPanel(
                                                 plotOutput("lineChart_2"),

                                                 bsPopover("lineChart_2", "Instruction", "This plot shows Fecundity Rate (per 1,000 women) to age for comparing 3 countries"),

                                                 br(),br(),br(),br(),
                                                 htmlOutput("refer6", container = tags$div, class = "custom-li-output")
                                               ))))))
                        
                      )))