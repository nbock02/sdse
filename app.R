# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(shiny)
library(wesanderson)
library(tidyverse)
library(shiny)
library(plotly)
library(stringr)
library(zoo)
library(reshape2)
library(markdown)
library(BSDA)
library(shinyjs)
library(bslib)
library(vroom)
source("sdse_supportFunctions.R")

globalRepLimit = 100
cols <- wes_palette("Darjeeling1",n=6,type = "continuous")
cols[5] <- "#EFCA08"
# Define UI for application that draws a histogram
ui <- navbarPage("",id="maintab",
                 theme = bs_theme(bootswatch="yeti",base_font = "Arial"),
                 # Sidebar with a slider input for number of bins
                 
                 tabPanel("Introduction",
                          fluidRow(
                            column(12, align = "center",
                                   includeMarkdown("mdt/title.md")
                            )
                          )
                          ),
                          
                 tabPanel("Descriptive Statistics",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectizeInput("ds_genre","Select genre",
                                                        choices = genreSublist),
                                         actionButton("ds_sampleClick", "Draw data point")),
                            mainPanel(
                              tabsetPanel(type = "tabs",id="dstab",
                                          tabPanel("Part 1.1",fluid=TRUE,id="dstab1",
                                                   br(),
                                                   plotlyOutput("ds_hist_1",height = 200),
                                                   br(),
                                                   wellPanel(htmlOutput("dsText_1")),
                                                   br(),
                                                   includeMarkdown("mdt/ds_1.md"),
                                                   #textOutput("tabTest"),
                                                   # textOutput("counter"),
                                          ),
                                          tabPanel("Part 1.2",fluid=TRUE,id="dstab2",
                                                   br(),
                                                   plotlyOutput("ds_hist_2",height = 200),
                                                   br(),
                                                   wellPanel(htmlOutput("dsText_2")),
                                                   br(),
                                                   includeMarkdown("mdt/ds_2.md")),
                                          )))),
                 
                 tabPanel("Inferential Statistics",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectInput("is_numDraws", "# data points to draw", 
                                                     choices = c(1,10,25,100),
                                                     selected = 1),
                                         selectizeInput("is_genre","Select genre",
                                                        choices = genreSublist),
                                         actionButton("is_sampleClick", "Draw data point")),
                            mainPanel(
                              tabsetPanel(type = "tabs",id = "istab",
                                          tabPanel("Part 2.1",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("is_hist_1",height = 200),
                                                   br(),
                                                   plotlyOutput("is_mean_1",height = 250),
                                                   br(),
                                                   wellPanel(htmlOutput("isText_1")),
                                                   br(),
                                                   includeMarkdown("mdt/is_1.md")),
                                          
                                          tabPanel("Part 2.2",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("is_hist_2",height = 200),
                                                   br(),
                                                   plotlyOutput("is_mean_2",height = 250),
                                                   br(),
                                                   wellPanel(htmlOutput("isText_2")),
                                                   br(),
                                                   includeMarkdown("mdt/is_2.md"),
                                                   br(),
                                                   wellPanel(style = "background: #ffe599ff;
                                                             padding: 5px",
                                                             includeMarkdown("mdt/is_2_proTip.md")),
                                                   br()),
                                          tabPanel("Part 2.3",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("is_hist_3",height = 200),
                                                   br(),
                                                   plotlyOutput("is_mean_3",height = 250),
                                                   br(),
                                                   plotlyOutput("sample_means_1",height=250),
                                                   br(),
                                                   wellPanel(htmlOutput("isText_3")),
                                                   br(),
                                                   includeMarkdown("mdt/is_3.md")),
                                          tabPanel("Part 2.4",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("is_hist_4",height = 200),
                                                   br(),
                                                   plotlyOutput("is_mean_4",height = 250),
                                                   br(),
                                                   plotlyOutput("sample_means_2",height=250),
                                                   br(),
                                                   wellPanel(htmlOutput("isText_4")),
                                                   br(),
                                                   includeMarkdown("mdt/is_4.md")),
                                          
                              )
                            )
                          )
                 ),

                 tabPanel("Comparing Genres",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectizeInput("gc_genre_1", "Genre 1", choices = NULL),
                                         selectizeInput("gc_genre_2", "Genre 2", choices = NULL),
                                         selectInput("gc_audioFeature", "Audio Feature", choices = paramList),
                                         selectInput("gc_numDraws", "# data points to draw", 
                                                     choices = c(1,10,25,100),selected=1),
                                         actionButton("gc_sampleClick", "Draw data point"),
                                         actionButton("reset", "Reset")),
                            mainPanel(
                              tabsetPanel(type = "tabs",id="gctab",
                                          tabPanel("Part 3.1",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("genre_mean_1",height = 200),
                                                   br(),
                                                   wellPanel(htmlOutput("cgText_1")),
                                                   br(),
                                                   includeMarkdown("mdt/cg_1.md")),
                                          tabPanel("Part 3.2",fluid=TRUE,
                                                   column(width = 12,
                                                   br(),
                                                   plotlyOutput("genre_mean_2",height = 200),
                                                   br(),
                                                   wellPanel(htmlOutput("cgText_2")),
                                                   br(),
                                                   includeMarkdown("mdt/cg_2.md"),
                                                   #br(),
                                                   wellPanel(style = "background: #ffe599ff;height: 50px;
                                                             padding-bottom: 0px; padding-bottom: 5px;
                                                             padding-left: 5px; padding-right: 5px;
                                                             border-color: black",
                                                             includeMarkdown("mdt/cg_2_proTip.md")))),
                                          tabPanel("Part 3.3",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("genre_mean_3",height = 200),
                                                   br(),
                                                   wellPanel(htmlOutput("cgText_3")),
                                                   br(),
                                                   includeMarkdown("mdt/cg_3.md")),
                                          tabPanel("Part 3.4",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("genre_mean_4",height = 200),
                                                   br(),
                                                   plotlyOutput("genre_hist_1",height=200),
                                                   br(),
                                                   wellPanel(htmlOutput("cgText_4")),
                                                   br(),
                                                   includeMarkdown("mdt/cg_4.md")),
                                          tabPanel("Part 3.5",fluid=TRUE,
                                                   br(),
                                                   plotlyOutput("genre_mean_5",height = 200),
                                                   br(),
                                                   plotlyOutput("genre_hist_2",height=200),
                                                   br(),
                                                   wellPanel(htmlOutput("cgText_5")),
                                                   br(),
                                                   includeMarkdown("mdt/cg_5.md"))
                                          )
                            )
                          )
                 ),
                 tabPanel("Confidence Intervals",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectizeInput("ci_genre_1", "Genre 1", choices = NULL),
                                         selectizeInput("ci_genre_2", "Genre 2", choices = NULL),
                                         selectInput("ci_audioFeature", "Audio Feature", choices = paramList),
                                         actionButton("ci_sampleClick", "Draw sample")),
                            mainPanel(br(),
                                      plotlyOutput("ci_hist_1",height = 200),
                                      br(),
                                      plotlyOutput("ci_hist_2",height = 200),
                                      br(),
                                      plotlyOutput("ci_hist_3",height = 200),
                                      #wellPanel(htmlOutput("cgText_1")),
                                      br(),
                                      includeMarkdown("mdt/ci_1.md"))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  output$titleimage <- renderImage({list(src = "mdt/title.jpg",
                                         width = 800)},
                                   deleteFile = FALSE)
  
  genreList <- sort(unique(trackData$genre))
  randomGenre <- genreSublist[floor(runif(1,min =1, max = length(genreSublist)))]
  
  updateSelectizeInput(session,'gc_genre_1', choices = genreList, server = TRUE,
                       selected = randomGenre)
  updateSelectizeInput(session,'gc_genre_2', choices = genreList, server = TRUE,
                       selected = genreList[runif(1,min=2,max =length(genreList))])
  
  updateSelectizeInput(session,'ci_genre_1', choices = genreList, server = TRUE,
                       selected = randomGenre)
  updateSelectizeInput(session,'ci_genre_2', choices = genreList, server = TRUE,
                       selected = genreList[runif(1,min=2,max =length(genreList))])
  
  #Get sampling data for randomGenre
  ds_sampleData <- reactive(
    trackData %>%
      filter(genre==input$ds_genre) %>%
      select(popularity,genre,artist,song,tempo) %>%
      #mutate(
      arrange(desc(popularity))
  )

  is_sampleData <- reactive(
    trackData %>%
      filter(genre==input$is_genre) %>%
      select(popularity,genre,artist,song,tempo) %>%
      #mutate(
      arrange(desc(popularity))
  )
  
  #Generate sampling data for genre_1
  gc_sampleData_1 <- reactive(
    trackData %>% 
      filter(genre == input$gc_genre_1) %>% 
      arrange(desc(popularity))
    )
  
  #Generate sampling data for genre_1
  gc_sampleData_2 <- reactive(
    trackData %>% 
      filter(genre == input$gc_genre_2) %>% 
      arrange(desc(popularity))
    )
  
  #Generate ci sampling data for genre_1
  ci_sampleData_1 <- reactive(
    trackData %>% 
      filter(genre == input$ci_genre_1) %>% 
      arrange(desc(popularity))
  )
  
  #Generate ci sampling data for genre_1
  ci_sampleData_2 <- reactive(
    trackData %>% 
      filter(genre == input$ci_genre_2) %>% 
      arrange(desc(popularity))
  )

  dsText <- renderText(getGenreText(ds_sampleData()))
  lapply(1:2,function(i){
    outputID<-paste("dsText",i,sep="_")
    output[[outputID]] = dsText
  })
  
  isText <- renderText(getGenreText(is_sampleData()))
  lapply(1:4,function(i){
    outputID<-paste("isText",i,sep="_")
    output[[outputID]] = isText
  })
  
  cgText <- renderText(getGenreText(gc_sampleData_1(),gc_sampleData_2()))
  lapply(1:5,function(i){
    outputID<-paste("cgText",i,sep="_")
    output[[outputID]] = cgText
  })

  #######Initialize Data##########
  ds_data <- reactiveValues(value = NA)
  is_data <- reactiveValues(value=NA)
  gc_data_1<-reactiveValues(value = NA)
  gc_data_2<-reactiveValues(value = NA)
  ci_data_1<-reactiveValues(value = NA)
  ci_data_2<-reactiveValues(value = NA)
  #################################
  
  ##########Click Observers##########
  observeEvent(input$ds_sampleClick,{
    ds_data$value <- generate_data(samplingData = ds_sampleData(),
                                   inputData = ds_data$value,
                                   draws = 1,
                                   feat = "tempo")
  })
  
  observeEvent(input$is_sampleClick,{
    is_data$value <- generate_data(samplingData = is_sampleData(),
                                     inputData = is_data$value,
                                     draws = as.numeric(input$is_numDraws),
                                     feat = "tempo")
  })
  
  observeEvent(input$gc_sampleClick,{
    gc_data_1$value<-generate_data(samplingData = gc_sampleData_1(),
                                   inputData = gc_data_1$value,
                                   draws = as.numeric(input$gc_numDraws),
                                   feat = input$gc_audioFeature)
    
    gc_data_2$value<-generate_data(samplingData = gc_sampleData_2(),
                                      inputData = gc_data_2$value,
                                      draws = as.numeric(input$gc_numDraws),
                                      feat = input$gc_audioFeature)
    
  })
  
  observeEvent(input$ci_sampleClick,{
    ci_data_1$value<-generate_data(samplingData = ci_sampleData_1(),
                                    inputData = NA,
                                    draws = 100,
                                    feat = input$ci_audioFeature)
    
    ci_data_2$value<-generate_data(samplingData = ci_sampleData_2(),
                                      inputData = NA,
                                      draws = 100,
                                      feat = input$ci_audioFeature)
    
  })
  ###################################
  
  ##########Tab Observers##########
  #Reset SD counter + data if dstab changed
  observeEvent(input$dstab,{
    ds_data$value = NA
  })
  
  #Reset SD counter + data if initial genre is changed
  observeEvent(input$init_genre,{
    ds_data$value = NA
  })
  
  #Reset boot counter + data if istab is switched
  observeEvent(input$istab,{
    is_data$value = NA
  })
  
  #Reset genre counter + data if dstab is switched 
  observeEvent(input$gctab,{
    gc_data_1$value = NA
    gc_data_2$value = NA
  })
  
  #Reset everything if main tab is switched
  observeEvent(input$maintab,{
    is_data$value = NA
    ds_data$value = NA
    gc_data_1$value = NA
    gc_data_2$value = NA
    ci_data_1$value = NA
    ci_data_2$value = NA
  })
  #################################

  ##########GC Input Observers##########
  #Reset genre comparison counter to 0 if user changes genre 1
  observeEvent(input$gc_genre_1, {
    gc_data_1$value = NA
    gc_data_2$value = NA
  })
  
  #Reset genre comparison counter to 0 if user changes genre 2
  observeEvent(input$gc_genre_2, {
    gc_data_1$value = NA
    gc_data_2$value = NA
  })
  
  #Reset genre comparison counter to 0 if user changes audio parameter
  observeEvent(input$gc_udioFeature,{
    gc_data_1$value = NA
    gc_data_2$value = NA
  })
  ###################################
 
  ##########CI Input Observers##########
  #Reset genre comparison counter to 0 if user changes genre 1
  observeEvent(input$ci_genre_1, {
    ci_data_1$value = NA
    ci_data_2$value = NA
  })
  
  #Reset genre comparison counter to 0 if user changes genre 2
  observeEvent(input$ci_genre_2, {
    ci_data_1$value = NA
    ci_data_2$value = NA
  })
  
  #Reset genre comparison counter to 0 if user changes audio parameter
  observeEvent(input$ci_audioFeature,{
    ci_data_1$value = NA
    ci_data_2$value = NA
  })
  ###################################
   
  ##########UI en/disablers##########
  observeEvent(input$istab, {
    if(input$istab=="Part 2.1"){
      useShinyjs()
      output$observerTest <- renderText(input$istab)
      shinyjs::toggleState("is_numDraws")
    }
  })
  ###################################
  
  ###################HISTOGRAMS FOR DESCRIPTIVE STATISTICS PAGE###################
  output$ds_hist_1 <- renderPlotly(
    generate_histogram(data = ds_data$value, repLimit = globalRepLimit)
  )
  
  output$ds_hist_2 <- renderPlotly(
    generate_histogram(data = ds_data$value, repLimit = globalRepLimit)
  )
  
  output$ds_hist_3 <- renderPlotly(
    generate_histogram(data = ds_data$value, repLimit = globalRepLimit)
  )
  
  output$ds_hist_4 <- renderPlotly(
    generate_histogram(data = ds_data$value, repLimit = globalRepLimit)
  )
  
  ###################HISTOGRAMS FOR INFERENTIAL STATISTICS PAGE###################
  output$is_hist_1 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit)
  )
  output$is_hist_2 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit)
    
  )
  output$is_hist_3 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit)
  )
  
  output$is_hist_4 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit)
  )
  
  output$sample_means_1 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit,
                                    means = T))
    
  output$sample_means_2 <- renderPlotly(
    is_hist <- generate_histogram(data = is_data$value, repLimit = globalRepLimit,
                                    means = T))
  
  ###################LINEPLOTS FOR INFERENTIAL STATISTICS PAGE###################
  output$is_mean_1 <- renderPlotly({
    is_mean <- generate_timeseries(data = is_data$value,repLimit = globalRepLimit)
  })
  output$is_mean_2 <- renderPlotly({
    is_mean <- generate_timeseries(data = is_data$value,repLimit = globalRepLimit)
  })
  output$is_mean_3 <- renderPlotly({
    is_mean <- generate_timeseries(data = is_data$value,repLimit = globalRepLimit)
  })
  output$is_mean_4 <- renderPlotly({
    is_mean <- generate_timeseries(data = is_data$value,repLimit = globalRepLimit)
  })
  
  ###################LINEPLOTS FOR GENRE COMPARISON PAGE###################
  output$genre_mean_1 <- renderPlotly({
    genre_mean <- generate_gc_timeseries(genre_1_data = gc_data_1$value,
                                         genre_2_data = gc_data_2$value,
                                         repLimit = globalRepLimit) })
  
  output$genre_mean_2 <- renderPlotly({
    genre_mean <- generate_gc_timeseries(genre_1_data = gc_data_1$value,
                                         genre_2_data = gc_data_2$value,
                                         repLimit = globalRepLimit) })
  
  output$genre_mean_3 <- renderPlotly({
    genre_mean <- generate_gc_timeseries(genre_1_data = gc_data_1$value,
                                         genre_2_data = gc_data_2$value,
                                         repLimit = globalRepLimit) })
  
  output$genre_mean_4 <- renderPlotly({
    genre_mean <- generate_gc_timeseries(genre_1_data = gc_data_1$value,
                                         genre_2_data = gc_data_2$value,
                                         repLimit = globalRepLimit) })
  
  output$genre_mean_5 <- renderPlotly({
    genre_mean <- generate_gc_timeseries(genre_1_data = gc_data_1$value,
                                         genre_2_data = gc_data_2$value,
                                         repLimit = globalRepLimit)})
  
  ###################HISTOGRAMS FOR GENRE COMPARISON PAGE###################
      output$genre_hist_1 <- renderPlotly({
        generate_gc_histogram(genre_1_data = gc_data_1$value,
                              genre_2_data = gc_data_2$value,
                              repLimit = globalRepLimit)})
      
      output$genre_hist_2 <- renderPlotly({
        generate_gc_histogram(genre_1_data = gc_data_1$value,
                              genre_2_data = gc_data_2$value,
                              repLimit = globalRepLimit)})
      
  ###################HISTOGRAMS FOR DESCRIPTIVE STATISTICS PAGE###################
  output$ci_hist_1 <- renderPlotly(
    generate_histogram(data = ci_data_1$value, repLimit = globalRepLimit,plotCols = cols[5],
                       genreTitle = T)
  )
  
  output$ci_hist_2 <- renderPlotly(
    generate_histogram(data = ci_data_2$value, repLimit = globalRepLimit,
                       plotCols = cols[6], genreTitle = T)
  )
  
  output$ci_hist_3 <- renderPlotly({
    generate_ci_histogram(genre_1_data = ci_data_1$value,
                          genre_2_data = ci_data_2$value)})
      
}

# Run the application 
shinyApp(ui = ui, server = server)
#profvis::profvis(runApp(shinyApp(ui, server)))
