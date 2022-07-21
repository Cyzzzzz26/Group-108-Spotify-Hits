library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(wordcloud)
library(dplyr)

main = read_csv("Spotify 2010 - 2019 Top 100.csv",show_col_types = FALSE)
main2 = read_csv("Spotify 2010 - 2019 Top 100-1.csv",show_col_types = FALSE)
stat= main2%>%
  filter(artist=='Adele')%>%
  group_by(artist,top_year)%>%
  summarise(Count=n())

ui <- dashboardPage(
    dashboardHeader(title = "Spotify Top Hits"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "page1", icon = icon("home")),
            menuItem("Data Overview", tabName = "page2", icon = icon("file")),
            menuItem("Data Analysis", tabName = "page3", icon = icon("envelope-open")),
            menuItem("Statistics", tabName = "page4", icon = icon("chart-line")),
            menuItem("Trend", tabName = "page5", icon = icon("align-left")),
            menuItem("Word Cloud", tabName = "page6", icon = icon("cloud"))
        )
    ),
    

    dashboardBody(
      tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                font-weight: bold;
                                }
                         
                                '))),
      
      #########################################################################page1
        tabItems(
          tabItem("page1",
                  img(
                    src ='banner.png',
                    #src = "spotify-logo.png",
                    height = 210,
                    width = 1160
                  ),
                  br(),
                  br(),
                  fluidRow(
                    box(
                      title = "Abstract",
                      solidHeader = TRUE,
                      status = "primary",
                      width = 12,
                      collapsible = TRUE,
                      column(
                        12,
                        tags$div(
                          "With the innovation of information technology, our life is getting faster and faster, 
                          and people's listening tastes have become very fast, so that we are almost forgetting 
                          the very popular songs before.Our group decided to find the 2010 to 2019 on Spotify for 
                          the top100 song list, and did some data processing to check the trend of popular songs 
                          and changes in people's listening tastes at these years."
                        ),
                        style = "font-size:14px"
                      )
                    ),
                    box(
                      title = "About the Dataset", solidHeader = TRUE,
                      status = "success", width = 12, collapsible = TRUE,
                      column(12, 
                             tags$div(
                               tags$span("Spotify has an outstanding API to connect you to its ubiquitous database of songs and their features. 
                               We can get visual insights from songs you love or integrate a playback into your web application. There is also 
                               a powerful song search engine available as well as a recommendation system which helps you listen to more of what you love. 
                               In this project, we using data from the Spotify API + Rshiny app illustrate different plots and tables to tell people  how the top tracks, 
                                         artists, and genres have changed during  2009-2019. Hope you can have a deeper insight into them 
                                         through our web application."),
                               br(),
                                )
                      ),
                      
                    ),
                    box(
                      title = "About us", solidHeader = TRUE,
                      status = "warning", width = 12, collapsible = TRUE,
                      column(12,tags$div(
                               fluidRow( 
                                 column(2,img(src ="zhichao.jpg",height = 100, width = 100)),
                                 column(6,style = "font-size:16px",
                                        tags$strong("Zhichao Li"),
                                    br(),
                                    tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                    tags$li("In this project, he is responsible for ui design, Data analysis, Data Overview, Home page.")))),br(),),
                      
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="huangdian.pic.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Dian Huang"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, she is responsible for ui design, Data analysis, Data Overview, Home page.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="Xiangyu_Lei.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Xiangyu Lei"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for ui design.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="Chuyun_Zou.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Chuyun Zou"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for Statistics tab design.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="Ziyue_Luo.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Ziyue Luo"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for Trend tab.")))))
                    ),
                  )),                     
          
          #########################################################################page2                    
                     
            tabItem(
              tabName = "page2",
              
              fluidRow(
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("Spotify top songs data set contains information about 
                               top 100 songs on Spotify between 2010 and 2019. Descriptors of 
                               each songs are as follow."),
                           br(),br(),
                           tags$span(
                             "This shiny dashboard application designed to explore and compare those hits among", tags$strong("17"), "factors, including:"),
                           br(),br(),
                           fluidRow(column(5, tags$li("title - song's title"), tags$li("artist - Song's artist"), tags$li("genre - Genre of song"), tags$li("year released -Year the song was released"), 
                                           tags$li("added - Day song was added to Spotify's Top Hits playlist"), tags$li("bpm (Beats Per Minute )- The tempo of the song"), 
                                           tags$li("nrgy (Energy) - How energetic the song is"), tags$li("dnc (Danceability) - How easy it is to dance to the song")),
                                    column(5, tags$li("db (Decibel) - How loud the song is"), tags$li("live - How likely the song is a live recording"), 
                                           tags$li("val - How positive the mood of the song is"), tags$li("dur - Duration of the song"),
                                           tags$li("acous - How acoustic the song is"), tags$li("spch - The more the song is focused on spoken word"), 
                                           tags$li("pop - Popularity of the song (not a ranking)"), tags$li("top year - Year the song was a top hit"),
                                           tags$li("artist type - Tells if artist is solo, duo, trio, or a band"))
                                    
                           ),
                           br(),
                           tags$li(tags$strong("Source: "),tags$a(href = "https://www.kaggle.com/datasets/muhmores/spotify-top-100-songs-of-20152019", "Spotify Top 100 Songs of 2010-2019")),
                           
                         )
                  ),
                ),
              ),
              h2(tags$strong("Top 100 Table:")),
              h3(tags$li("By top year:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg21",
                         label = h5("Select top year:"), 
                         choices = c("2010","2011", "2012","2013","2014","2015","2016","2017","2018","2019")
                       )
                ),
                column(10,
                       dataTableOutput("myTable1")
                )
              ),
              br(),
              h3(tags$li("By top genre:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg22",
                         label = h5("Select genre:"), 
                         choices = sort(unique(main$`top genre`)),
                         selected = "dance pop"
                       )
                ),
                column(10,
                       dataTableOutput("myTable2")
                )
              ),
              br(),
              h3(tags$li("By artist type:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg23",
                         label = h5("Select artist type:"), 
                         choices = sort(unique(main$`artist type`)),
                         selected = "Solo"
                       )
                ),
                column(10,
                       dataTableOutput("myTable3")
                )
              ),
              br(),
              br(),
            ),    
              #########################################################################page3              

            tabItem(tabName = "page3",
                    h3(tags$strong("Top 5 Artist in each year")),
                    h4(tags$li("Ariana Grande, Post Malone, Billie Eilish are the top singer in recent year.")),
                    br(),
                    plotlyOutput("plot1", height = "550px"),
                    br(),
                    br(),
                    h3(tags$strong("Top genre in each year")),
                    h4(tags$li("Dance pop is the most genres in every year, but the proportion is decreasing in recent year.")),
                    h4(tags$li("Latin, metro rap, rap are popular in recent year.")),
                    br(),
                    plotlyOutput("plot2", height = "550px"),
                    br(),
                    plotlyOutput("plot3",  height = "500px")
                    ),
          #########################################################################page4 
          tabItem(tabName='page4',
                  fluidRow(
                    column(2,
                           h2('Artist Statistics'),
                           
                           selectInput(
                             inputId = 'p4artist',
                             label='Artist',
                             choices = sort(unique(main2$artist)),
                             selected = 'Adele'
                           ),
                           selectInput(
                             inputId = 'p4stat1',
                             label='Statistics',
                             choices = c('year_released','bpm','nrgy','dnce','dB','val','acous ','spch','pop','top_year'),
                             selected = ''
                           )),
                    column(4,
                           br(),
                           br(),
                           br(),
                           br(),
                           plotOutput('p4pt1')),
                    column(2,
                           h2('Artist Comparison'),
                           selectInput(
                             inputId = 'p4artist2',
                             label='Artist 2',
                             choices = sort(main2$artist),
                             selected = ""
                           ),
                           selectInput(
                             inputId = 'p4stat12',
                             label='Statistics',
                             choices = c('year_released','bpm','nrgy','dnce','dB','val','acous ','spch','pop','top_year')
                           )),
                    
                    column(4,
                           br(),
                           br(),
                           br(),
                           br(),
                           plotOutput('p4pt12'))
                  ),
                  fluidRow(
                    
                    column(2,
                           h2('Genre Statistics'),
                           selectInput(
                             inputId = 'p4genre',
                             label='Genre',
                             choices = sort(unique(main2$top_genre)),
                             selected = "dance pop"
                           ),
                           selectInput(
                             inputId = 'p4stat2',
                             label='Statistics',
                             choices = c('year_released','bpm','nrgy','dnce','dB','val','acous ','spch','pop','top_year'),
                             selected = ""
                           )),
                    column(4,
                           br(),
                           br(),
                           br(),
                           br(),
                           plotOutput('p4pt2')),
                    column(2,
                           h2('Genre comparison'),
                           selectInput(
                             inputId = 'p4genre2',
                             label='Genre 2',
                             choices = sort(main2$top_genre),
                             selected = ""
                           ),
                           selectInput(
                             inputId = 'p4stat22',
                             label='Statistics',
                             choices = c('year_released','bpm','nrgy','dnce','dB','val','acous ','spch','pop','top_year')
                           )),
                    column(4,
                           br(),
                           br(),
                           br(),
                           br(),
                           plotOutput('p4pt22'))
                  )
          ),
          #########################################################################page5 
          tabItem(tabName = "page5",
                  h3(tags$strong("Trend for genres in each year")),
                  fluidRow(
                    column(2,
                           selectInput(
                             inputId = "pg88",
                             label = h5("Select genre:"), 
                             choices = sort(unique(main$`top genre`)),
                             selected = "dance pop"
                           )
                    ),
                    column(10,
                           plotlyOutput("plot4", height = "550px")
                    )
                  ),
                  br()
                  
                  
          ),
          #########################################################################page6 
          #wordcloud
          tabItem(tabName = "page6",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("color", "Color Pallet:",
                                  choices = c("Dark2","Paired","Set1")),
                      radioButtons("method","Scale Method" ,c("Square"="square",
                                                              "Sqrt"="sqrt",
                                                              "None"="none")),
                      sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1,  max = 21,  value = 5)
                    ),
                    mainPanel(
                      plotOutput("plot6",height = 500,width = "100%"),
                      plotOutput("plot7",height = 500,width = "100%"),
                      br(),
                      br(),
                      a("Data Source",href="https://www.kaggle.com/datasets/muhmores/spotify-top-100-songs-of-20152019",target="_blank")
                    ),
                    fluid = TRUE
                  )
          )
          #########################################################################page7 
          #########################################################################page3 
        )
    )
)


server <- function(input, output, session) {

  
  #(1) By top year  
  output$myTable1 = renderDataTable({
    tops_topyear <- main %>%
      filter(`top year` == input$pg21)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_topyear, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  #(2) By top genre  
  output$myTable2 = renderDataTable({
    tops_topgenre <- main %>%
      filter(`top genre` == input$pg22)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_topgenre, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  #(3) By artist type  
  output$myTable3 = renderDataTable({
    tops_artisttype <- main %>%
      filter(`artist type` == input$pg23)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_artisttype, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })  
  
    output$plot1 = renderPlotly({
      ###top artist
      top_artist = main%>%
        group_by(`top year`)%>%
        count(artist)%>%
        mutate(prop=n/sum(n))%>%
        arrange(desc(prop))%>%
        slice(1:5)
      
      p = top_artist %>% ggplot(aes(as_factor(`top year`), prop, fill=artist))+
        geom_bar(stat='identity', color = 'white', show.legend = F)+
        geom_text(aes(label=paste(artist)), size=2.5, color='black',
                  position = position_stack(vjust = .5))+
        theme_gray()+
        labs(title='Hot artists in each year', y='Percent', x='Year')
      
      ggplotly(p)
})
    
    output$plot2 = renderPlotly({
      ###top genre
      top_genre<-main%>%
        group_by(`top year`)%>%
        count(`top genre`)%>%
        mutate(prop=n/sum(n))
      p1 = top_genre[order(top_genre$n, decreasing=TRUE)[1:50], ]%>%
        ggplot(aes(as_factor(`top year`), prop, fill=`top genre` ))+
        geom_bar(stat='identity',  color = 'white', show.legend = F)+
        geom_text(aes(label=paste(`top genre` )), size=2.5, color='black',
                  position = position_stack(vjust = .5))+
        theme_gray()+
        labs(title='Hot genre in each year', y='Percent', x='Year')
      ggplotly(p1)
        
    })
    
    output$plot3 = renderPlotly({
      p2 = main%>%
        group_by(`top year`)%>%
        count(`top genre`)%>%
        filter(`top genre` == 'dance pop')%>%
        ggplot(aes(as_factor(`top year`), n))+
        geom_point(color='blue')+
        geom_line(group=1, color='blue')+
        theme_gray()+
        labs(title='Number of Dance pop in each year', y='Number of Dance pop', x='Year')
      ggplotly(p2)
    })
    output$plot4 = renderPlotly({
      p4=main%>%
        group_by(`top year`)%>%
        count(`top genre`)%>%
        filter(`top genre` == input$pg88)%>%
        ggplot(aes(as_factor(`top year`), n))+
        geom_bar(aes(as_factor(`top year`), n),stat="identity", fill="cyan",colour="#006000")+
        
        geom_point(color='red')+
        geom_line(group=1, color='red',size=3)+
        theme_bw()+
        labs(title=paste0('Number of ',input$pg88,' in each year'), y=paste0('Number of ',input$pg88), x='Year')
      ggplotly(p4)
    })
    output$plot6 = renderPlot({
      #creat artist and genre word dataframe
      main$artist <- tolower(main$artist)
      artists <- main%>%
        group_by(artist) %>%
        summarise(freq = n())
      
      # set method and freq
      cal1=switch(input$method,
                  square=(artists$freq)^2,
                  sqrt=sqrt(artists$freq),
                  none=artists$freq
      )
      
      frequency1=round(cal1,0)
      
      set.seed(10)
      wordcloud(words = artists$artist, 
                freq = frequency1,
                min.freq = 2,
                max.words=input$max,
                colors=brewer.pal(8, input$color),
                random.order=F)
    })
    
    
    
    output$plot7 = renderPlot({
      main$`top genre`<- tolower(main$`top genre`)
      Genre<- main%>%
        group_by(`top genre`) %>%
        summarise(freq = n())
      cal2=switch(input$method,
                  square=(Genre$freq)^2,
                  sqrt=sqrt(Genre$freq),
                  none=Genre$freq
      )
      frequency2=round(cal2,0)
      set.seed(11)
      wordcloud(words = Genre$`top genre`,
                freq = frequency2,
                min.freq = 2,
                max.words=input$max,
                colors=brewer.pal(8, input$color),
                scale = c(3,0.5),
                random.order=F)
    })
    
    output$p4pt1 = renderPlot({
      if(input$p4stat1 %in% c('top_genre','year_released','top_year')){
        stat= main2%>%
          filter(artist==input$p4artist)%>%
          group_by(!!sym(input$p4stat1),artist)%>%
          summarise(Count=n())
        ggplot(data=stat,mapping=aes_string(x=input$p4stat1,y=stat$Count))+
          geom_bar(stat='identity')+
          ylab('Count')+
          theme_classic()
      }
      else{
        stat= main2%>%
          filter(artist==input$p4artist)
        ggplot(data=stat,mapping=aes_string(x=input$p4stat1))+
          geom_histogram()+
          ylab('Count')+
          theme_classic()
      }
    })
    
    output$p4pt12 = renderPlot({
      if(input$p4stat12 %in% c('top_genre','year_released','top_year')){
        stat= main2%>%
          filter(artist==input$p4artist2)%>%
          group_by(!!sym(input$p4stat12),artist)%>%
          summarise(Count=n())
        ggplot(data=stat,mapping=aes_string(x=input$p4stat12,y=stat$Count))+
          geom_bar(stat='identity')+
          ylab('Count')+
          theme_classic()
      }
      else{
        stat= main2%>%
          filter(artist==input$p4artist2)
        ggplot(data=stat,mapping=aes_string(x=input$p4stat12))+
          geom_histogram()+
          ylab('Count')+
          theme_classic()
      }
    })
    
    output$p4pt2 = renderPlot({
      if(input$p4stat2 %in% c('year_released','top_year')){
        stat= main2%>%
          filter(top_genre==input$p4genre)%>%
          group_by(!!sym(input$p4stat2),top_genre)%>%
          summarise(Count=n())
        ggplot(data=stat,mapping=aes_string(x=input$p4stat2,y=stat$Count))+
          geom_bar(stat='identity')+
          ylab('Count')+
          theme_classic()
      }
      else{
        stat= main2%>%
          filter(top_genre==input$p4genre)
        ggplot(data=stat,mapping=aes_string(x=input$p4stat2))+
          geom_histogram()+
          ylab('Count')+
          theme_classic()
      }
    })
    output$p4pt22 = renderPlot({
      if(input$p4stat22 %in% c('year_released','top_year')){
        stat= main2%>%
          filter(top_genre==input$p4genre2)%>%
          group_by(!!sym(input$p4stat22),top_genre)%>%
          summarise(Count=n())
        ggplot(data=stat,mapping=aes_string(x=input$p4stat22,y=stat$Count))+
          geom_bar(stat='identity')+
          ylab('Count')+
          theme_classic()
      }
      else{
        stat= main2%>%
          filter(top_genre==input$p4genre2)
        ggplot(data=stat,mapping=aes_string(x=input$p4stat22))+
          geom_histogram()+
          ylab('Count')+
          theme_classic()
      }
    })
    

}

shinyApp(ui = ui, server = server)
