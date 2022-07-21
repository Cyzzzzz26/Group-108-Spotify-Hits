library(tidyverse)




h4(tags$li("By top year:")),
br(),
fluidRow(
  column(2,
         selectInput(
           inputId = "pg21",
           label = h4("Select top year:"), 
           choices = c("2010","2011", "2012","2013","2014","2015","2016","2017","2018","2019")
         )
  ),
  column(10,
         dataTableOutput("myTable1")
  )
),

output$myTable1 = renderDataTable({
  #(1) By top year
  tops_topyear <- main %>%
    filter(top_year == input$pg21)%>%
    group_by(top_year)%>%
    slice_max(order_by = artist)
  
  #tops_topyear = tops_topyear%>%
    #select("Platform", "Name", "Global_Sales")
  
  return(datatable(tops_topyear, 
                   options = list(lengthChange = FALSE),
                   rownames= FALSE))
})

main = read_csv("Spotify 2010 - 2019 Top 100.csv")

top_artist = main%>%
  filter(artist %in%  c("Taylor Swift", "Rihanna", "Ariana Grande", "Post Malone", "Drake"))%>%
  group_by(`top year`)%>%
  count(artist)
  
p = ggplot(top_artist, aes(x = `top year`, n, color=artist))+
  geom_line()+
  geom_point()
ggplotly(p)







top_artist = main%>%
  group_by(`top year`)%>%
  count(artist)%>%
  mutate(prop=n/sum(n))%>%
  arrange(desc(prop))%>%
  slice(1:5)

p = ggplot(top_artist, aes(as_factor(`top year`), prop, fill=artist))+
  geom_bar(stat='identity', color = 'white', show.legend = F)+
  geom_text(aes(label=paste(artist)), size=2.5, color='black',
            position = position_stack(vjust = .5))+
  theme_gray()+
  labs(title='Hot artists in each year', y='Percent', x='Year')
ggplotly(p)


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

  







