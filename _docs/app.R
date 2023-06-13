library(shiny)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(DT)
library(sf)
library(leaflet)
library(leaflet.extras2)


writings <- read_csv('https://raw.githubusercontent.com/wilfordwoodruff/Main-Data/main/data/derived/derived_data.csv') %>%
  mutate(`First Date` = ymd(ifelse(is.na(`First Date`),substr(Dates,0,10),`First Date`)))
  
url <- "https://docs.google.com/spreadsheets/d/1npLJA_nJtqokbSdLSIuhpshoFD3X7487wctXCoXRGIQ/edit#gid=0"
#drive_id <- drive_find(url)
#tell terminal to answer "2"

get_sheet <- function() {
  drive_get(url) %>%
    read_sheet() %>%
    mutate(StartDate = as.numeric(str_replace(unlist(StartDate),"\\'","")),
           EndDate = as.numeric(str_replace(unlist(EndDate),"\\'","")),
           TimeSent = as.numeric(str_replace(unlist(TimeSent),"\\'","")))
}
#get_sheet()
stories <- get_sheet() 

clara_compiled <- read_csv('https://raw.githubusercontent.com/wilfordwoodruff/clara_woodruff_timeline_map/main/data/final_data.csv')
clara_compiled$point <- st_geometry(st_as_sf(clara_compiled,coords = c("lng","lat")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
    # Application title
    titlePanel("Explore President Woodruff's Diaries"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId='chosen_preset',
                      label='See What Others have Found!',
                      choices=stories$Name),
            textAreaInput(inputId='chosen_description',
                          label='Why they liked it',
                          value = "If you choose a submitted fact, here's where they'll write why it's interesting!",
                          width='400px',height='100px'),            
            br(),
            dateRangeInput(inputId = "startEndDate",
                           label="Writing Period",
                           start='1801-03-01',
                           end='1898-09-08',
                           separator='Journals between',
                           min='1801-03-01',
                           max='1898-09-08'
                           ),
            checkboxGroupInput(inputId = 'journal_type',
                               label = 'Types of Writings',
                               choices = unique(writings$`Document Type`),
                               selected = unique(writings$`Document Type`)
              
            ),
            textInput(inputId='word_search',
                      label='Search for a Word',
                      placeholder= "eg. Utah"),
            br(),
            textInput(inputId='submit_name',
                      label='Name Your Discovery!'),
            textAreaInput(inputId='submit_description',
                      label='Describe What You Found',
                      width='400px',height='100px'),
            actionButton(inputId="saveStory",
                         label="Save Your Story"),
            bookmarkButton()
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("See Dataset",
             tableOutput("fiverows")
            ),
            tabPanel("See Graphs",
                     leafletOutput("claramap")
            ) 
          )
        )
    )
)

server <- function(input, output) {
  #Collect the values that will update over time
  user_filters <- reactiveValues()
  user_filters$submit <- 0
  user_filters$stories <- get_sheet()
  
  observe({
    #If they choose a preset, save the values connected to it
    if(input$chosen_preset!='Default') {
      selected <- stories %>%
        filter(Name==input$chosen_preset) %>%
        slice_head() #if duplicates, only return the 1st one
      user_filters$startdate <- ymd(as_datetime(selected$StartDate))
      user_filters$enddate <- ymd(as_datetime(selected$EndDate))
      user_filters$wordsearch <- selected$Words
      user_filters$journaltype <- str_split(selected$WritingType,',')
      user_filters$description <- selected$UserDescription
      #input$ducks <- input$ducks-1
    }
    #Otherwise, save whatever they've created
    else {
      user_filters$startdate=input$startEndDate[1]
      user_filters$enddate =input$startEndDate[2]
      user_filters$wordsearch = input$word_search
      user_filters$journaltype <- input$journal_type
      user_filters$description <- "If you choose a submitted fact, here's where they'll write why it's interesting!"
    }
    
    #saveStory counts up on each click, so when it's increased we 
    #run this and then increase user_filters$submit to match
    if(input$saveStory > user_filters$submit) {
      current_submission <- c(input$submit_name,
                              as.numeric(as.POSIXct(user_filters$startdate, format = "%Y-%m-%d")), #Start Day
                              as.numeric(as.POSIXct(user_filters$enddate, format = "%Y-%m-%d")), #End Day
                              input$word_search,
                              paste(user_filters$journaltype,collapse = ','),
                              as.numeric(as.POSIXct(today(), format = "%Y-%m-%d")),
                              input$submit_description)
      user_filters$stories <- rbind(get_sheet(),current_submission) 
      write_sheet(data=user_filters$stories,
                  ss=url,sheet='Main')
      user_filters$submit = user_filters$submit + 1
      stories <<- user_filters$stories
    }
    
    #If a pre-made filter gets chosen, update the sliders and check boxes
    updateTextInput(inputId = "word_search",
                    value=user_filters$wordsearch)
    updateDateRangeInput(inputId = "startEndDate",
                         start = user_filters$startdate,
                         end = user_filters$enddate)
    updateCheckboxGroupInput(inputId = "journal_type",
                             selected = user_filters$journaltype)
    updateTextAreaInput(inputId = "chosen_description",
                        value = user_filters$description)
    
    user_filters$filtered_writing <- writings %>%
      mutate(`Word Count`=str_count(`Text Only Transcript`,user_filters$wordsearch)) %>%
      filter(`Word Count` > 0 & `Document Type` %in% user_filters$journaltype &
               `First Date` > user_filters$startdate & `First Date` < user_filters$enddate)
    
    user_filters$map_data <- clara_compiled %>%
      filter(short_url %in% user_filters$filtered_writing$`Short URL`) %>%
      group_by(lat,lng) %>%
      summarise(url=first(search_url),
                count= n(),
                city=first(city),
                day=first(day),
                state=first(state_name))
      #add date filter
    user_filters$max_map_count <- max(user_filters$map_data$count)
    
    #Avoid a 
    if(user_filters$max_map_count < 24) {
      user_filters$map_bins <- c(1,2,4,8,23)
    }
    else if (user_filters$max_map_count < 100) {
      user_filters$map_bins <- c(1,2,5,10,50,99)
    }
    else {
    user_filters$map_bins <- c(1, 2, round(user_filters$max_map_count/8,0), 
                               round(user_filters$max_map_count/4,0), 
                               round(user_filters$max_map_count/2,0), user_filters$max_map_count)
    }
    user_filters$map_palette <- colorBin(palette="YlGnBu", domain=user_filters$map_data$count,
                                         na.color="transparent", bins=user_filters$map_bins)
    
  })
  output$fiverows <- renderTable({
      head(user_filters$filtered_writing)
    })
  output$claramap <- renderLeaflet({
    # creating leaflet graph
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      setView(-98.5795, 39.8283, zoom = 3) %>%
      addCircles(data=user_filters$map_data, 
                 lng = user_filters$map_data$lng, 
                 lat = user_filters$map_data$lat, 
                 color = ~user_filters$map_palette(user_filters$map_data$count), 
                 popup = ~paste("<b>", "<a href=", user_filters$map_data$url, ">", user_filters$map_data$city, 
                                "</a>", ",", "</b>", user_filters$map_data$state, "<br>Number of Mentions:",
                                user_filters$map_data$count)) %>%
      addLegend(data=user_filters$map_data, pal=user_filters$map_palette, values=user_filters$map_data$count, opacity=0.9, title = "Mentions", 
                position = "bottomleft")
    
  })
  
  #Plots--------------------
'  output$sentiment_graph <- renderGraph({
    #ggplot stuff here, 
  })
  output$location_graph <- renderGraph({
    
  })
  output$frequency_graph <- renderGraph({
    
  })'
  
}

"
Main pieces to filter by:
user_filters$startdate
            $enddate
            $word_search
            $journal_type
"


"
output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white',
       xlab = 'Waiting time to next eruption (in mins)',
       main = 'Histogram of waiting times')
})
"
# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")

