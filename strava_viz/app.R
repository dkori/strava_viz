#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(scales)
library(httr)
library(shinydashboard)
library(plotly)
library(dashboardthemes)
library(sf)
library(leaflet)
library(shinyjs)
# read in secret key from environment
readRenviron(".Renviron")
client_secret = Sys.getenv("CLIENT_SECRET")
# create a function that replaces nulls
replace_null<-function(x){
    return(ifelse(is.null(x),NA,x))
}

header <- dashboardHeader(title = "Enhanced Strava Visualization"
)
sidebar <- dashboardSidebar(disable=TRUE
    # sidebarMenu(id='tabs',
    #             menuItem('All runs overview',icon = icon("bar-chart"),tabName='all_runs'),
    #             menuItem('Single run focus',icon = icon("bar-chart"),tabName='run_focus'))
)

body<-dashboardBody(
    fluidRow(
        useShinyjs(),
        shinyDashboardThemes(theme = 'grey_dark'),
        tabBox(title=NULL,width=12,id='tabs',
               tabPanel('All Runs',class='active',
                       selectInput("series_choice", "Choose Color-code:",
                                   choices = c("Pace (min/mile)" = "moving_minutes_per_mile",
                                               "Cadence" = "average_cadence",
                                               #"Heartrate" = "average_heartrate",
                                               "Suffer Score" = "suffer_score"),
                                   selected = "Pace (min/mile)"),
                       box(
                           width=12,
                           plotOutput('runs_over_time')
                           
                       )
                       
                       # box(title='Runs over time',
                       #     #width=12,
                       #     #plotlyOutput('runs_over_time')
                       #     )
               ),
               tabPanel('Run Focus',
                        selectInput("run_choice","Choose Run:",
                                    choices=NULL,selected=NULL),
                        box(width=12,
                            plotOutput('single_run')
                            ),
                        box(width=12,
                            leafletOutput('map'))
               )
    
)))

# Define UI for application 
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # using the code passed in the query parameter, run the authentication query for the athlete
    response_content<-reactive({
            query <- parseQueryString(session$clientData$url_search)
            # store app client_id
            client_id = '78033'
            grant_type='authorization_code'
            # hardcoded authentication code
            #hardcode = 'cd298e828e6e441e06e0e4967a47551f1e91d1e2'
            post_auth_url = paste0('https://www.strava.com/oauth/token?client_id=',
                                   client_id,
                                   '&client_secret=',
                                   client_secret,
                                   '&code=',
                                   query[['code']],
                                   '&grant_type=',
                                   grant_type
            )
            response = POST(post_auth_url,encode='json')
            response_content = content(response)
            return(response_content)
    })
    # create a reactive that verifies that response_content worked, and an observer that redirects to authentication if it 
    # did not
    observeEvent(response_content,{
        if("access_token"%in%names(response_content())){
            #return(TRUE)
            print("authenticated")
        }else{
            shinyjs::runjs(paste0('window.location.href = "http://www.strava.com/oauth/authorize?client_id=78033&response_type=code&redirect_uri=https://dkori.shinyapps.io/strava_viz/&approval_prompt=force&scope=activity:read";'))
        }
    })
    # 
    # store athlete name
    athlete_name <- reactive({
        athlete_name<-paste(response_content()$athlete$firstname,
              response_content()$athlete$lastname,sep=' ')
        return(athlete_name)}
        
    )
    # store access token for subsequent calls
    access_token<-reactive({
        access_token<-response_content()[['access_token']]
        return(access_token)
    })
    # define authentication header for subsequent api calls
    auth_header<-reactive({
        return(auth_header = add_headers(Authorization=paste('Bearer',access_token(),
                                                             sep=' ')))
    })
    # get the activity list for the user
    activity_list<-reactive({
        before = as.integer(as.POSIXct(Sys.time()))
        after = as.integer(as.POSIXct(Sys.time()-(365*24*60*60)))
        # definie url for list activity api call
        list_activity_url = paste0('https://www.strava.com/api/v3/athlete/activities?before=',
                                   before,'&after=',after,'&page=1&per_page=100')
        # call api to list activity
        activity_list_response = GET(list_activity_url,auth_header())
        return(content(activity_list_response))
    })
    # create a dataframe out of the activity list
    runs_frame<-reactive({
        # define function that extracts needed info from activity
        extract_run = function(activity){
            if(activity[['type']]=='Run'){
                temp_df = data.frame('id' = activity[['id']],
                                     'name' = activity[['name']],
                                     'distance_km' = activity[['distance']]/1000,
                                     'moving_time' = replace_null(activity[['moving_time']]),
                                     'elapsed_time' = replace_null(activity[['elapsed_time']]),
                                     'total_elevation_gain' = activity[['total_elevation_gain']],
                                     'type' = activity[['type']],
                                     'date' = as.Date(activity[['start_date']]),
                                     'average_cadence' = replace_null(activity[['average_cadence']]),
                                     'average_speed' = activity[['average_speed']],
                                     'average_heartrate'=replace_null(activity[['average_heartrate']]),
                                     'suffer_score'=replace_null(activity[['suffer_score']])
                                     )
                # add heartrate if available
                if(activity[['has_heartrate']]==TRUE){
                    temp_df$average_heartrate<-activity[['average_heartrate']]
                }
                return(temp_df)
            }
        }
        #return(extract_run(activity_list()))
        return(lapply(activity_list(),
                      function(x) extract_run(x))%>%
                   bind_rows()%>%
                   mutate(distance_miles = distance_km/1.609344,
                          moving_minutes_per_mile = moving_time/60/distance_miles,
                          elapsed_minutes_per_mile = elapsed_time/60/distance_miles))
        # apply it over activity_list and bind into frame
        # return(bind_rows(
        #     lapply(activity_list(),extract_run))%>%
        #         mutate(distance_miles = distance_km/1.609344,
        #                moving_minutes_per_mile = moving_time/60/distance_miles,
        #                elapsed_minutes_per_mile = elapsed_time/60/distance_miles))
    })
    
    #output$runs_over_time<-reactive({paste(runs_frame()$name,collapse=', ')})
    # plot the user's runs over time
    series_choice<-reactive({paste0(input$series_choice)})
    output$runs_over_time<-renderPlot({
        # get the date range
        min_date<-min(runs_frame()[['date']],na.rm=TRUE)
        max_date<-max(runs_frame()[['date']],na.rm=TRUE)
        min_pace<-min(runs_frame()[['moving_minutes_per_mile']],na.rm=TRUE)
        max_pace<-max(runs_frame()[['moving_minutes_per_mile']],na.rm=TRUE)
        ot<-runs_frame()%>%
            filter(!is.na(moving_minutes_per_mile))%>%
            ggplot()+
            geom_point(aes_string(x="date",y="distance_miles",
                           color=input$series_choice) ,size=3 ,alpha=.9
                       )+
            #theme_minimal()+
            theme_gray()+
            theme(panel.background = element_rect(fill = "transparent",colour=NA),
                  plot.background = element_rect(fill = "transparent",colour=NA), # bg of the plot
                  panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                                  colour = "white"), # get rid of major grid
                  panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                                  colour = "white"),
                  plot.title = element_text(colour="white",size=18,face='bold'),
                  axis.text = element_text(colour="white"),
                  axis.title = element_text(colour="white"),
                  legend.text = element_text(colour='white'),
                  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                  legend.position='bottom',
                  legend.key.width = unit(2, 'cm'),
                  legend.title = element_text(colour="white", size=14))+
                scale_color_viridis_c()+
            labs(x='',y='distance (miles)',color=names(input$series_choice),
                 title =paste0(athlete_name(),': All Runs\n',
                               min_date,' to ',max_date))
        ot
        # ggplotly(ot)%>%
        #     layout(legend = list(
        #                          orientation='h',
        #                          x = 0.4, y = -0.2))
    },bg='transparent')
    # a vector of all run ids and that will be passed to the run selector
   run_list<-reactive({
       run_names<-runs_frame()%>%
           unite(runName,name, date,sep=' - ')
       run_ids<-runs_frame()[['id']]
       names(run_ids)<-run_names$runName
       return(run_ids)
       })
   # observer to populate the series_choice box for the athlete overview page
   observeEvent(runs_frame(),{
       series_options<-c("Pace (min/mile)" = "moving_minutes_per_mile",
                              "Cadence" = "average_cadence",
                              #"Heartrate" = "average_heartrate",
                              "Suffer Score" = "suffer_score")
       # add heartrate as an option to series options if the user has heartrate info for at least one run
       if('average_heartrate' %in% names(runs_frame())){
           series_options[['Average Heartrate']]<-"average_heartrate"
       }
       updateSelectInput(session,"series_choice",choices = series_options,selected="Pace (min/mile)")
                })
   # observer to update the input box choices to choose a run
   observeEvent(run_list,
                updateSelectInput(session, "run_choice", choices=run_list(),selected=names(run_list)[[1]])
       )
   
   # retrieve run stream data for selected run
   stream_frame<-eventReactive(input$run_choice,{
       get_stream = function(activity_id,stream_type){
           stream_url = paste0('https://www.strava.com/api/v3/activities/',
                               activity_id,
                               '/streams?keys=',
                               stream_type,'&key_by_type=true')
           GET(stream_url,auth_header())
       }
       # get the time stream
       time_stream = content(get_stream(input$run_choice,'time'))
       altitude_stream = content(get_stream(input$run_choice,'altitude'))
       coord_stream<- reactive({content(get_stream(input$run_choice,'latlng'))})
       
       #heart_stream = content(get_stream(input$run_choice,'heartrate'))
       # turn stream into dataframe
       stream_frame_temp <- data.frame('distance'=sapply(time_stream$distance$data,c),
                                  'time' = sapply(time_stream$time$data,c),
                                  'altitude'=sapply(altitude_stream$altitude$data,c))%>%
           # convert to miles, minutes, calculate diffs
           mutate(miles = distance/1609.344,
                  miles_round = round(miles,2),
                  distance_change = distance-lag(distance),
                  miles_change = miles-lag(miles),
                  minutes = time/60,2,
                  time_change = time-lag(time),
                  pace = (distance_change*0.0372823/time_change)^(-1),
                  altitude_change = altitude-lag(altitude),
                  altitude_per_time=altitude_change/time_change)%>%
           cbind(lapply(coord_stream()$latlng$data,
                        function(x) data.frame(lng=as.double(x[[2]]),lat=as.double(x[[1]])))%>%bind_rows())
           
       return(stream_frame_temp)
   })
   # plot the run
   output$single_run<-renderPlot({
       min_graph_pace=5
       max_graph_pace=15
       smoothing = 50
       sp<-stream_frame()%>%
           # convert to miles, minutes, calculate diffs
           mutate(miles = distance/1609.344,
                  miles_round = round(miles,2),
                  distance_change = distance-lag(distance),
                  miles_change = miles-lag(miles),
                  minutes = time/60,2,
                  time_change = time-lag(time),
                  pace = (distance_change*0.0372823/time_change)^(-1),
                  altitude_change = altitude-lag(altitude),
                  altitude_per_time=altitude_change/time_change)%>%
           filter(pace<15)%>%
           ggplot(aes(x=miles))+
           geom_point(aes(x=miles,y=pace,color=altitude_per_time),alpha=.15)+
           geom_line(aes(y=rollmean(pace,smoothing,na.pad=TRUE),color=rollmean(altitude_per_time,20,na.pad=TRUE),x=miles),size=2)+
           #theme_minimal()+
           scale_color_viridis_c(option='inferno')+
           scale_y_continuous(limits=c(min_graph_pace,max_graph_pace*1.1),oob = rescale_none)+
           theme_gray()+
           theme(panel.background = element_rect(fill = "transparent",colour=NA),
                 plot.background = element_rect(fill = "transparent",colour=NA), # bg of the plot
                 panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                                 colour = "white"), # get rid of major grid
                 panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                                 colour = "white"),
                 plot.title = element_text(colour="white",size=18,face='bold'),
                 axis.text = element_text(colour="white"),
                 axis.title = element_text(colour="white"),
                 legend.text = element_text(colour='white'),
                 legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                 legend.position='bottom',
                 legend.key.width = unit(2, 'cm'),
                 legend.title = element_text(colour="white"))+
           labs(x = 'mile',y='pace\n(min/mile)',color='vertical\nclimb (m/s)')
       sp
   },bg='transparent')
   # create the sf version of the run streams
   # create an observer for the leaflet map
   # get coordinates from api
   # # make geom_point dataframe
   run_sf<-eventReactive(input$run_choice,{
       stream_frame()%>%
           filter(pace<20)%>%
           mutate(altitude_roll=rollmean(altitude_change,50,na.pad=TRUE),
                  pace_roll=rollmean(pace,50,na.pad=TRUE)
                  )%>%
           st_as_sf(coords=c('lng','lat'))})
   output$map<-renderLeaflet({
       min_stream<-min(run_sf()[['pace_roll']],na.rm=TRUE)
       max_stream<-max(run_sf()[['pace_roll']],na.rm=TRUE)
       #min_stream<-5
       #max_stream<-15
       pal<-colorNumeric("viridis",domain = c(min_stream,max_stream))
       leaflet()%>%
           clearGroup('run')%>%
           addProviderTiles(providers$CartoDB.Positron)%>%
           addCircleMarkers(data=run_sf(),
                            radius=.001,
                            #fillOpacity = 0.1,
                            opacity=.5,
                            color = ~pal(pace_roll),
                            group='run')%>%
           clearGroup('legend')%>%
           addLegend("bottomleft",
                     pal = pal,
                     values = c(min_stream,max_stream),
                     title='pace</br>(min/mile)',
                     opacity = 1,
                     group="legend")
   })


    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
