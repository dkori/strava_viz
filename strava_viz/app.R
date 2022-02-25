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
# source the horizontal legend function
source('horizontal_legend.R')
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
                                               "Average Heartrate" = "average_heartrate",
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
                        selectInput("series_choice2", "Choose Color-code:",
                                    choices = c("Pace (min/mile)" = "pace",
                                                "Cadence" = "cadence",
                                                "Heartrate" = "heartrate",
                                                "Vertical climb (m/s)" = "climb"),
                                    selected = "climb"
                                    ),
                        box(width=12,
                            plotOutput('single_run')
                            ),
                        box(width=12,
                            leafletOutput('map'),
                            absolutePanel(
                                bottom = 0, left = 0, width="225px",
                                uiOutput("map_legend")
                            )
                        )
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
            response_content_temp = content(response)
            return(response_content_temp)
    })
    # create a reactive that's true if response_content contains an access token, all other functions should be conditional on this working
    token_received<-eventReactive(response_content,{
        if("access_token"%in%names(response_content())){
            return(TRUE)
            #print("authenticated")
        }else{
            return(FALSE)
        }
    })
    # redirect to authentication if the bearer token fails
    observeEvent(c(response_content,token_received),{
        if(token_received()){
            #return(TRUE)
            print("authenticated")
        }else{
            # define the redirect url based on if its running locally or not
            if(reactiveValuesToList(session$clientData)[['url_hostname']]=='127.0.0.1'){
                redirect_url<-paste0('http://127.0.0.1:',reactiveValuesToList(session$clientData)[['url_port']])
            }else{
                redirect_url<-'https://dkori.shinyapps.io/strava_viz/'
            }
            shinyjs::runjs(paste0('window.location.href = "http://www.strava.com/oauth/authorize?client_id=78033&response_type=code&redirect_uri=',redirect_url,'&approval_prompt=force&scope=activity:read";'))
        }
    })
    # 
    # store athlete name - reacts to response_content()
    athlete_name <- reactive({
        if(token_received()){
            athlete_name<-paste(response_content()$athlete$firstname,
                                response_content()$athlete$lastname,sep=' ')
            return(athlete_name)}
        }
    )
    # store access token for subsequent calls - reacts to response_content
    access_token<-reactive({
        if(token_received()){
            access_token<-response_content()[['access_token']]
            return(access_token)
        }
    })
    # define authentication header for subsequent api calls - reacts to access_token()
    auth_header<-reactive({
        if(token_received()){
            return(add_headers(Authorization=paste('Bearer',access_token(),
                                                   sep=' ')))
        }
    })
    # get the activity list for the user
    activity_list<-reactive({
        if(token_received()){
            before = as.integer(as.POSIXct(Sys.time()))
            after = as.integer(as.POSIXct(Sys.time()-(365*24*60*60)))
            # definie url for list activity api call
            list_activity_url = paste0('https://www.strava.com/api/v3/athlete/activities?before=',
                                       before,'&after=',after,'&page=1&per_page=100')
            # call api to list activity
            activity_list_response = GET(list_activity_url,auth_header())
            return(content(activity_list_response))
        }
    })
    # create a dataframe out of the activity list
    runs_frame<-reactive({
        if(token_received()){
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
        }
    })
    
    #output$runs_over_time<-reactive({paste(runs_frame()$name,collapse=', ')})
    # plot the user's runs over time
    series_choice<-reactive({paste0(input$series_choice)})
    pal_dict<-c("moving_minutes_per_mile" = "viridis",
                "suffer_score"="cividis",
                "average_cadence"="plasma",
                "average_heartrate"="magma")
    output$runs_over_time<-renderPlot({
        if(token_received()){
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
                scale_color_viridis_c(option=pal_dict[[input$series_choice]])+
                labs(x='',y='distance (miles)',color=names(input$series_choice),
                     title =paste0(athlete_name(),': All Runs\n',
                                   min_date,' to ',max_date))
            ot
            # ggplotly(ot)%>%
            #     layout(legend = list(
            #                          orientation='h',
            #                          x = 0.4, y = -0.2))
        }
    },bg='transparent')
    # a vector of all run ids and that will be passed to the run selector
   run_list<-eventReactive(runs_frame,{
       if(token_received()){
           run_names<-runs_frame()%>%
               # create a miles field with the word "miles" in it
               rowwise()%>%
               mutate(miles_str = paste0(round(distance_miles,2),' miles'))%>%
               unite(runName,name, miles_str,date,sep=' - ')
           run_ids<-runs_frame()[['id']]
           names(run_ids)<-run_names$runName
           return(run_ids)
       }
       })
   # observer to populate the series_choice box for the athlete overview page
   observeEvent(runs_frame,{
           # add heartrate as an option to series options if the user has heartrate info for at least one run
           if(length(runs_frame()[['average_heartrate']])<2){
               print(names(runs_frame))
               series_options<-c("Pace (min/mile)" = "moving_minutes_per_mile",
                                 "Cadence" = "average_cadence",
                                 #"Heartrate" = "average_heartrate",
                                 "Suffer Score" = "suffer_score")
               updateSelectInput(session,"series_choice",choices = series_options,selected="Pace (min/mile)")
       }})

   
   # observer to update the input box choices to choose a run
   observeEvent(run_list,
                updateSelectInput(session, "run_choice", choices=run_list(),selected=names(run_list)[[1]])

       )
   
   # retrieve run stream data for selected run
   stream_frame<-eventReactive(input$run_choice,{
       if(token_received()){
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
           coord_stream<- content(get_stream(input$run_choice,'latlng'))
           heart_stream<- content(get_stream(input$run_choice,'heartrate'))
           cadence_stream<- content(get_stream(input$run_choice,'cadence'))
           #heart_stream = content(get_stream(input$run_choice,'heartrate'))
           # turn stream into dataframe
           stream_frame_temp <- data.frame('distance'=sapply(time_stream$distance$data,c),
                                           'time' = sapply(time_stream$time$data,c),
                                           'altitude' = sapply(altitude_stream$altitude$data,c),
                                           'cadence' = sapply(cadence_stream$cadence$data,c))%>%
               # # convert to miles, minutes, calculate diffs
               mutate(miles = distance/1609.344,
                      miles_round = round(miles,2),
                      distance_change = distance-lag(distance),
                      miles_change = miles-lag(miles),
                      minutes = time/60,2,
                      time_change = time-lag(time),
                      pace = (distance_change*0.0372823/time_change)^(-1),
                      altitude_change = altitude-lag(altitude),
                      climb=altitude_change/time_change)%>%
               cbind(lapply(coord_stream$latlng$data,
                            function(x) data.frame(lng=as.double(x[[2]]),lat=as.double(x[[1]])))%>%bind_rows())
           # if heartrate data is available for the run, 
           if(heart_stream$heartrate$original_size==nrow(stream_frame_temp)){
               stream_frame_temp$heartrate<-sapply(heart_stream$heartrate$data,c)
           }
           return(stream_frame_temp)
       }
   })
   # create a reactive to stream_frame that rolls it for the given series choice
   stream_rolled<-reactive({
       # create a function that limits the frame to only rows in the time window, 
       timeroll<-function(i,df,roll_var,time_var='time', time_range=30){
           # extract the time for that value of the given i
           time<-df[i,][[time_var]]
           # limit df to just rows within time range
           temp_df<-df[df[[time_var]]>=time-time_range & df[[time_var]]<=time+time_range,]
           # return the change in variable over change in time
           return(weighted.mean(temp_df[[roll_var]],temp_df[['time_change']],na.rm=TRUE))
       }
       if(token_received()){
           sp<-stream_frame()%>%
               # convert to miles, minutes, calculate diffs
               # mutate(miles = distance/1609.344,
               #        miles_round = round(miles,2),
               #        distance_change = distance-lag(distance),
               #        miles_change = miles-lag(miles),
               #        minutes = time/60,2,
               #        time_change = time-lag(time),
               #        pace = (distance_change*0.0372823/time_change)^(-1),
               #        altitude_change = altitude-lag(altitude),
               #        climb=altitude_change/time_change)%>%
               filter(pace<15 & cadence>20)
           # create rolled variable based on series choice
           sp$pace_roll<-sapply(1:nrow(sp),function(x)timeroll(x,sp,"pace"))
           sp$rolled<-sapply(1:nrow(sp),function(x)timeroll(x,sp,input$series_choice2))
           sp$point_color<-sp[[input$series_choice2]]
           return(sp)
       }
   })
   # observer that removes heartrate from choices if its not in stream_frame
   # observeEvent(input$run_choice,{
   #     if(token_received()){
   #         heart_stream<- content(get_stream(input$run_choice,'heartrate'))
   #         # add heartrate as an option to series options if the user has heartrate info for at least one run
   #         # THIS SHOULD BE REACTING TO STREAM_FRAME, BUT IT CAUSES AN ERROR WHENEVER THAT HAPPENS. THE CURRENT IMPLEMENTATION WILL CAUSE PROBLEMS IF heart_stream original_size is >30 but less than nrow(stream_frame) once stream_frame is populated
   #         if(heart_stream$heartrate$original_size>30){
   #             print(names(runs_frame))
   #             series_options2<-c("Pace (min/mile)" = "pace",
   #                                "Cadence" = "cadence",
   #                                #"Heartrate" = "heartrate",
   #                                "Vertical climb (m/s)" = "climb")
   #             updateSelectInput(session,"series_choice2",choices = series_options2,selected="Pace (min/mile)")
   #         }
   #     }
   #     })
   # create a dictionary of color options based on the series chosen
   pal_dict2<-c("pace" = "viridis",
               "climb"="inferno",
               "cadence"="plasma","heartrate"="magma")
   #plot the run
   output$single_run<-renderPlot({
       # create a function that limits the frame to only rows in the time window, 
       # timeroll<-function(i,df,roll_var,time_var='time', time_range=30){
       #     # extract the time for that value of the given i
       #     time<-df[i,][[time_var]]
       #     # limit df to just rows within time range
       #     temp_df<-df[df[[time_var]]>=time-time_range & df[[time_var]]<=time+time_range,]
       #     # return the change in variable over change in time
       #     return(weighted.mean(temp_df[[roll_var]],temp_df[['time_change']],na.rm=TRUE))
       # }
       if(token_received()){
           min_graph_pace=5
           max_graph_pace=15
           # plot sp
           stream_rolled()%>%
               ggplot(aes(x=miles))+
               geom_line(aes(y=pace_roll,color=rolled,x=miles),size=2)+
               geom_point(aes(x=miles,y=pace,color=point_color),alpha=.15)+
               #theme_minimal()+
               scale_color_viridis_c(option=pal_dict2[[input$series_choice2]])+
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
               labs(x = 'mile',y='pace\n(min/mile)',color=gsub(' (?=\\()','\n',input$series_choice2,perl=TRUE))
           
           
           
       }
   },bg='transparent')
   # create the sf version of the run streams
   # create an observer for the leaflet map
   # get coordinates from api
   # # make geom_point dataframe
   # run_sf<-reactive({
   #     # create a function that limits the frame to only rows in the time window, 
   #     timeroll<-function(i,df,roll_var,time_var='time', time_range=30){
   #         # extract the time for that value of the given i
   #         time<-df[i,][[time_var]]
   #         # limit df to just rows within time range
   #         temp_df<-df[df[[time_var]]>=time-time_range & df[[time_var]]<=time+time_range,]
   #         # return the change in variable over change in time
   #         return(weighted.mean(temp_df[[roll_var]],temp_df[['time_change']],na.rm=TRUE))
   #     }
   #     if(token_received()){
   #         temp_frame<-stream_frame()%>%
   #             filter(pace<15 & cadence>20)%>%
   #             mutate(altitude_roll=rollmean(altitude_change,50,na.pad=TRUE),
   #                    pace_roll=rollmean(pace,50,na.pad=TRUE)
   #                    )
   #         # roll the selected choice variable
   #         temp_frame$rolled<-sapply(1:nrow(temp_frame),function(x)timeroll(i,temp_frame,input$series_choice2))
   #         temp_frame%>%
   #             st_as_sf(coords=c('lng','lat'))
   #             
   #     }
   #     })
   
   output$map<-renderLeaflet({
       timeroll<-function(i,df,roll_var,time_var='time', time_range=30){
           # extract the time for that value of the given i
           time<-df[i,][[time_var]]
           # limit df to just rows within time range
           temp_df<-df[df[[time_var]]>=time-time_range & df[[time_var]]<=time+time_range,]
           # return the change in variable over change in time
           return(weighted.mean(temp_df[[roll_var]],temp_df[['time_change']],na.rm=TRUE))
       }
       
       if(token_received()){
           sp<-stream_rolled()
           min_stream<-round(min(sp$rolled,na.rm=TRUE),2)
           max_stream<-round(max(sp$rolled,na.rm=TRUE),2)
           print(min_stream)
           print(max_stream)
           #min_stream<-5
           #max_stream<-15
           pal<-colorNumeric(pal_dict2[[input$series_choice2]],domain = c(min_stream,max_stream))
           # quick rounding function for popup
           roundif<-function(num){
               if(is.na(num)){
                   return(str(num))
               }else{
                   return(str(round(num)))
               }
           }
           leaflet()%>%
               clearGroup('run')%>%
               addProviderTiles(providers$CartoDB.Positron)%>%
               addCircleMarkers(data=sp%>%
                                    st_as_sf(coords=c('lng','lat')),
                                radius=.001,
                                #fillOpacity = 0.1,
                                opacity=.5,
                                color = ~pal(rolled),
                                group='run')%>%
               clearGroup('legend')%>%
               addLegend("bottomleft",
                         pal = pal,
                         values = c(min_stream,max_stream),
                         title=input$series_choice2,
                         opacity = 1,
                         group="legend")
           
           
       }
   })
   # at some point, figure out enough javascript to make the horizontal legend function work
   # output$map_legend<-renderUI({
   #     min_stream<-min(run_sf()[['pace_roll']],na.rm=TRUE)
   #     max_stream<-max(run_sf()[['pace_roll']],na.rm=TRUE)
   # 
   #     pal<-colorNumeric("viridis",domain = c(min_stream,max_stream))
   #     horizontal_legend(values=run_sf()[['pace_roll']],
   #                       palette='viridis',
   #                       title='pace</br>(min/mile)',
   #                       left_label=round(min_stream),
   #                       right_label=round(max_stream))
   # })



    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
