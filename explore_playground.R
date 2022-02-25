library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(scales)
library(sf)
library(leaflet)
library(stats)
'http://www.strava.com/oauth/authorize?client_id=78033&response_type=code&redirect_uri=http://localhost/exchange_token&approval_prompt=force&scope=activity:read'
'http://www.strava.com/oauth/authorize?client_id=78033&response_type=code&redirect_uri=https://dkori.shinyapps.io/strava_viz/&approval_prompt=force&scope=activity:read'
'http://www.strava.com/oauth/authorize?client_id=78033&response_type=code&redirect_uri=http://127.0.0.1:3565&approval_prompt=force&scope=activity:read'
# readRenviron(".Renviron")
#client_secret = Sys.getenv("CLIENT_SECRET")
client_id='78033'
code='493b5b9250fd902164ec18028bdaa943f4e2141c'
grant_type='authorization_code'
post_auth_url = paste0('https://www.strava.com/oauth/token?client_id=',
                       client_id,
                       '&client_secret=',
                       client_secret,
                       '&code=',
                       code,
                       '&grant_type=',
                       grant_type
                       )
response = POST(post_auth_url,encode='json')
good_content = content(response)
response_content = content(response)
"access_token"%in%names(good_content)
"access_token"%in%names(response_content)
response_content

access_token = response_content[['access_token']]
athlete_name = paste(response_content$athlete$firstname,
                      response_content$athlete$lastname,sep=' ')
athlete_id = response_content[['athlete']][['id']]
before = as.integer(as.POSIXct('2022-02-20 12:00:00.000'))
after = as.integer(as.POSIXct('2021-02-20 12:00:00.000'))
list_activity_url = paste0('https://www.strava.com/api/v3/athlete/activities?before=',
                           before,'&after=',after,'&page=1&per_page=200')
auth_header = add_headers(Authorization=paste('Bearer',access_token,sep=' '))
activity_list_response = GET(list_activity_url,auth_header)

activity_list = content(activity_list_response)

activity = activity_list[[1]]
activity$id
activity[['has_heartrate']]==TRUE
# find the types of activities from acivity list
unique(sapply(activity_list,function(x) x$type))
# quick function to extract distance, moving_time, elapsed_time, total_elevation_gain, start_date
extract_activity = function(activity,activity_type){
  if(activity$type==activity_type){
    temp_df = data.frame('id' = activity$id,
                         'name' = activity$name,
                         'distance_km' = replace_null(activity[['distance']]/1000),
                         'moving_time' = replace_null(activity[['moving_time']]),
                         'elapsed_time' = replace_null(activity$elapsed_time),
                         'total_elevation_gain' = replace_null(activity$total_elevation_gain),
                         'type' = replace_null(activity$type),
                         'date' = replace_null(as.Date(activity$start_date)),
                         'average_cadence' = replace_null(activity[['average_cadence']]),
                         'average_speed' = replace_null(activity$average_speed))
    return(temp_df)
  }
}
filter_func<-function(x){
  if(x$type=='Ride'){
    return(x)
  }else
}
test<-lapply(activity_list,function(x)filter_func(x))
test<-test[sapply(test,function(x) !is.null(x))]
activity<-test[[1]]
extracted<-extract_activity(activity,'Ride')
replace_null<-function(x){
  return(ifelse(is.null(x),NA,x))
}
ifelse(is.null(activity[['average_cadence']]),NA,activity[['average_cadence']])
activity<-test[1]
run_frame = bind_rows(
                           lapply(activity_list,function(x)extract_activity(x,'Run')))%>%
  mutate(distance_miles = distance_km/1.609344,
         moving_minutes_per_mile = moving_time/60/distance_miles,
         elapsed_minutes_per_mile = elapsed_time/60/distance_miles)
bike_frame = bind_rows(
  lapply(activity_list,function(x)extract_activity(x,activity_type='Ride')))%>%
  mutate(distance_miles = distance_km/1.609344,
         moving_minutes_per_mile = moving_time/60/distance_miles,
         elapsed_minutes_per_mile = elapsed_time/60/distance_miles)
test<-lapply(activity_list,function(x)extract_activity(x,activity_type='Ride'))
# make a graph of runs over time with color showing pace
run_frame%>%
  ggplot(aes(x=date,y=distance_miles,color=moving_minutes_per_mile))+
    geom_point(size=3,alpha=.9)+
    scale_color_viridis_c()+
    theme_minimal()+
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'cm'))+
  labs(x='date',y='distance (miles)',color='average\npace')
# extract the id of the last activity
last_id = run_frame$id[[nrow(run_frame)]]
# create a function that gets the stream for a given activity_id and stream type
get_stream = function(activity_id,stream_type){
  stream_url = paste0('https://www.strava.com/api/v3/activities/',
                      last_id,
                      '/streams?keys=',
                      stream_type,'&key_by_type=true')
  GET(stream_url,auth_header)
}
# get the time stream
time_stream = content(get_stream(last_id,'time'))
altitude_stream = content(get_stream(last_id,'altitude'))
coord_stream<- content(get_stream(last_id,'latlng'))
heart_stream<-content(get_stream(last_id,'heartrate'))
test = cbind(t(time_stream$distance$data),
             t(time_stream$time$data))
# turn stream into dataframe
stream_frame <- data.frame('distance'=sapply(time_stream$distance$data,c),
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
    altitude_per_time=altitude_change/time_change)
# create a function that limits the frame to only rows in the time window, 
timeroll<-function(i,df,roll_var,time_var='time', time_range=10){
  # extract the time for that value of the given roll_var
  time<-df[i,][['time']]
  # limit df to just rows within time range
  temp_df<-df[df[[time_var]]>=time-time_range & df[[time_var]]<=time+time_range,]
  # return the change in variable over change in time
  return(weighted.mean(temp_df[[roll_var]],temp_df[['time_change']],na.rm=TRUE))
}
time<-stream_frame[1,][['distance']]
timeroll(1,stream_frame,'pace')
stream_frame$distance_roll<-sapply(1:nrow(stream_frame),
                                  function(x) timeroll(x,stream_frame,'distance_change'))
stream_frame$altitude_roll<-sapply(1:nrow(stream_frame),
                                  function(x) timeroll(x,stream_frame,'altitude_per_time'))
stream_frame$pace_roll<-sapply(1:nrow(stream_frame),
                                   function(x) timeroll(x,stream_frame,'pace'))


min_graph_pace=5
max_graph_pace=15
smoothing = 50
stream_frame%>%
  filter(pace<15)%>%
  ggplot(aes(x=miles))+
  geom_point(aes(x=miles,y=pace,color=altitude_per_time),alpha=.3)+
  geom_line(aes(y=pace_roll,color=altitude_per_time,x=miles),size=1)+
  theme_minimal()+
  scale_color_viridis_c(option='inferno')+
  scale_y_continuous(limits=c(min_graph_pace,max_graph_pace*1.1),oob = rescale_none)+
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'cm'))+
  labs(x = 'mile',y='min/mile',color='climb (m/s)')
# make sf frme of latest run
run_sf<-stream_frame%>%
  cbind(lapply(coord_stream$latlng$data,
               function(x) data.frame(lng=as.double(x[[2]]),lat=as.double(x[[1]])))%>%bind_rows())%>%
  filter(pace<20)%>%
  mutate(altitude_roll=rollmean(altitude_change,50,na.pad=TRUE),
         pace_roll=rollmean(pace,50,na.pad=TRUE)
         )%>%
  st_as_sf(coords=c('lng','lat'))
min_stream<-min(run_sf$pace_roll,na.rm=TRUE)
max_stream<-max(run_sf$pace_roll,na.rm=TRUE)
pal<-colorNumeric("viridis",domain = c(min_stream,max_stream))
leaflet()%>%
  #setView(-79.9250, 40.4350,zoom=13.35)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addCircleMarkers(data=run_sf,
                   radius=.001,
                   #fillOpacity = 0.1,
                   opacity=.5,
                   color = ~pal(pace_roll)
  )
