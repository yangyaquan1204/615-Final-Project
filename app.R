library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
library(scales)
library(dplyr)
library(fontawesome)
#library(shinythemes)

load('LRmini.RData')
load('bus.RData')




##############
weekdays.name<-strsplit('Sun,Mon,Tue,Wed,Thu,Fri,Sat',',')[[1]]
dat[,weekday:=wday(service_date)]
dat[,weekDay:=weekdays.name[as.integer(weekday)]]
dat[,weekDay:=ordered(weekDay,weekdays.name)]
dat[,startHour:=hour(startTime)]
##############
tra<-dat[,.(Traffic=.N),.(from_stop_id,to_stop_id)]
tra[,from_stop_id:=as.character(from_stop_id)]
tra[,to_stop_id:=as.character(to_stop_id)]
to.anno<-sites[,.(to_stop_id=stop_id,to_stop_lat=stop_lat, to_stop_lon=stop_lon,to_stop_name=stop_name)]
tra<-to.anno[tra,on=.(to_stop_id)]
from.anno<-sites[,.(from_stop_id=stop_id,from_stop_lat=stop_lat,  from_stop_lon=stop_lon,from_stop_name=stop_name)]
tra<-from.anno[tra,on=.(from_stop_id)]
tra<-tra[order(-Traffic)]
tra<-na.omit(tra)
##############

basemap<-leaflet() %>%
     addTiles() %>% 
     setView(lng = mean(sites$stop_lon,na.rm=T), lat = mean(sites$stop_lat,na.rm=T), zoom = 13)
##############

##############
bus<-na.omit(bus)
bus[,scheduledHour:=as.character(hour(scheduled))]
bus[,scheduledHour:=ordered(scheduledHour,0:23)]
bus[,type:=fcase(timeDiff==0,'Intime',timeDiff>0,'Delay',timeDiff<0,'Advance')]


basemap2<-leaflet() %>%
          addTiles() %>% 
          setView(lng = mean(sites$stop_lon,na.rm=T), lat = mean(sites$stop_lat,na.rm=T), zoom = 13)
##############




ui<-navbarPage("MBTA", collapsible = TRUE, inverse = TRUE, 


tabPanel(
title=tags$h2('Subway'),
   
   fluidRow(
   column(width=4,offset=1,selectInput('metroFrom','STOP from',choices=tra[,unique(from_stop_name)])),
   column(width=4,uiOutput('metroTosel'))
   ##column(width=4,verbatimTextOutput('check'))
   ),
   
   
   
   fluidRow(
   column(width=5,offset=1,leafletOutput('mapout')),
   column(width=6,plotlyOutput('metroPairplot0'))
   ),
   
   fluidRow(
   column(width=5,offset=1,plotlyOutput('metroPairplot1')),
   column(width=6,plotlyOutput('metroPairplot2'))
   )
),  ###end tabPanel


tabPanel(
title=tags$h2('Bus'),

fluidRow(
column(width=4,offset=1,selectInput('busroutine','Bus Routine',choices=bus[,unique(route_id)])),
column(width=4,uiOutput('busstopSel'))
##column(width=4,verbatimTextOutput('check'))
),



fluidRow(
column(width=10,offset=1,leafletOutput('mapout2'))
),

fluidRow(style='height:10px'),


fluidRow(
column(width=4,offset=1,plotOutput('busplot0')),
column(width=6,plotOutput('busplot1'))
),

fluidRow(
column(width=10,offset=1,plotOutput('busplot2'))
)
)


)



server<-function(input,output,session){


output$mapout<-renderLeaflet({
basemap
})


observe({
req(input$metroFrom)
req(input$metroTo)
getmetroLinedata()->inter.data
lines<-inter.data[[1]]
unique(lines$route_id)[1]->linetype
color.used<-switch(linetype,'Blue'='blue','Red'='red','Green-B'='green')

inter.from<-unique(tra[from_stop_name==input$metroFrom,.(long=from_stop_lon,lat=from_stop_lat)])[1,]
inter.to<-unique(tra[to_stop_name==input$metroTo,.(long=to_stop_lon,lat=to_stop_lat)])[1,]
inter.line<-rbind(inter.from,inter.to)

    leafletProxy("mapout") %>%
      clearMarkers() %>%
	  clearShapes() %>% 
      addMarkers(~long, ~lat, data=inter.from, popup = paste('From:',input$metroFrom)) %>%
	  addMarkers(~long, ~lat, data=inter.to, popup = paste('To:',input$metroTo)) %>%
	  addPolylines(~long, ~lat, data=inter.line,color=color.used) %>%
	  flyTo(lng=inter.from$long,lat=inter.from$lat,zoom=13)

})

output$metroTosel<-renderUI({
can.tos<-tra[from_stop_name==input$metroFrom,unique(to_stop_name)]
selectInput('metroTo','STOP to',choices=can.tos)
})


output$check<-renderPrint({
inter.from<-unique(tra[from_stop_name==input$metroFrom,.(long=from_stop_lon,lat=from_stop_lat)])[1,]
inter.from
})


getmetroLinedata<-reactive({
req(input$metroFrom)
req(input$metroTo)
   lines<-dat[from_stop_name==input$metroFrom & to_stop_name==input$metroTo]
   unique(lines$route_id)->linetype
   linetype.color<-setNames(c('#1E90FF','#E55451','#4CC552'),c('Blue','Red','Green-B'))
   color.used<-linetype.color[linetype][1]
   list(lines,color.used)
})





output$metroPairplot0<-renderPlotly({
  inter.data<-getmetroLinedata()
  lines<-inter.data[[1]]
  color.used<-inter.data[[2]]
  
  ggplot(lines,aes(y=travel_time_sec,x=paste(from_stop_name,to_stop_name,sep=" -> ")))+
    geom_boxplot(colour=color.used,fill=alpha(color.used,0.3),lwd=0.3)+
    theme_minimal()+
    labs(x='')
})

output$metroPairplot1<-renderPlotly({
  inter.data<-getmetroLinedata()
  lines<-inter.data[[1]]
  color.used<-inter.data[[2]]
  
p1<-lines[,mean(travel_time_sec),.(weekDay)]
setnames(p1,'V1','Mean Time Required')

plot1<-ggplot(p1,aes(x=weekDay,y=`Mean Time Required`))+
  geom_line(group='aa',colour=alpha(color.used,0.3))+
  geom_point(colour=color.used)+
  theme_minimal()+
  labs(y='Mean Time Required (seconds)')

ggplotly(plot1)
})

output$metroPairplot2<-renderPlotly({
  inter.data<-getmetroLinedata()
  lines<-inter.data[[1]]
  color.used<-inter.data[[2]]
  
p1<-lines[,mean(travel_time_sec),.(startHour)]
setnames(p1,'V1','Mean Time Required')

plot1<-ggplot(p1,aes(x=startHour,y=`Mean Time Required`))+
  geom_line(group='aa',colour=alpha(color.used,0.3))+
  geom_point(colour=color.used)+
  theme_minimal()+
  labs(y='Mean Time Required (seconds)')+
  scale_x_continuous(breaks=0:23)


ggplotly(plot1)
})

####lositic for bus
output$mapout2<-renderLeaflet({
basemap2
})

#
observe({
req(input$busroutine)
req(input$bustop)
busgetdata()->inter.data
inter.data[[2]]->inter.site


    leafletProxy("mapout2") %>%
      clearMarkers() %>%
	  clearShapes() %>% 
      addMarkers(~long, ~lat, data=inter.site, popup = paste('Bus STOP:',input$bustop)) %>%
	  flyTo(lng=inter.site$long,lat=inter.site$lat,zoom=13)
})

output$busstopSel<-renderUI({
busstops<-bus[route_id==input$busroutine,unique(stop_name)]
selectInput('bustop','BUS STOP',choices=busstops)
})


busgetdata<-reactive({
req(input$busroutine)
req(input$bustop)

inter.bus<-bus[route_id==input$busroutine & stop_name==input$bustop]
inter.bus.stop<-inter.bus[1,stop_id]
inter.site<-sites[stop_id==inter.bus.stop,.(stop_id,stop_name,long=stop_lon,lat=stop_lat)]
list(inter.bus,inter.site)
})


output$busplot0<-renderPlot({
busgetdata()->inter.data
inter.data[[1]]->inter.bus

ggplot(inter.bus,aes(y=timeDiff,x=stop_name))+
geom_violin(colour='#3B3131',fill=alpha('#3B3131',0.3))+
theme_minimal()+
labs(x='')
})


output$busplot1<-renderPlot({
busgetdata()->inter.data
inter.data[[1]]->inter.bus
ggplot(inter.bus,aes(x=weekDay,y=timeDiff,colour=weekDay,fill = after_scale(alpha(colour, 0.4))))+
   geom_boxplot()+
   theme_bw()+
   scale_colour_hue()+
   theme(legend.position='none')
})


output$busplot2<-renderPlot({
busgetdata()->inter.data
inter.data[[1]]->inter.bus
ggplot(inter.bus,aes(x=scheduledHour,y=timeDiff,colour=scheduledHour,fill = after_scale(alpha(colour, 0.4))))+
   geom_boxplot()+
   theme_bw()+
   scale_colour_hue()+
   theme(legend.position='none')
})

}


shinyApp(ui,server)





