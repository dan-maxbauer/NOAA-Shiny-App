library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("NOAA Climate at a Glance ~ Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("parameters", h3("Parameters"),
                  choices = list("Avg Temperature"="tavg","Max Temperature"="tmax",
                                 "Min Temperature"="tmin","Precipitation"="pcp")),
      selectInput("scale", h3("Time Scale"),
                  choices = list("1-Month"=1,"2-Month"=2,"3-Month"=3,"4-Month"=4,"5-Month"=5,"6-Month"=6,
                                 "7-Month"=7,"8-Month"=8,"9-Month"=9,"10-Month"=10,"11-Month"=11,"12-Month"=12)),
      selectInput("month", h3("Month"), 
                  choices = list("January"=1,"February"=2,"March"=3,"April"=4,"May"=5,"June"=6,
                                 "July"=7,"August"=8,"September"=9,"October"=10,"November"=11,"December"=12)),
      h6("Please choose the last month in a Time Scale (ex. choose December and 12-Month for data for the entire calendar year)"),
      sliderInput("years", h3("Years"),
                  min = 1895,max = 2019,value = c(1980,2010)),
      selectInput("state", h3("State"),
                  choices = list("Alabama"=1,"Alaska"=50,"Arizona"=2,"Arkansas"=3,"California"=4,
                                 "Colorado"=5,"Connecticut"=6,"Delaware"=7,"Florida"=8,"Georgia"=9,
                                 "Hawaii"=51,"Idaho"=10,"Illinois"=11,"Indiana"=12,"Iowa"=13,
                                 "Kansas"=14,"Kentucky"=15,"Louisiana"=16,"Maine"=17,"Maryland"=18,
                                 "Massachusetts"=19,"Michigan"=20,"Minnesota"=21,"Mississippi"=22,"Missouri"=23,
                                 "Montana"=24,"Nebraska"=25,"Nevada"=26,"New Hampshire"=27,"New Jersey"=28,
                                 "New Mexico"=29,"New York"=30,"North Carolina"=31,"North Dakota"=32,"Ohio"=33,
                                 "Oklahoma"=34,"Oregon"=35,"Pennsylvania"=36,"Rhode Island"=37,"South Carolina"=38,
                                 "Souuth Dakota"=39,"Tennessee"=40,"Texas"=41,"Utah"=42,"Vermont"=43,
                                 "Virginia"=44,"Washington"=45,"West Virginia"=46,"Wisconsin"=47,"Wyoming"=48))
    ), 
    mainPanel(
      plotOutput("my_plot"),
      textOutput("my_mean"),
      textOutput("my_line"),
      h5("Copy and paste your dataset URL below"),
      textOutput("my_url")
    )
  )
) 

shinyApp(ui=ui, server=server)
