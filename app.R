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

server <- function(input, output) {
    NOAA_states <- as.data.frame(state.name)
    NOAA_states <- as.data.frame(NOAA_states[-c(2,11),])
    colnames(NOAA_states) <- "States"
    NOAA_states <- rbind(NOAA_states,data.frame("States" = c("NULL","Alaska","Hawaii")))
  
  my_csv <- reactive({
    my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                    input$state,"-",input$parameters,"-",input$scale,"-",
                    input$month,"-",input$years[1],"-",input$years[2],
                    ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    csv_data <- readr::read_csv(my_url,skip=4)
    csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
    return(csv_data)
    })
  my_label <- renderText({
    if(input$parameters=="tavg") {"Average Temperature (°F)"}
    else if (input$parameters=="tmax") {print("Maximum Temperature (°F)")}
    else if (input$parameters=="tmin") {print("Minimum Temperature (°F)")}
    else {print("Precipitation (inches)")}
  })
  my_scale <- renderText({
    month_num <- as.numeric(input$month)
    scale_num <- as.numeric(input$scale)
    if ((month_num-scale_num+1)<1){num <- month_num-scale_num+13}
    else {num <- month_num-scale_num+1}
    scale_name <- as.character(month.name[num])
    return(scale_name)
  })
  my_month <- renderText({
    num <- as.numeric(input$month)
    month_name <- as.character(month.name[num])
    return(month_name)
  })
  my_state <- renderText({
    num <- as.numeric(input$state)
    state_name <- as.character(NOAA_states[num,])
    return(state_name)
  }) ##issues with being a character
  
  output$my_plot <- renderPlot({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_scale <- as.character(my_scale())
    plot_month <- as.character(my_month())
    plot_state <- as.character(my_state())
    ggplot(data=plot_csv, aes(x=Year,y=Value)) + geom_line() + geom_smooth(method="lm", se=FALSE) + theme_classic() +
      xlab("Years") + ylab(plot_label) + geom_hline(yintercept=mean(plot_csv$Value), color= "grey", lty=2)+
      ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))
  })
  output$my_mean <- renderText({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)
  })
  output$my_line <- renderText({
    plot_csv <- my_csv()
    lm <- lm(Value ~ Year, data = plot_csv)
     summary_lm <- summary(lm)
     slope <- round(summary_lm$coefficients[2],3)
     intercept <- round(summary_lm$coefficients[1],2)
     paste("Value =",intercept, "+", slope,"*Year",sep="")
    #return(intercept)
  })
  output$my_url <- renderText({
    plot_formula <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                          input$state,"-",input$parameters,"-",input$scale,"-",
                          input$month,"-",input$years[1],"-",input$years[2],
                          ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    return(plot_formula)
  })
}

shinyApp(ui=ui, server=server)

