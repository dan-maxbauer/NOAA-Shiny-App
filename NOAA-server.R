library(shiny)
library(tidyverse)

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
