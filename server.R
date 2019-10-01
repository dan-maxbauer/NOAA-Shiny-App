library(shiny)
library(tidyverse)

server <- function(input, output) {
  NOAA_states <- as.data.frame(state.name)
  NOAA_states <- as.data.frame(NOAA_states[-c(2,11),])
  colnames(NOAA_states) <- "States"
  NOAA_states <- rbind(NOAA_states,data.frame("States" = c("NULL","Alaska","Hawaii")))

  ##inputs
  #graph1
  my_csv <- reactive({
    my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                    input$state,"-",input$parameters,"-",input$scale,"-",
                    input$month,"-",input$years[1],"-",input$years[2],
                    ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    if(input$parameters=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
    else {csv_data <- readr::read_csv(my_url,skip=4)}
    csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
    return(csv_data)
  })
  my_label <- renderText({
    if(input$parameters=="tavg") {"Average Temperature (°F)"}
    else if (input$parameters=="tmax") {print("Maximum Temperature (°F)")}
    else if (input$parameters=="tmin") {print("Minimum Temperature (°F)")}
    else if (input$parameters=="pcp"){print("Precipitation (inches)")}
    else if (input$parameters=="cdd"){print("Cooling Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters=="hdd"){print("Heating Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters=="pdsi"){print("PDSI")}
    else if (input$parameters=="phdi"){print("PHDI")}
    else if (input$parameters=="pmdi"){print("PMDI")}
    else {print("Palmer Z-Index")}
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
  })
  #graph2
  my_csv2 <- reactive({
    my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                    input$state2,"-",input$parameters2,"-",input$scale2,"-",
                    input$month2,"-",input$years2[1],"-",input$years2[2],
                    ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    if(input$parameters2=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters2=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters2=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters2=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
    else {csv_data <- readr::read_csv(my_url,skip=4)}
    csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
    return(csv_data)
  })
  my_label2 <- renderText({
    if(input$parameters2=="tavg") {"Average Temperature (°F)"}
    else if (input$parameters2=="tmax") {print("Maximum Temperature (°F)")}
    else if (input$parameters2=="tmin") {print("Minimum Temperature (°F)")}
    else if (input$parameters2=="pcp"){print("Precipitation (inches)")}
    else if (input$parameters2=="cdd"){print("Cooling Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters2=="hdd"){print("Heating Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters2=="pdsi"){print("PDSI")}
    else if (input$parameters2=="phdi"){print("PHDI")}
    else if (input$parameters2=="pmdi"){print("PMDI")}
    else {print("Palmer Z-Index")}
  })
  my_scale2 <- renderText({
    month_num <- as.numeric(input$month2)
    scale_num <- as.numeric(input$scale2)
    if ((month_num-scale_num+1)<1){num <- month_num-scale_num+13}
    else {num <- month_num-scale_num+1}
    scale_name <- as.character(month.name[num])
    return(scale_name)
  })
  my_month2 <- renderText({
    num <- as.numeric(input$month2)
    month_name <- as.character(month.name[num])
    return(month_name)
  })
  my_state2 <- renderText({
    num <- as.numeric(input$state2)
    state_name <- as.character(NOAA_states[num,])
    return(state_name)
  })  
  #graph3
  my_csv3 <- reactive({
    my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                    input$state3,"-",input$parameters3,"-",input$scale3,"-",
                    input$month3,"-",input$years3[1],"-",input$years3[2],
                    ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    if(input$parameters3=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters3=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters3=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
    else if (input$parameters3=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
    else {csv_data <- readr::read_csv(my_url,skip=4)}
    csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
    return(csv_data)
  })
  my_label3 <- renderText({
    if(input$parameters3=="tavg") {"Average Temperature (°F)"}
    else if (input$parameters3=="tmax") {print("Maximum Temperature (°F)")}
    else if (input$parameters3=="tmin") {print("Minimum Temperature (°F)")}
    else if (input$parameters3=="pcp"){print("Precipitation (inches)")}
    else if (input$parameters3=="cdd"){print("Cooling Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters3=="hdd"){print("Heating Degree Days (Fahrenheit Degree-Days)")}
    else if (input$parameters3=="pdsi"){print("PDSI")}
    else if (input$parameters3=="phdi"){print("PHDI")}
    else if (input$parameters3=="pmdi"){print("PMDI")}
    else {print("Palmer Z-Index")}
  })
  my_scale3 <- renderText({
    month_num <- as.numeric(input$month3)
    scale_num <- as.numeric(input$scale3)
    if ((month_num-scale_num+1)<1){num <- month_num-scale_num+13}
    else {num <- month_num-scale_num+1}
    scale_name <- as.character(month.name[num])
    return(scale_name)
  })
  my_month3 <- renderText({
    num <- as.numeric(input$month3)
    month_name <- as.character(month.name[num])
    return(month_name)
  })
  my_state3 <- renderText({
    num <- as.numeric(input$state3)
    state_name <- as.character(NOAA_states[num,])
    return(state_name)
  })  

  ##outputs
  output$my_tsplot <- renderPlot({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_scale <- as.character(my_scale())
    plot_month <- as.character(my_month())
    plot_state <- as.character(my_state())
   
    plot_csv2 <- my_csv2()
    plot_label2 <- as.character(my_label2())
    plot_scale2 <- as.character(my_scale2())
    plot_month2 <- as.character(my_month2())
    plot_state2 <- as.character(my_state2()) 
   
    plot_csv3 <- my_csv3()
    plot_label3 <- as.character(my_label3())
    plot_scale3 <- as.character(my_scale3())
    plot_month3 <- as.character(my_month3())
    plot_state3 <- as.character(my_state3()) 
     
    if(input$graph3==TRUE) {
      ggplot(data=plot_csv, aes(x=Year,y=Value)) + geom_line() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(plot_label) + geom_hline(yintercept=mean(plot_csv$Value), color= "grey", lty=2)+
        ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))+
        ##graph2
        geom_line(data=plot_csv2, color="blue") + geom_smooth(data=plot_csv2, method="lm", se=FALSE, color="darkblue") + 
        geom_hline(yintercept=mean(plot_csv2$Value), color= "cornflowerblue", lty=2) +
        ##graph3
        geom_line(data=plot_csv3, color="red") + geom_smooth(data=plot_csv3, method="lm", se=FALSE, color="red4") + 
        geom_hline(yintercept=mean(plot_csv3$Value), color= "pink", lty=2) 
    }
    else if(input$graph2==TRUE) {
      
      ggplot(data=plot_csv, aes(x=Year,y=Value)) + geom_line() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(plot_label) + geom_hline(yintercept=mean(plot_csv$Value), color= "grey", lty=2)+
        ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))+
      ##graph2
        geom_line(data=plot_csv2, color="blue") + geom_smooth(data=plot_csv2, method="lm", se=FALSE, color="darkblue") + 
        geom_hline(yintercept=mean(plot_csv2$Value), color= "cornflowerblue", lty=2)
    }
    else {ggplot(data=plot_csv, aes(x=Year,y=Value)) + geom_line() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(plot_label) + geom_hline(yintercept=mean(plot_csv$Value), color= "grey", lty=2)+
        ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))}
  })
  output$my_dsplot <- renderPlot({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_scale <- as.character(my_scale())
    plot_month <- as.character(my_month())
    plot_state <- as.character(my_state())
    
    plot_csv2 <- my_csv2()
    plot_label2 <- as.character(my_label2())
    plot_scale2 <- as.character(my_scale2())
    plot_month2 <- as.character(my_month2())
    plot_state2 <- as.character(my_state2())
    
    plot_csv3 <- my_csv3()
    plot_label3 <- as.character(my_label3())
    plot_scale3 <- as.character(my_scale3())
    plot_month3 <- as.character(my_month3())
    plot_state3 <- as.character(my_state3())

    if(input$graph3==TRUE){
      ggplot(data=plot_csv, aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2,) + theme_classic() +
        xlab(plot_label) + ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))+
        ##graph2
        geom_density(data=plot_csv2,color="blue") + geom_vline(aes(xintercept = mean(plot_csv2$Value)), lty = 2,color="blue")+
        ##graph2
        geom_density(data=plot_csv3,color="red") + geom_vline(aes(xintercept = mean(plot_csv3$Value)), lty = 2,color="red")
    }
    if(input$graph2==TRUE){
      ggplot(data=plot_csv, aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2,) + theme_classic() +
        xlab(plot_label) + ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))+
        ##graph2
        geom_density(data=plot_csv2,color="blue") + geom_vline(aes(xintercept = mean(plot_csv2$Value)), lty = 2,color="blue")
    }
    else {ggplot(data=plot_csv, aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
      xlab(plot_label) + ggtitle(paste(plot_scale,"-",plot_month,plot_label,"in",plot_state,"from",input$years[1],"to",input$years[2]))}
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
