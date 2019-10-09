library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
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
  output$my_tsplot <- renderPlotly({ ggplotly(
    if(input$graph3==TRUE) {
      ggplot(data=my_csv(), aes(x=Year,y=Value)) + geom_line() + geom_point() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(my_label()) + geom_hline(yintercept=mean(my_csv()$Value), color= "grey", lty=2)+
        ##graph2
        geom_line(data=my_csv2(), color="blue") + geom_point(data = my_csv2(), color="blue") + geom_smooth(data=my_csv2(), method="lm", se=FALSE, color="darkblue") + 
        geom_hline(yintercept=mean(my_csv2()$Value), color= "cornflowerblue", lty=2) +
        ##graph3
        geom_line(data=my_csv3(), color="red") + geom_point(data = my_csv3(), color="red") + geom_smooth(data=my_csv3(), method="lm", se=FALSE, color="red4") + 
        geom_hline(yintercept=mean(my_csv3()$Value), color= "pink", lty=2) 
    }
    else if(input$graph2==TRUE) {
      ggplot(data=my_csv(), aes(x=Year,y=Value)) + geom_line() + geom_point() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(my_label()) + geom_hline(yintercept=mean(my_csv()$Value), color= "grey", lty=2)+
        ##graph2
        geom_line(data=my_csv2(), color="blue") + geom_point(data = my_csv2(), color="blue") + geom_smooth(data=my_csv2(), method="lm", se=FALSE, color="darkblue") + 
        geom_hline(yintercept=mean(my_csv2()$Value), color= "cornflowerblue", lty=2)
    }
    else {ggplot(data=my_csv(), aes(x=Year,y=Value)) + geom_line() + geom_point() + geom_smooth(method="lm", se=FALSE, color="dimgrey") + theme_classic() +
        xlab("Years") + ylab(my_label()) + geom_hline(yintercept=mean(my_csv()$Value), color= "grey",  lty=2)}
  )})
  output$my_dsplot <- renderPlotly({ ggplotly(
    if(input$graph3==TRUE){
      ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
        xlab(my_label()) +
        ##graph2
        geom_density(data=my_csv2(),color="blue") + geom_vline(aes(xintercept = mean(my_csv2()$Value)), lty = 2,color="blue")+
        ##graph2
        geom_density(data=my_csv3(),color="red") + geom_vline(aes(xintercept = mean(my_csv3()$Value)), lty = 2,color="red")
    }
    else if(input$graph2==TRUE){
      ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
        xlab(my_label()) + 
        ##graph2
        geom_density(data=my_csv2(),color="blue") + geom_vline(aes(xintercept = mean(my_csv2()$Value)), lty = 2,color="blue")
    }
    else {ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
        xlab(my_label())}
  )})
    #graph1
  output$my_legend1 <- renderText({
    if(input$scale==1) {paste(my_month(),my_label(),"in",my_state(),"from",input$years[1],"to",input$years[2])}
    else {paste(my_scale(),"-",my_month(),my_label(),"in",my_state(),"from",input$years[1],"to",input$years[2])}
  })
  output$my_legend1ds <- renderText({
    if(input$scale==1) {paste(my_month(),my_label(),"in",my_state(),"from",input$years[1],"to",input$years[2])}
    else {paste(my_scale(),"-",my_month(),my_label(),"in",my_state(),"from",input$years[1],"to",input$years[2])}
  })
  output$my_mean1 <- renderText({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)
  })
  output$my_mean1ds <- renderText({
    plot_csv <- my_csv()
    plot_label <- as.character(my_label())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)
  })
  output$my_line1 <- renderText({
    plot_csv <- my_csv()
    lm <- lm(Value ~ Year, data = plot_csv)
    summary_lm <- summary(lm)
    slope <- round(summary_lm$coefficients[2],3)
    intercept <- round(summary_lm$coefficients[1],2)
    paste("Value =",intercept, "+", slope,"*Year",sep="")
    #return(intercept)
  })
    #graph2
  output$my_legend2 <- renderText({
    if (input$graph2==FALSE) {""}
    else if(input$scale2==1) {paste(my_month2(),my_label2(),"in",my_state2(),"from",input$years2[1],"to",input$years2[2])}
    else {paste(my_scale2(),"-",my_month2(),my_label2(),"in",my_state2(),"from",input$years2[1],"to",input$years2[2])}
  })
  output$my_legend2ds <- renderText({
    if (input$graph2==FALSE) {""}
    else if(input$scale2==1) {paste(my_month2(),my_label2(),"in",my_state2(),"from",input$years2[1],"to",input$years2[2])}
    else {paste(my_scale2(),"-",my_month2(),my_label2(),"in",my_state2(),"from",input$years2[1],"to",input$years2[2])}
  })
  output$my_mean2 <- renderText({
    if (input$graph2==FALSE) {""}
    else {plot_csv <- my_csv2()
    plot_label <- as.character(my_label2())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)}
  })
  output$my_mean2ds <- renderText({
    if (input$graph2==FALSE) {""}
    else {plot_csv <- my_csv2()
    plot_label <- as.character(my_label2())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)}
  })
  output$my_line2 <- renderText({
    if (input$graph2==FALSE) {""}
    else {plot_csv <- my_csv2()
    lm <- lm(Value ~ Year, data = plot_csv)
    summary_lm <- summary(lm)
    slope <- round(summary_lm$coefficients[2],3)
    intercept <- round(summary_lm$coefficients[1],2)
    paste("Value =",intercept, "+", slope,"*Year",sep="")}
    #return(intercept)
  })
    #graph3
  output$my_legend3 <- renderText({
    if (input$graph3==FALSE) {""}
    else if(input$scale3==1) {paste(my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
    else {paste(my_scale3(),"-",my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
  })
  output$my_legend3ds <- renderText({
    if (input$graph3==FALSE) {""}
    else if(input$scale3==1) {paste(my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
    else {paste(my_scale3(),"-",my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
  })
  output$my_mean3 <- renderText({
    if (input$graph3==FALSE) {""}
    else {plot_csv <- my_csv3()
    plot_label <- as.character(my_label3())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)}
  })  
  output$my_mean3ds <- renderText({
    if (input$graph3==FALSE) {""}
    else {plot_csv <- my_csv3()
    plot_label <- as.character(my_label3())
    plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
    return(plot_formula)}
  })
  output$my_line3 <- renderText({
    if (input$graph3==FALSE) {""}
    else {plot_csv <- my_csv3()
    lm <- lm(Value ~ Year, data = plot_csv)
    summary_lm <- summary(lm)
    slope <- round(summary_lm$coefficients[2],3)
    intercept <- round(summary_lm$coefficients[1],2)
    paste("Value =",intercept, "+", slope,"*Year",sep="")}
    #return(intercept)
  })  
    
  output$my_url <- renderText({
    plot_formula <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                          input$state,"-",input$parameters,"-",input$scale,"-",
                          input$month,"-",input$years[1],"-",input$years[2],
                          ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    return(plot_formula)
  })
})
