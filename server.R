library(shiny)
library(tidyverse)

shinyServer(function(input, output,session) {
  NOAA_states <- as.data.frame(state.name)
  NOAA_states <- as.data.frame(NOAA_states[-c(2,11),])
  colnames(NOAA_states) <- "States"
  NOAA_states <- rbind(NOAA_states,data.frame("States" = c("NULL","Alaska","Hawaii")))
  NOAA_states$StateAbr <- c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS",
                            "KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                            "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
                            "VT","VA","WA","WV","WI","WY","NULL","AK","HI")
  
  counties <- readr::read_table("https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt",skip=70)
  counties <- counties[-c(1,2,70,99,115,191,250,314,323,329,397,557,563,608,711,804,904,1010,1131,1196,1213,
                          1238,1253,1337,1425,1508,1624,1682,1776,1794,1805,1827,1861,1924,2025,2079,2168,
                          2246,2283,2351,2357,2404,2471,2567,2822,2852,2867,3004,3044,3100,3173),]
  colnames(counties) <- c("FIPS Code","County")
  counties[859,2] = "O'Brien County"
  counties <- counties %>% mutate(CountyCode = substr(`FIPS Code`, 3, 5))
  
  countylist <- list()
  for (i in 1:3145) {
    name <- counties[i,2]
    num <- counties[i,3]
    countylist[i] <- paste(name,"=",num,sep="")
  }
  
  county_by_state <- data.frame(
    first = c(1,96,111,186,244,307,315,320,387,551,595,697,789,888,993,1113,1177,1193,1217,1231,1314,1401,
              1483,1598,1655,1748,1765,1775,1796,1829,1891,1991,2044,2132,2209,2245,2312,2317,2363,2429,
              2524,2778,2807,2821,2957,2996,3051,3123,0,68,546),
    last = c(67,110,185,243,306,314,317,386,545,594,696,788,887,992,1112,1176,1192,1216,1230,1313,1400,1482,
             1597,1654,1747,1764,1774,1795,1828,1890,1990,2043,2131,2208,2244,2311,2316,2362,2428,2523,
             2777,2806,2820,2956,2995,3050,3122,3145,0,95,550)
  )
  ##inputs
  my_csv <- reactive({
    if (input$countybox1==TRUE) {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/county/time-series/",
                      NOAA_states[input$state,2],"-",substring(input$county,nchar(input$county)-2,nchar(input$county)),
                      "-",input$parameters,"-",input$scale,"-",
                      input$month,"-",input$years[1],"-",input$years[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    else {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                      input$state,"-",input$parameters,"-",input$scale,"-",
                      input$month,"-",input$years[1],"-",input$years[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    if(is.null(tryCatch(readr::read_csv(my_url), error=function(e) NULL))==TRUE)
      {return(NULL)}
    else 
      {if(input$parameters=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
      else {csv_data <- readr::read_csv(my_url,skip=4)}
      csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
      return(csv_data)}
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
    state_name <- as.character(NOAA_states[num,1])
    return(state_name)
  })
  
  my_csv2 <- reactive({
    if (input$countybox2==TRUE) {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/county/time-series/",
                      NOAA_states[input$state2,2],"-",substring(input$county2,nchar(input$county2)-2,nchar(input$county2)),
                      "-",input$parameters2,"-",input$scale2,"-",
                      input$month2,"-",input$years2[1],"-",input$years2[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    else {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                      input$state2,"-",input$parameters2,"-",input$scale2,"-",
                      input$month2,"-",input$years2[1],"-",input$years2[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    if(is.null(tryCatch(readr::read_csv(my_url), error=function(e) NULL))==TRUE)
    {return(NULL)}
    else 
    {if(input$parameters2=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters2=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters2=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters2=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
      else {csv_data <- readr::read_csv(my_url,skip=4)}
      csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
      return(csv_data)}
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
    state_name <- as.character(NOAA_states[num,1])
    return(state_name)
  })  

  my_csv3 <- reactive({
    if (input$countybox3==TRUE) {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/county/time-series/",
                      NOAA_states[input$state3,2],"-",substring(input$county3,nchar(input$county3)-2,nchar(input$county3)),
                      "-",input$parameters3,"-",input$scale3,"-",
                      input$month3,"-",input$years3[1],"-",input$years3[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    else {
      my_url <- paste("https://www.ncdc.noaa.gov/cag/statewide/time-series/",
                      input$state3,"-",input$parameters3,"-",input$scale3,"-",
                      input$month3,"-",input$years3[1],"-",input$years3[2],
                      ".csv?base_prd=true&begbaseyear=1901&endbaseyear=2000",sep = "")
    }
    if(is.null(tryCatch(readr::read_csv(my_url), error=function(e) NULL))==TRUE)
    {return(NULL)}
    else 
    {if(input$parameters3=="pdsi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters3=="phdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters3=="pmdi") {csv_data <- readr::read_csv(my_url,skip=3)}
      else if (input$parameters3=="zndx") {csv_data <- readr::read_csv(my_url,skip=3)}
      else {csv_data <- readr::read_csv(my_url,skip=4)}
      csv_data <- csv_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
      return(csv_data)}
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
    state_name <- as.character(NOAA_states[num,1])
    return(state_name)
  })
  
  ##outputs
  observeEvent(input$refresh ,{session$reload()})
  
  output$ts_error_message <- renderUI({
   if (input$graph1==FALSE&input$graph2==FALSE&input$graph3==FALSE) {
     h1("Please click the checkbox to display a graph")
   }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==TRUE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1, 2, & 3 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==TRUE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 & 2 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==FALSE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 & 3 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==FALSE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==TRUE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 2 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==TRUE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 2 & 3 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==FALSE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 3 to Display a Graph or Restart the App by Clicking the Button Below", style = "color:red")
    }
   else{}
 })
  output$ts_refresh <- renderUI({
    if(is.null(my_csv())==TRUE|is.null(my_csv2())==TRUE|is.null(my_csv3())==TRUE) {actionButton("refresh","Refresh")}
    else {}
      })
  output$ds_error_message <- renderUI({
    if (input$graph1==FALSE&input$graph2==FALSE&input$graph3==FALSE) {
      h1("Please click the checkbox to display a graph")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==TRUE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1, 2, & 3 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==TRUE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 & 2 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==FALSE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 & 3 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==TRUE&is.null(my_csv2())==FALSE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 1 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==TRUE&is.null(my_csv3())==FALSE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 2 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==TRUE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 2 & 3 to Display a Graph", style = "color:red")
    }
    else if(is.null(my_csv())==FALSE&is.null(my_csv2())==FALSE&is.null(my_csv3())==TRUE) {
      h1("There is No Existing Dataset for Your Current Settings. Please Change Your Current Settings for Dataset 3 to Display a Graph", style = "color:red")
    }
   else{}
 })
  output$ds_refresh <- renderUI({
    if(is.null(my_csv())==TRUE|is.null(my_csv2())==TRUE|is.null(my_csv3())==TRUE) {actionButton("refresh","Refresh")}
    else {}
  })
  
  
  output$my_tsplot <- renderPlotly({
      g <- ggplot(data=my_csv(), aes(x=Year,y=Value)) + theme_classic() + xlab("Years") + ylab(my_label())
      if (input$graph1==TRUE) {g <- g + geom_line() + geom_point()}
      #if (input$meanln1==TRUE) {g <- g + geom_hline(yintercept=mean(my_csv()$Value), color= "grey", lty=2)}
      #if (input$trendln1==TRUE) {g <- g + geom_smooth(method="lm", se=FALSE, color="dimgrey")}
      if (input$graph2==TRUE) {g <- g + geom_line(data=my_csv2(), color="blue") + geom_point(data = my_csv2(), color="blue")}
      if (input$meanln2==TRUE) {g <- g +  geom_hline(yintercept=mean(my_csv2()$Value), color= "cornflowerblue", lty=2)}
      if (input$trendln2==TRUE) {g <- g + geom_smooth(data=my_csv2(), method="lm", se=FALSE, color="darkblue")}
      if (input$graph3==TRUE) {g <- g + geom_line(data=my_csv3(), color="red") + geom_point(data = my_csv3(), color="red")}
      if (input$meanln3==TRUE) {g <- g +  geom_hline(yintercept=mean(my_csv3()$Value), color= "pink", lty=2)}
      if (input$trendln3==TRUE) {g <- g + geom_smooth(data=my_csv3(), method="lm", se=FALSE, color="red4")}
      ggplotly(g)
  }) 
  output$my_dsplot <- renderPlotly({ ggplotly(
     if (is.null(my_csv())==TRUE|is.null(my_csv2())==TRUE|is.null(my_csv3())==TRUE) {NULL}
     else if(input$graph1==TRUE&input$graph2==TRUE&input$graph3==TRUE){
       ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
         xlab(my_label()) +
         ##graph2
         geom_density(data=my_csv2(),color="blue") + geom_vline(aes(xintercept = mean(my_csv2()$Value)), lty = 2,color="blue")+
         ##graph3
         geom_density(data=my_csv3(),color="red") + geom_vline(aes(xintercept = mean(my_csv3()$Value)), lty = 2,color="red")
     }
     else if(input$graph1==TRUE&input$graph2==TRUE&input$graph3==FALSE) {
       ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
         xlab(my_label()) +
         ##graph2
         geom_density(data=my_csv2(),color="blue") + geom_vline(aes(xintercept = mean(my_csv2()$Value)), lty = 2,color="blue")
     }
     else if(input$graph1==TRUE&input$graph2==FALSE&input$graph3==TRUE) {
       ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
         xlab(my_label()) +
         ##graph3
         geom_density(data=my_csv3(),color="red") + geom_vline(aes(xintercept = mean(my_csv3()$Value)), lty = 2,color="red")
     }
     else if (input$graph1==TRUE&input$graph2==FALSE&input$graph3==FALSE){
       ggplot(data=my_csv(), aes(Value)) + geom_density() +geom_vline(aes(xintercept = mean(Value)), lty = 2) + theme_classic() +
         xlab(my_label())
     }
     else if (input$graph1==FALSE&input$graph2==TRUE&input$graph3==FALSE){
       ggplot(data=my_csv2(), aes(Value)) + geom_density(color="blue") +geom_vline(aes(xintercept = mean(Value)), lty = 2, color="blue") + theme_classic() +
         xlab(my_label2())
       }
     else if (input$graph1==FALSE&input$graph2==TRUE&input$graph3==TRUE){
       ggplot(data=my_csv2(), aes(Value)) + geom_density(color="blue") +geom_vline(aes(xintercept = mean(Value)), lty = 2, color="blue") + theme_classic() +
         xlab(my_label2()) +
         ##graph3
       geom_density(data=my_csv3(),color="red") + geom_vline(aes(xintercept = mean(my_csv3()$Value)), lty = 2,color="red")
     }
     else if (input$graph1==FALSE&input$graph2==FALSE&input$graph3==TRUE){
       ggplot(data=my_csv3(), aes(Value)) + geom_density(color="red") +geom_vline(aes(xintercept = mean(Value)), lty = 2, color="red") + theme_classic() +
         xlab(my_label3())
      }
     else return(NULL)
   )})

  output$county_box1 <- renderUI({
    county_legend_list <- list()
    for (i in county_by_state[input$state,1]:county_by_state[input$state,2]) {county_legend_list <- c(county_legend_list,countylist[i])}
    if (input$countybox1==TRUE) {
      selectInput("county", h3("County"),choices = county_legend_list)
    }
    else{}
  })
  output$my_legend1ts <- renderUI({
    if(input$graph1==TRUE&is.null(my_csv())==FALSE){
      my_location <- if (input$countybox1==FALSE) {my_state()}
        else {paste(substring(input$county,1,nchar(input$county)-4)," ",NOAA_states[input$state,2])}
      title <- if(input$scale==1) {paste(my_month(),my_label(),"in",my_location,"from",input$years[1],"to",input$years[2])}
        else {paste(my_scale(),"-",my_month(),my_label(),"in",my_location,"from",input$years[1],"to",input$years[2])}
      plot_csv <- my_csv()
      plot_label <- as.character(my_label())
      plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
      lm <- lm(Value ~ Year, data = plot_csv)
      summary_lm <- summary(lm)
      slope <- round(summary_lm$coefficients[2],3)
      intercept <- round(summary_lm$coefficients[1],2)
      line_formula <- paste("Value =",intercept, "+", slope,"*Year",sep="")
      column(4, wellPanel(style = "background: lightgrey",title,br(),br(),
                          plot_formula,checkboxInput("meanln1",h6("Display Mean")),br(),br(),
                          line_formula,checkboxInput("trendln2",h6("Display Trendline")),br(),br(),
                          downloadButton("my_download1ts","Dowload")))
    }
    else {}
  })
  output$my_legend1ds <- renderUI({
    if(input$graph1==TRUE&is.null(my_csv())==FALSE){
      my_location <- if (input$countybox1==FALSE) {my_state()}
      else {paste(substring(input$county,1,nchar(input$county)-4)," ",NOAA_states[input$state,2])}
      title <- if(input$scale==1) {paste(my_month(),my_label(),"in",my_location,"from",input$years[1],"to",input$years[2])}
      else {paste(my_scale(),"-",my_month(),my_label(),"in",my_location,"from",input$years[1],"to",input$years[2])}
      plot_csv <- my_csv()
      plot_label <- as.character(my_label())
      plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
      column(4, wellPanel(style = "background: lightgrey",title,br(),br(),
                          plot_formula,checkboxInput("meands1",h6("Display Mean")),br(),br(),
                          downloadButton("my_download1ds","Dowload")))
    }
    else {}
  })
  output$my_download1ts <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox1==FALSE) {NOAA_states[input$state,2]}
      else {paste(substring(input$county,1,nchar(input$county)-4),"-",NOAA_states[input$state,2], sep="")}
      title <- if(input$scale==1) {paste(substring(my_month(),1,3),"-",input$parameters,"-",my_location,"-",input$years[1],"-",input$years[2],".csv", sep="")}
      else {paste(substring(my_scale(),1,3),"-",substring(my_month(),1,3),"-",input$parameters,"-",my_location,"-",input$years[1],"-",input$years[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv(),file)}
  )
  output$my_download1ds <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox1==FALSE) {NOAA_states[input$state,2]}
      else {paste(substring(input$county,1,nchar(input$county)-4),"-",NOAA_states[input$state,2], sep="")}
      title <- if(input$scale==1) {paste(substring(my_month(),1,3),"-",input$parameters,"-",my_location,"-",input$years[1],"-",input$years[2],".csv", sep="")}
      else {paste(substring(my_scale(),1,3),"-",substring(my_month(),1,3),"-",input$parameters,"-",my_location,"-",input$years[1],"-",input$years[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv(),file)}
  )
  
  output$county_box2 <- renderUI({
    county_legend_list <- list()
    for (i in county_by_state[input$state2,1]:county_by_state[input$state2,2]) {county_legend_list <- c(county_legend_list,countylist[i])}
    if (input$countybox2==TRUE) {
      selectInput("county2", h3("County"),choices = county_legend_list)
    }
    else{}
  })
  output$my_legend2ts <- renderUI({
    if(input$graph2==TRUE&is.null(my_csv())==FALSE){
      my_location <- if (input$countybox2==FALSE) {my_state2()}
      else {paste(substring(input$county2,1,nchar(input$county2)-4)," ",NOAA_states[input$state2,2])}
      title <- if(input$scale2==1) {paste(my_month2(),my_label2(),"in",my_location,"from",input$years2[1],"to",input$years2[2])}
      else {paste(my_scale2(),"-",my_month2(),my_label2(),"in",my_location,"from",input$years2[1],"to",input$years2[2])}
       plot_csv <- my_csv2()
       plot_label <- as.character(my_label2())
       plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
       lm <- lm(Value ~ Year, data = plot_csv)
       summary_lm <- summary(lm)
       slope <- round(summary_lm$coefficients[2],3)
       intercept <- round(summary_lm$coefficients[1],2)
       line_formula <- paste("Value =",intercept, "+", slope,"*Year",sep="")
       column(4, wellPanel(style = "background: cornflowerblue",title,br(),br(),
                           plot_formula,checkboxInput("meanln2",h6("Display Mean")),br(),br(),
                           line_formula,checkboxInput("trendln2",h6("Display Trendline")),br(),br(),
                           downloadButton("my_download2ts","Dowload")))
     }
     else {}
   })
  output$my_legend2ds <- renderUI({
    if(input$graph2==TRUE&is.null(my_csv())==FALSE){
      my_location <- if (input$countybox2==FALSE) {my_state2()}
      else {paste(substring(input$county2,1,nchar(input$county2)-4)," ",NOAA_states[input$state2,2])}
      title <- if(input$scale2==1) {paste(my_month2(),my_label2(),"in",my_location,"from",input$years2[1],"to",input$years2[2])}
      else {paste(my_scale2(),"-",my_month2(),my_label2(),"in",my_location,"from",input$years2[1],"to",input$years2[2])}
       plot_csv <- my_csv2()
       plot_label <- as.character(my_label2())
       plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
       column(4, wellPanel(style = "background: cornflowerblue",title,br(),br(),
                           plot_formula,checkboxInput("meands2",h6("Display Mean")),br(),br(),
                           downloadButton("my_download2ds","Dowload")))
     }
     else {}
   })
  output$my_download2ts <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox2==FALSE) {NOAA_states[input$state2,2]}
      else {paste(substring(input$county2,1,nchar(input$county2)-4),"-",NOAA_states[input$state2,2], sep="")}
      title <- if(input$scale2==1) {paste(substring(my_month2(),1,3),"-",input$parameters2,"-",my_location,"-",input$years2[1],"-",input$years2[2],".csv", sep="")}
      else {paste(substring(my_scale2(),1,3),"-",substring(my_month2(),1,3),"-",input$parameters2,"-",my_location,"-",input$years2[1],"-",input$years2[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv2(),file)}
  )
  output$my_download2ds <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox2==FALSE) {NOAA_states[input$state2,2]}
      else {paste(substring(input$county2,1,nchar(input$county2)-4),"-",NOAA_states[input$state2,2], sep="")}
      title <- if(input$scale2==1) {paste(substring(my_month2(),1,3),"-",input$parameters2,"-",my_location,"-",input$years2[1],"-",input$years2[2],".csv", sep="")}
      else {paste(substring(my_scale2(),1,3),"-",substring(my_month2(),1,3),"-",input$parameters2,"-",my_location,"-",input$years2[1],"-",input$years2[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv2(),file)}
  )
  
  output$county_box3 <- renderUI({
    county_legend_list <- list()
    for (i in county_by_state[input$state3,1]:county_by_state[input$state3,2]) {county_legend_list <- c(county_legend_list,countylist[i])}
    if (input$countybox3==TRUE) {
      selectInput("county3", h3("County"),choices = county_legend_list)
    }
    else{}
  })
  output$my_legend3ts <- renderUI({
     if(input$graph3==TRUE&is.null(my_csv3())==FALSE){
       my_location <- if (input$countybox3==FALSE) {my_state3()}
       else {paste(substring(input$county3,1,nchar(input$county3)-4)," ",NOAA_states[input$state3,2])}
       title <- if(input$scale3==1) {paste(my_month3(),my_label3(),"in",my_location,"from",input$years3[1],"to",input$years3[2])}
       else {paste(my_scale3(),"-",my_month3(),my_label3(),"in",my_location,"from",input$years3[1],"to",input$years3[2])}
       plot_csv <- my_csv3()
       plot_label <- as.character(my_label3())
       plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
       lm <- lm(Value ~ Year, data = plot_csv)
       summary_lm <- summary(lm)
       slope <- round(summary_lm$coefficients[2],3)
       intercept <- round(summary_lm$coefficients[1],2)
       line_formula <- paste("Value =",intercept, "+", slope,"*Year",sep="")
       column(4, wellPanel(style = "background: pink",title,br(),br(),
                           plot_formula,checkboxInput("meanln3",h6("Display Mean")),br(),br(),
                           line_formula,checkboxInput("trendln3",h6("Display Trendline")),br(),br(),
                           downloadButton("my_download3ts","Dowload")))
     }
     else {}
   })
  output$my_legend3ds <- renderUI({
     if(input$graph3==TRUE&is.null(my_csv3())==FALSE){
       title <- if(input$scale3==1) {paste(my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
       else {paste(my_scale3(),"-",my_month3(),my_label3(),"in",my_state3(),"from",input$years3[1],"to",input$years3[2])}
       plot_csv <- my_csv3()
       plot_label <- as.character(my_label3())
       plot_formula <- paste("Mean",plot_label,"=",round(mean(plot_csv$Value),2))
       column(4, wellPanel(style = "background: pink",title,br(),br(),
                           plot_formula,checkboxInput("meands3",h6("Display Mean")),br(),br(),
                           downloadButton("my_download3ds","Dowload")))
     }
     else {}
   })
  output$my_download3ts <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox3==FALSE) {NOAA_states[input$state3,2]}
      else {paste(substring(input$county3,1,nchar(input$county3)-4),"-",NOAA_states[input$state3,2], sep="")}
      title <- if(input$scale3==1) {paste(substring(my_month3(),1,3),"-",input$parameters3,"-",my_location,"-",input$years3[1],"-",input$years3[2],".csv", sep="")}
      else {paste(substring(my_scale3(),1,3),"-",substring(my_month3(),1,3),"-",input$parameters3,"-",my_location,"-",input$years3[1],"-",input$years3[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv3(),file)}
  )
  output$my_download3ds <- downloadHandler(
    filename = function () {
      my_location <- if (input$countybox3==FALSE) {NOAA_states[input$state3,2]}
      else {paste(substring(input$county3,1,nchar(input$county3)-4),"-",NOAA_states[input$state3,2], sep="")}
      title <- if(input$scale3==1) {paste(substring(my_month3(),1,3),"-",input$parameters3,"-",my_location,"-",input$years3[1],"-",input$years3[2],".csv", sep="")}
      else {paste(substring(my_scale3(),1,3),"-",substring(my_month3(),1,3),"-",input$parameters3,"-",my_location,"-",input$years3[1],"-",input$years3[2],".csv", sep="")}
    },
    content = function (file) {write.csv(my_csv3(),file)}
  )
  
})
