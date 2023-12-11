library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(shiny)
library(sf)
library(DT)
library(reshape2)

df <- read.csv("df.csv")
precinct <- read.csv("precinct_graph.csv")
UHF <- read.csv("UHF_graph.csv")

df_total <- filter(df, Name=="Fine Particulate Matter (PM2.5)")
df_total_grp <- group_by(df_total, Year, Borough, Name) 
total_sum <- summarize(df_total_grp, across(c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))
df_total_sum <- summarize(df_total_grp, across(c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))

ui <- fluidPage(
  navbarPage("NYC Air Pollution and Crime",
             
             #------------ Overview tab ---------------
             
             tabPanel("Overview",
                      h1(strong("Data Anaylsis of NYC Air Quality and Crime Data")),
                      h2("Our data analysis compares crime statistics to air quality reports of Boroughs in New York City from 2009 to 2020."),
                      br(),
                      h4("With this analysis, we plan to evaluate different metrics and present findings related to:"),
                      p("- The different types of crime happening in each borough and any yearly increases or decreases seen in the prevalence of different crimes"),
                      p("- The air quality seen in each borough at a given time, determined using the proportions of pollutants such as Nitrogen Dioxide, Ozone, and fine particulate matter collected"),
                      p("- Correlations between air quality and crime metrics in a given borough and year"),
                      p("- Comparisons between proportions of air pollutants and prevalences of felony and misdemeanor crimes in a given borough to city-wide averages"),
                      p("- The different types of crime happening in each borough and any yearly increases or decreases seen in the prevalence of different crimes"),
                      br(),
                      h4("With this data, we plan to gain a better understanding of the potential interplay between crime rates and air quality in 
        different New York City boroughs. Despite it being well-understood that people's actions in a community are greatly influenced 
        by their environment, there is more work to be done in understanding which environmental factors are correlated with higher indexes 
        of crime metrics in a given area. Our aim is to shed light on any discernible patterns, correlations, or anomalies that may exist, 
        providing key insights into the complex relationship between urban environmental factors and crime dynamics. Through our examination 
        of crime types, yearly trends, air quality metrics, and their potential connections between 2010 and 2020, we aim to contribute to a 
        deeper understanding of the multifaceted factors influencing public safety and environmental well-being in our city."),
                      br(),
                      img(src="nyc_skyline.jpeg", height="auto", width="100%")
             ),
             
             #---------- Borough crime tab ------------
             
             tabPanel("Borough crime",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 p(strong("Data Story - why is it important?")),
                                 p("With the arrest data filtered by various specifications, we can get a better look
              at what types of people are ususally arrested, where they are arrested, what types of arrests
              are happening in a given area at a given time, and the nature of common arrests. When considering
              the air qualities in these same precincts and boroughs, comparisons can 
              be made to air quality in a given area and the metrics found in these models")
                               ),
                               wellPanel(
                                 sliderInput(inputId="borough_year", "Select Year", 2009, 2020, 2009, sep=""),
                                 selectInput(inputId="select_type", "Select Info Type", c("Gender", "Age", "Crime"), "Gender"),
                                 uiOutput(outputId="type"),
                                 selectInput(inputId="select_borough", "Select Borough", c("Bronx","Queens","Staten Island","Brooklyn","Manhattan"), "Bronx")
                               ),
                               wellPanel(
                                 p(strong("How to use filter")),
                                 helpText("Using the slider and selection menus you can", strong(code("filter")), "the crime statistic shown on the map based on the year of observation and demographic of criminal by",
                                          strong(code("Gender")), ",", strong(code("Age")), "," , strong(code("Crime Type")), ". ", "You may also", strong(code("hover")), "and", strong(code("zoom in")),
                                          "over the map to see specific statistic for each particular Precinct and Borough providing you insight on where different types of crime generally occur and the frequency of it. The table
            at the bottom is reactive basedd on the last select menu where you can display data for each Precicnt that is within the Borough you selected. Lastly on the population graph
            tabpanel you can futher compare the crime statistic of each Borough filtering by the same", strong(code("Year")), ",", strong(code("Gender")), ",", strong(code("Age")), ", and," , strong(code("Crime")),
                                          "using the same controls")
                               ),
                               wellPanel(
                                 p(strong("Summary")),
                                 p("Taking a look at the Arrest in Borough choropleth map, we can see that most of the arrest (dark red) comes from the Brooklyn and Manhattan with Bronx in first. When zooming into the Precinct level, we see that
              most of the arrest comes from precinct number 75 in Brooklyn and number 44, 40, and 14 in Manhattan indicating a high level of danger and a need for more resources, policy changes, and caution when roaming those area.
              Using the population graph panel, we see a more in-depth picture of the individual demographic groups that contributed to the most crimes from each Borough although there are some interesting trends. For example, how 
              the number of felony seem to stay roughly the same throughout the years in all Borough, indicating some sort of fauly with policies relating to felony type crimes. Last interesting observation was how in 2009, how Brooklyn
              and Manhattan seemed to have similar number of perpetrators ages 25-44, but Manhattan had a substantially larger population of 45-64 perpetrator, indicating possible differences in how air pollution might affect people of 
              different age and their tendency to commit crimes")
                               )
                        ),
                        
                        column(8,
                               wellPanel(
                                 tabsetPanel(
                                   tabPanel("Borough Map",
                                            h4("NYC Borough Map", align="center"),
                                            fluidRow(
                                              column(6, plotlyOutput(outputId="precinct_choro_map")),
                                              column(6, plotlyOutput(outputId="borough_choro_map"))
                                            ),
                                            p("Note: Items may take up to ten seconds to load."),
                                            br(),
                                            DT::dataTableOutput(outputId="borough_map_table")
                                   ),
                                   tabPanel("Population Graph",
                                            h4("Population Graph", align="center"),
                                            plotlyOutput(outputId="population_bar"),
                                            br(),
                                            DT::dataTableOutput(outputId="population_table")
                                   )
                                   
                                 )
                               )
                        )
                      ),
             ),
             
             
             #--------------- Comparing Borough Mean tab --------------------
             
             tabPanel("Air crime trend",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 p(strong("Data Story - why is it important?")),
                                 p("Moving on to the correlation between air pollutation and crime rates itself, we can use this page to look at the
                                   trends for each and compare the trends to see if we find a relationship between the ammount of pollutant in the air and the crime rate of each Borough. 
                                   This is important as it will allow us to determine if there is a connection between air pollution and a person's tendency to commit a crime. This will 
                                   also allow us to better understand the effects of pollution of the human mind, specifically on the side of it responsible for moral decision making and 
                                   whether or not long term exposure to pollution will lead to mental problems. It can also create room for dialogue regarding air pollution regulations and 
                                   coming up with new ways to create more human-friendly air emmissions")
                               ),
                               wellPanel(
                                 selectInput(inputId="borough_first_input", "Select Borough", choices=c("Bronx","Queens","Staten Island","Brooklyn","Manhattan"), "Staten Island"),
                                 selectInput(inputId="pollutant_input", "Select Pollutant Data to Display", choices=c("Fine Particulate Matter (PM2.5)","Nitrogen Dioxide (NO2)","Ozone (O3)","Sulfur Dioxide (SO2)"), selected="Fine Particulate Matter (PM2.5)"),
                                 sliderInput(inputId="borough_year1", "Select Year", 2009, 2020, 2009, sep="")
                               ),
                               wellPanel(
                                 p(strong("How to use filters")),
                                 p("Using the", code("Select Borough"), "dropdown, you may select the particular Borough you want crime and pollutant data plotted on the same line graph to observe trends for.
                                   You may also select the type of pollutant using", code("Select pollutant"), "to display the respective data on the graph along with to display on the Borough choropleth map above.
                                   Lastly, to observe the similarity and differences between high and low pollution and crime areas on the choropleth map over time, you may use the", code("select year"), "slider to select a particular year to display")
                               ),
                               wellPanel(
                                 p(strong("Summary")),
                                 p("Looking at the choropleth map between the different years, it is clear that the darker areas, corresponding to higher air pollution value and larger crime amount, matches up with one another as time goes on. For exmaple, 
                                 one of the consistently darker blue area corresponding to high pollution level is Manhattan which also seem to have a consistently high crime rate in comparison to the other Boroughs. Thus, we noted that this may be indication 
                                 of a positive correlation between air pollution and crime rates. Moving on to the line chart, we yet again see most clearly when filtering the Borough by Staten Island, that the intervals of increases and decreases of both the pollutant 
                                 value and crime values seem to match up, peaking on 2011 and 2014 while dropping on 2012 and 2015/2016, showing another possible correlation between the two variable")
                               )
                        ),
                        column(8,
                               wellPanel(
                                 tabsetPanel(
                                   tabPanel("Correlation Line",
                                            fluidRow(
                                              column(6, plotlyOutput(outputId="first_map")),
                                              column(6, plotlyOutput(outputId="second_map"))
                                            ),
                                            br(),
                                            plotlyOutput(outputId="first_line")
                                   )
                                 )
                               )
                        )
                      )
             ),
             
             #------------- Seasonal crime tab -----------------
             
             tabPanel("Seasonal crime",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 p(strong("Data Story - why is it important?")),
                                 p("Finally, we can gain more insight on the data by seperating it into Summer and Winter seasons. 
              By doing so we can look deeper into whether or not the positive correlation between crime and air 
              pollutant continues even with different warm/cold weathers. This is important as it let us to better 
              have an picture of what the correlation between crime and air pollution is like, if it is generally more
              polluted and dangerous during the winters, and it will also allows law enforcement agencies and policymakers better allocate resources and plan for the future to crime 
              rates by knowing when most likely occurs")
                               ),
                               wellPanel(
                                 selectInput(inputId="selected_borough", "Select Borough", multiple=FALSE, choices=c("Bronx","Queens","Staten Island","Brooklyn","Manhattan")),
                                 selectInput(inputId="selected_pollutant", "Select pollutant to include", multiple=TRUE, choices=c("Fine Particulate Matter (PM2.5)","Nitrogen Dioxide (NO2)","Ozone (O3)","Sulfur Dioxide (SO2)"), selected=c("Fine Particulate Matter (PM2.5)","Nitrogen Dioxide (NO2)","Ozone (O3)","Sulfur Dioxide (SO2)")),
                                 selectInput(inputId="selected_crime", "Select crime type to include", multiple=TRUE, choices=c("misdemeanor","felony","violation"), selected=c("misdemeanor","felony","violation"))
                               ),
                               wellPanel(
                                 p(strong("How to use filters")),
                                 helpText("Using the", code("Select Borough"), "dropdown box, you may select the particular Borough you want to display data for. Then, by clicking the pollutant names in", 
                                          code("Select pollutant to include"), ", press backspace to remove the pollutant from the graph or re-add the pollutant by pressing on it again. You may also do the same with the",
                                          code("Select crime type to include"), "box to choose which crime type in order to compare and contrast it with the trends of the pollutant lines over time. To use the scatterplot panel, 
              you may remove all selection in both the", code("Select pollutant"), "and", code("Select crime"), "box and re-add one type from each to include. Through the plot you may see trends 
              of those two types with the help of the best fit estimation line")
                               ),
                               wellPanel(
                                 p(strong("Summary")),
                                 p("Taking a look at the line graphs, we see no significant differences of values for both crime rate and pollutant ammount between both summer trend and winter trends.  However, we noted that
            in both summer and winter, the pollutant and crime value seem to follow a downward trend as time goes on. Moreover, we found that the peaks and valleys of the misdemeanor line, in particular,
            seem to follow the ones for the pollutants, PM2.5 in particular, fairly closely, indicating some sort of positive correlation between air pollution and crime rates in both the summer and winte
            r seasons. Laslty, in looking at the scatter plot, particularly with PM2.5 and misdemeanor data from the bronx, we yet again see a positive correlation along with the similarity of values between
            both seasons, indicating no significant differences in seasonal crime and pollution")
                               )
                        ),
                        column(8,
                               wellPanel(
                                 tabsetPanel(
                                   tabPanel("Pollutant trend",
                                            plotlyOutput(outputId="summer_line"),
                                            plotlyOutput(outputId="winter_line") 
                                   ),
                                   tabPanel("Scatterplot",
                                            plotlyOutput(outputId="season_scatter"),
                                            DT::dataTableOutput(outputId="season_table")
                                   )
                                 ) 
                               )
                        )
                      )
             ),
             
             #----------------- Concluson tab ------------------- 
             
             
             #------------------- HTML style ---------------------
             
             tags$head(tags$style(HTML('.navbar-default .navbar-brand {background-color: #ffb200; color: #FFFFFF}; font-family: Arial;}'))),
             tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  )
)

server <- function(input, output) {
  
  #---------- Selection sidebar ------------
  
  output$type <- renderUI({
    if (input$select_type=="Gender") { selectInput(inputId="borough_gender", "Select Gender", c("Male","Female","All"), "All") }
    else if (input$select_type=="Age") { selectInput(inputId="borough_age", "Select Age Group", c("<18","18-24", "25-44", "45-64", "65+"), "<18") }
    else if (input$select_type=="Crime") { selectInput(inputId="borough_type", "Select Crime Type", c("Misdemeanor","Felony","Violation"), "Misdemeanor") }
  })
  
  #-------- Choropleth Map -----------------
  
  filt_borough <- reactive({
    df <- read.csv("df.csv")
    final <- filter(df, Name=="Fine Particulate Matter (PM2.5)")
    final_grp <- group_by(final, Year, Borough, Name) 
    final_df <- summarize(final_grp, across(c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))
    final_df <- filter(final_df, Year==input$borough_year)
    if (input$select_type=="Gender") { 
      if (input$borough_gender=="Male") { 
        map <- final_df[, c("Borough","male")] 
        map <- rename_at(map, "male", ~"value")
      } else if (input$borough_gender=="Female") { 
        map <- final_df[, c("Borough","female")] 
        map <- rename_at(map, "female", ~"value")
      } else if (input$borough_gender=="All") { 
        final_df$value <- rowSums(final_df[, c("male","female")])
        map <- final_df[, c("Borough","value")]
      }
    }
    else if (input$select_type=="Age") { 
      if (input$borough_age=="<18") { 
        map <- final_df[, c("Borough", "X.18")]
        map <- rename_at(map, "X.18", ~"value")
      } else if (input$borough_age=="18-24") { 
        map <- final_df[, c("Borough", "X18.24")]
        map <- rename_at(map, "X18.24", ~"value")
      } else if (input$borough_age=="25-44") { 
        map <- final_df[, c("Borough", "X25.44")]
        map <- rename_at(map, "X25.44", ~"value")
      } else if (input$borough_age=="45-64") { 
        map <- final_df[, c("Borough", "X45.64")]
        map <- rename_at(map, "X45.64", ~"value")
      } else if (input$borough_age=="65+") { 
        map <- final_df[, c("Borough", "X65.")]
        map <- rename_at(map, "X65.", ~"value")
      }
    }
    else if (input$select_type=="Crime") { 
      if (input$borough_type=="Misdemeanor") {
        map <- final_df[, c("Borough", "misdemeanor")]
        map <- rename_at(map, "misdemeanor", ~"value")
      } else if (input$borough_type=="Felony") {
        map <- final_df[, c("Borough", "felony")]
        map <- rename_at(map, "felony", ~"value")
      } else if (input$borough_type=="Violation") {
        map <- final_df[, c("Borough", "violation")]
        map <- rename_at(map, "violation", ~"value")
      }
    } else {
      map <- final_df[, c("Borough","male")] 
      map <- rename_at(map, "male", ~"value")
    }
    return(map)
  })
  
  filt_precinct <- reactive({
    end <- read.csv("precinct_graph.csv")
    end_df <- filter(end, Year==input$borough_year)
    if (input$select_type=="Gender") { 
      if (input$borough_gender=="Male") { 
        m <- end_df[, c("ARREST_PRECINCT","male")] 
        m <- rename_at(m, "male", ~"value")
      } else if (input$borough_gender=="Female") { 
        m <- end_df[, c("ARREST_PRECINCT","female")] 
        m <- rename_at(m, "female", ~"value")
      } else if (input$borough_gender=="All") { 
        end_df$value <- rowSums(end_df[, c("male","female")])
        m <- end_df[, c("ARREST_PRECINCT","value")]
      }
    }
    else if (input$select_type=="Age") { 
      if (input$borough_age=="<18") { 
        m <- end_df[, c("ARREST_PRECINCT", "X.18")]
        m <- rename_at(m, "X.18", ~"value")
      } else if (input$borough_age=="18-24") { 
        m <- end_df[, c("ARREST_PRECINCT", "X18.24")]
        m <- rename_at(m, "X18.24", ~"value")
      } else if (input$borough_age=="25-44") { 
        m <- end_df[, c("ARREST_PRECINCT", "X25.44")]
        m <- rename_at(m, "X25.44", ~"value")
      } else if (input$borough_age=="45-64") { 
        m <- end_df[, c("ARREST_PRECINCT", "X45.64")]
        m <- rename_at(m, "X45.64", ~"value")
      } else if (input$borough_age=="65+") { 
        m <- end_df[, c("ARREST_PRECINCT", "X65.")]
        m <- rename_at(m, "X65.", ~"value")
      }
    }
    else if (input$select_type=="Crime") { 
      if (input$borough_type=="Misdemeanor") {
        m <- end_df[, c("ARREST_PRECINCT", "misdemeanor")]
        m <- rename_at(m, "misdemeanor", ~"value")
      } else if (input$borough_type=="Felony") {
        m <- end_df[, c("ARREST_PRECINCT", "felony")]
        m <- rename_at(m, "felony", ~"value")
      } else if (input$borough_type=="Violation") {
        m <- end_df[, c("ARREST_PRECINCT", "violation")]
        m <- rename_at(m, "violation", ~"value")
      }
    } else {
      m <- end_df[, c("ARREST_PRECINCT","male")] 
      m <- rename_at(m, "male", ~"value")
    }
    return(m)
  })
  
  output$borough_choro_map <- renderPlotly({
    borough_shape <- st_read("nybb.shp")
    borough_df <- merge(borough_shape, filt_borough(), by.x="BoroName", by.y="Borough", all.x=TRUE)
    borough_df$Statistics <- paste0("\nName: ", borough_df$BoroName, "\nCrimes: ", borough_df$value)
    p <- ggplot(borough_df) + geom_sf(aes(fill=value, label=Statistics)) + scale_fill_gradient(low = "yellow", high = "red") + labs(title="Arrest in Borough")
    return(ggplotly(p, tooltip="label"))
  })
  
  output$precinct_choro_map <- renderPlotly({
    precinct_shape <- st_read("nycc.shp")
    precinct_df <- merge(precinct_shape, filt_precinct(), by.x="precinct", by.y="ARREST_PRECINCT", all.x=TRUE)
    precinct_df$Statistics <- paste0("\nPrecinct Number: ", precinct_df$precinct, "\nCrimes: ", precinct_df$value)
    p <- ggplot(precinct_df) + geom_sf(aes(fill=value, label=Statistics)) + scale_fill_gradient(low = "yellow", high = "red") + labs(title=(title="Arrest in Precicnt"))
    return(ggplotly(p, tooltip="label"))
  })
  
  precinct_in_borough <- reactive({
    mask <- c()
    if (input$select_borough=="Bronx") { mask <- 40:52 }
    if (input$select_borough=="Queens") { mask <- 100:115 }
    if (input$select_borough=="Staten Island") { mask <- 120:123 }
    if (input$select_borough=="Brooklyn") { mask <- 60:94 }
    if (input$select_borough=="Manhattan") { mask <- 1:34 }
    return(mask)
  })
  
  output$borough_map_table <- renderDataTable({
    filtered_borough <- filter(precinct, Year==input$borough_year, ARREST_PRECINCT%in%precinct_in_borough())
    filtered_borough <- rename_at(filtered_borough, "ARREST_PRECINCT", ~"Precinct")
    filtered_borough <- select(filtered_borough, -c("Year", "drug_use", "larceny", "DUI", "assault", "total_crime"))
    return(filtered_borough)
  })
  
  #--------- Population Bar Charts --------------------
  
  output$population_bar <- renderPlotly({
    filtered_year <- filter(df_total_sum, Year==input$borough_year)
    if (input$select_type=="Gender") { dfm <- melt(filtered_year[, c("Borough","male","female")], id.vars=1) }
    else if (input$select_type=="Age") { dfm <- melt(filtered_year[, c("Borough","X.18","X18.24","X25.44","X45.64","X65.")], id.vars=1) }
    else if (input$select_type=="Crime") { dfm <- melt(filtered_year[, c("Borough","misdemeanor","felony", "violation")], id.vars=1) }
    p <- ggplot(dfm) + geom_bar(aes(x=Borough, y=value, fill=variable), stat = "identity",position = "dodge") + ylim(0, 100000) + labs(y="Crimes", fill=input$select_type)
    return(p)
  })
  
  output$population_table <- renderDataTable({
    population <- filter(select(df_total_sum, -c("Name")), Year==input$borough_year)
    if (input$select_type=="Gender") { population <- population[, c("Borough","male","female")] }
    else if (input$select_type=="Age") { population <- population[, c("Borough","X.18","X18.24","X25.44","X45.64","X65.")]  }
    else if (input$select_type=="Crime") { population <- population[, c("Borough","misdemeanor","felony", "violation")] }
    return(population)
  })
  
  #----------- Season Line Graphs --------------------
  
  summer_borough_data <- reactive({
    data <- melt(filter(df, Borough==input$selected_borough, start_season=="Summer")[, c("Borough", "Year", "Name", "avg_value", "misdemeanor", "felony", "violation")], id.vars=c(1,2,3,4))
    data <- filter(data, Name%in%input$selected_pollutant, variable%in%input$selected_crime) 
    return(data)
  })
  
  winter_borough_data <- reactive({
    data <- melt(filter(df, Borough==input$selected_borough, start_season=="Winter")[, c("Borough", "Year", "Name", "avg_value", "misdemeanor", "felony", "violation")], id.vars=c(1,2,3,4))
    data <- filter(data, Name%in%input$selected_pollutant, variable%in%input$selected_crime)    
    return(data)
  })
  
  output$summer_line <- renderPlotly({
    p <- ggplot(summer_borough_data(), aes(x=Year, y=avg_value, col=Name)) + geom_line(linetype="dashed")
    p <- p + geom_line(data=summer_borough_data(), mapping=aes(x=Year, y=value/1000, col=variable)) + ylim(0, 42) +
      labs(x="Year of Season", y="Avgerage Value", title="Summer Pollutant Trend")
    return(ggplotly(p))
  })
  
  output$winter_line <- renderPlotly({
    p <- ggplot(winter_borough_data(), aes(x=Year, y=avg_value, col=Name)) + geom_line(linetype="dashed")
    p <- p + geom_line(data=winter_borough_data(), mapping=aes(x=Year, y=value/1000, col=variable)) + ylim(0, 42) +
      labs(x="Year of Season", y="Average Value", title="Winter Pollutant Trend")
    return(ggplotly(p))
  })
  
  #----------- Season Scatterplot ------------
  
  output$season_scatter <- renderPlotly({
    p <- ggplot() + geom_point(data=filter(summer_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), mapping=aes(x=avg_value, y=value), color="orange") + 
      geom_smooth(data=filter(summer_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), aes(x=avg_value, y=value), fill="red", colour="red", size=0.5) +
      geom_point(data=filter(winter_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), mapping=aes(x=avg_value, y=value), color="skyblue") + 
      geom_smooth(data=filter(winter_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), aes(x=avg_value, y=value), fill="blue", colour="blue", size=0.5) +
      labs(x="Average annual pollutant value (ppm)", y="Number of crime Seasonally", title="Summer vs Winter Scatterplot")
    return(ggplotly(p))
  })
  
  output$season_table <- renderDataTable({
    arrange(rbind(filter(summer_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), filter(winter_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1])), value)
  })
  
  #------------- Pollutant Map ----------------
  
  pollutant_first_filter <- reactive({
    df <- read.csv("df.csv")
    df_total <- filter(df, Name==input$pollutant_input)
    df_total_grp <- group_by(df_total, Year, Borough, Name) 
    df_total_sum <- summarize(df_total_grp, across(c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))
    filt <- filter(df_total_sum, Borough==input$borough_first_input)
    return(filt)
  })
  
  output$first_line <- renderPlotly({
    p <- ggplot(pollutant_first_filter(), mapping=aes(x=Year, y=total_crime, col=Borough)) + geom_line(linetype="dashed")
    p <- p + geom_line(data=pollutant_first_filter(), aes(x=Year, y=avg_value*500, col=Name)) + labs(y="Arrests / Parts per Million") +
      ggtitle(paste(input$borough_first_input, "Crime and Pollutant rate")) + theme(plot.title = element_text(size = 12))
    return(p)
  })
  
  first_map_filter <- reactive({
    df <- read.csv("df.csv")
    df_total <- filter(df)
    df_total_grp <- group_by(df_total, Year, Borough, Name) 
    total_sum <- summarize(df_total_grp, across(c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))
  })
  
  output$first_map <- renderPlotly({
    borough_shape <- st_read("nybb.shp")
    borough_df <- merge(borough_shape, filter(first_map_filter(), Year==input$borough_year1, Name==input$pollutant_input), by.x="BoroName", by.y="Borough", all.x=TRUE)
    borough_df$Statistics <- paste0("\nName: ", borough_df$BoroName, "\nValue: ", borough_df$avg_value/2)
    p <- ggplot(borough_df) + geom_sf(aes(fill=avg_value, label=Statistics)) + scale_fill_gradient(low = "white", high = "darkblue") + 
      ggtitle(paste(input$pollutant_input, " By Parts Per Million")) + theme(plot.title = element_text(size = 11), plot.margin = margin(20, 20, 20, 20))
    return(ggplotly(p, tooltip="label"))
  })
  
  output$second_map <- renderPlotly({
    borough_shape <- st_read("nybb.shp")
    borough_df <- merge(borough_shape, filter(total_sum, Year==input$borough_year1), by.x="BoroName", by.y="Borough", all.x=TRUE)
    borough_df$Statistics <- paste0("\nName: ", borough_df$BoroName, "\nValue: ", borough_df$total_crime)
    p <- ggplot(borough_df) + geom_sf(aes(fill=total_crime, label=Statistics)) + scale_fill_gradient(low = "yellow", high = "red") +
      ggtitle(paste("Number of Arrests")) + theme(plot.title = element_text(size = 11), plot.margin = margin(20, 20, 20, 20))
    return(ggplotly(p, tooltip="label"))
  })
  
}

#


shinyApp(ui = ui, server = server)
