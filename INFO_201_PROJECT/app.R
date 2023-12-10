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
df_total_sum <- summarize(df_total_grp, across(c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))

ui <- fluidPage(
  navbarPage("NYC Air Pollution and Crime",
             
    #------------ Overview tab ---------------
    
    tabPanel("Overview",
      h1("Overview")
    ),
    
    #---------- Borough crime tab ------------
    tabPanel("Borough crime",
      fluidRow(
        column(4,
          wellPanel(
            sliderInput(inputId="borough_year", "Select Year", 2009, 2020, 2009, sep=""),
            selectInput(inputId="select_type", "Select Info Type", c("Gender", "Age", "Crime"), "Gender"),
            uiOutput(outputId="type"),
            selectInput(inputId="select_borough", "Select Borough", c("Bronx","Queens","Staten Island","Brooklyn","Manhattan"), "Bronx")
          ),
          wellPanel(
            p(strong("How to use filter")),
            p("placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph 
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph")
          ),
          wellPanel(
            p(strong("Summary")),
            p("placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph 
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph")
          )
        ),

        column(8,
          wellPanel(
            tabsetPanel(
              tabPanel("Borough Map",
                       h4("NYC Borough Map", align="center"),
                       fluidRow(
                         column(6, plotOutput(outputId="borough_choro_map")),
                         column(6, plotOutput(outputId="precinct_choro_map"))
                       ),
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
    
    #------------- Seasonal crime tab -----------------
    
    tabPanel("Seasonal crime",
      fluidRow(
        column(3,
          wellPanel(
            selectInput(inputId="selected_borough", "Select Boroughs to include", multiple=FALSE, choices=c("Bronx","Queens","Staten Island","Brooklyn","Manhattan")),
            selectInput(inputId="selected_pollutant", "Select pollutant to include", multiple=TRUE, choices=c("Fine Particulate Matter (PM2.5)","Nitrogen Dioxide (NO2)","Ozone (O3)","Sulfur Dioxide (SO2)"), selected=c("Fine Particulate Matter (PM2.5)","Nitrogen Dioxide (NO2)","Ozone (O3)","Sulfur Dioxide (SO2)")),
            selectInput(inputId="selected_crime", "Select crime type to include", multiple=TRUE, choices=c("misdemeanor","felony","violation"), selected=c("misdemeanor","felony","violation"))
          ),
          wellPanel(
            p(strong("How to use filters"))
          ),
          wellPanel(
            p(strong("Summary"))
          )
        ),
        column(9,
          wellPanel(
            tabsetPanel(
              tabPanel("Pollutant trend",
                plotlyOutput(outputId="summer_line"),
               plotlyOutput(outputId="winter_line") 
              ),
              tabPanel("Scatterplot",
                plotlyOutput(outputId="season_scatter")
              )
            ) 
          )
        )
      )
    ),
    
    #---------------Third gage tab --------------------
    
    tabPanel("third page",
    ),
    
    #----------------- Concluson tab ------------------- 
    tabPanel("Conclusion",
             
    )
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
    }
    return(m)
  })
    
  output$borough_choro_map <- renderPlot({
    borough_shape <- st_read("nybb.shp")
    borough_df <- merge(borough_shape, filt_borough(), by.x="BoroName", by.y="Borough", all.x=TRUE)
    borough_df$Statistics <- paste0("\nName: ", borough_df$BoroName, "\nValue: ", borough_df$value)
    p <- ggplot(borough_df) + geom_sf(aes(fill=value, label=Statistics)) + scale_fill_gradient(low = "yellow", high = "red") 
    return(ggplotly(p, tooltip="label"))
  })
  
  output$precinct_choro_map <- renderPlot({
    precinct_shape <- st_read("nycc.shp")
    precinct_df <- merge(precinct_shape, filt_precinct(), by.x="precinct", by.y="ARREST_PRECINCT", all.x=TRUE)
    precinct_df$Statistics <- paste0("\nPrecinct Number: ", precinct_df$precinct, "\nValue: ", precinct_df$value)
    p <- ggplot(precinct_df) + geom_sf(aes(fill=value, label=Statistics)) + scale_fill_gradient(low = "yellow", high = "red") 
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
    p <- ggplot(dfm) + geom_bar(aes(x=Borough, y=value, fill=variable), stat = "identity",position = "dodge") + ylim(0, 100000)
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
    data <- filter(data, Name%in%input$selected_pollutant)    
    return(data)
  })

  output$summer_line <- renderPlotly({
    p <- ggplot(summer_borough_data(), aes(x=Year, y=avg_value, col=Name)) + geom_line()
    p <- p + geom_line(data=summer_borough_data(), mapping=aes(x=Year, y=value/1000, col=variable)) + ylim(0, 42) +
      labs(x="Year of Season", y="Avgerage Value", title="Summer")
    return(ggplotly(p))
  })
  
  output$winter_line <- renderPlotly({
    p <- ggplot(winter_borough_data(), aes(x=Year, y=avg_value, col=Name)) + geom_line()
    p <- p + geom_line(data=winter_borough_data(), mapping=aes(x=Year, y=value/1000, col=variable)) + ylim(0, 42) +
      labs(x="Year of Season", y="Average Value", title="Winter")
    return(ggplotly(p))
  })
  
  #----------- Season Scatterplot ------------

  output$season_scatter <- renderPlotly({
    p <- ggplot() + geom_point(data=filter(summer_borough_data(), variable=="misdemeanor", Name==input$selected_pollutant[1]), mapping=aes(x=avg_value, y=value), color="orange") + 
      geom_smooth(data=filter(summer_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), aes(x=avg_value, y=value), fill="red", colour="red", size=0.5) +
      geom_point(data=filter(winter_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), mapping=aes(x=avg_value, y=value), color="skyblue") + 
      geom_smooth(data=filter(winter_borough_data(), variable==input$selected_crime[1], Name==input$selected_pollutant[1]), aes(x=avg_value, y=value), fill="blue", colour="blue", size=0.5) +
      labs(x="Average annual pollutant value (ppm)", y="Number of crime Seasonally")
    return(ggplotly(p))
  })
}
  

shinyApp(ui = ui, server = server)
