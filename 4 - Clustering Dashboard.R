
library(shiny)
library(shinydashboard)
library(RColorBrewer)  ## Needed?
library(dplyr)
library(tidyr)   ## Not needed?
library(ggplot2)

# Load the data
wine_data <- readr::read_csv("data/processed/final_wine_data.csv")

# Create my colour palette for the clusters
cluster_palette <- c("#1b9e77", "#e7298a", "#7570b3", "#d95f02")

pie_chart_data <- wine_data %>% group_by(cluster) %>% summarise(count = n()) %>% mutate(cluster = paste0("Cluster ",cluster))


ui <- dashboardPage(
  
  dashboardHeader(title = "Wine Marketing Segmentation",
                  titleWidth = "100%"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tabItem(tabName = "main_dashboard",
            fluidRow( # Now the only row
              column(width = 6, plotlyOutput("test_plot", height = 600, width = "100%")),
              
              ## Right Hand Column
              column(width = 6,  # Width (relative to full screen)
                     fluidRow(box(plotlyOutput("test_plot2", height = 450, width = "200%"))), # First row - 1 graph
                     
                     # Next row - put in a box?
                     fluidRow(box(title = "Inputs", status = "primary", solidHeader = TRUE,  width = 10, #height = 440,
                       column(width = 4, selectInput("filter_var_1", "Filter Variable 1", choices=colnames(wine_data)[-12],
                                                      selected=colnames(wine_data)[1])),
                       column(width = 4, selectInput("filter_var_2", "Filter Variable 2", choices=colnames(wine_data)[-12],
                                          selected=colnames(wine_data)[2])),
                       column(width = 4, selectInput("filter_var_3", "Filter Variable 3", choices=colnames(wine_data)[-12],
                                          selected=colnames(wine_data)[3]))
                       )
                       ),
                     fluidRow(box(plotlyOutput("test_plot3", height = 450, width = "200%"))),
                     fluidRow(box(title = "Average Values by Cluster", status = "primary", solidHeader = TRUE,  width = 10, 
                       tableOutput("summary_table")#, width = 6
                                  )
                              ))
            )
    ))
)


shinyApp(ui, server)


server <- function(input, output) { 
  
  pie_chart_data2 <- reactive({pie_chart_data})
  
  output$test_plot <- renderPlotly({
    plot_ly(pie_chart_data2(), labels = ~cluster, values = ~count, type = 'pie', sort = FALSE, direction = "clockwise", textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = cluster_palette,
                          line = list(color = '#FFFFFF', width = 1)))  %>% 
      layout(title = 'United States Personal Expenditures by Categories in 1960',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(font = list(size = 20, itemwidth = 30)))
  })
  
  output$test_plot2 <- renderPlotly({
    plot_ly(pie_chart_data2(), labels = ~cluster, values = ~count, type = 'pie', sort = FALSE, direction = "clockwise", textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = cluster_palette,
                          line = list(color = '#FFFFFF', width = 1)))  %>% 
      layout(title = 'United States Personal Expenditures by Categories in 1960',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(font = list(size = 20, itemwidth = 30)))
  })
  

  output$test_plot3 <- renderPlotly({
    plot_ly(pie_chart_data2(), labels = ~cluster, values = ~count, type = 'pie', sort = FALSE, direction = "clockwise", textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = cluster_palette,
                          line = list(color = '#FFFFFF', width = 1)))  %>% 
      layout(title = 'United States Personal Expenditures by Categories in 1960',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(font = list(size = 20, itemwidth = 30)))
  })

  output$test_plot4 <- renderPlotly({
    plot_ly(pie_chart_data2(), labels = ~cluster, values = ~count, type = 'pie', sort = FALSE, direction = "clockwise", textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = cluster_palette,
                          line = list(color = '#FFFFFF', width = 1)))  %>% 
      layout(title = 'United States Personal Expenditures by Categories in 1960',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(font = list(size = 20, itemwidth = 30)))
  })
  
  
  
  ## Rendering the filter columns
  filter_data <- reactive({
    output_data <- c("Cluster", input$filter_var_1, input$filter_var_2, input$filter_var_3)
  })
  
  
  
  ### Rendering the object
  output$summary_table <- renderTable({

    wine_data %>% mutate(Cluster = paste0("Cluster ", as.integer(cluster))) %>% 
      group_by(Cluster) %>% select(filter_data()) %>% summarise_all(mean)
    
  })
  
  
  
}


shinyApp(ui, server)
