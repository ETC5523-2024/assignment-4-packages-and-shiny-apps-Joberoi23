library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(IndianLiteracyRates)
library(shinythemes)


# Defining the UI
ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Indian Literacy Rate Analysis"),

  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State or Union Territory:",
                  choices = unique(GOI_data$country_states_union_territories_name)),
      actionButton("analyze", "Analyze")
    ),

    mainPanel(
      plotOutput("literacy_plot"),    # Total literacy rate plot
      htmlOutput("summary"),          # Summary for total literacy rate

      plotOutput("urban_plot"),       # Urban literacy rate plot
      htmlOutput("urban_summary"),    # Summary for urban literacy rate

      plotOutput("rural_plot"),       # Rural literacy rate plot
      htmlOutput("rural_summary")     # Summary for rural literacy rate
    )
  )
)

# Defining the server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$state)
    GOI_data %>%
      filter(country_states_union_territories_name == input$state)
  })

  # Plot and summary for total literacy rates
  output$literacy_plot <- renderPlot({
    req(filtered_data())

    # Preparing data for plotting
    plot_data <- filtered_data() %>%
      select(literacy_rate_persons_total_2001, literacy_rate_persons_total_2011) %>%
      pivot_longer(
        cols = c("literacy_rate_persons_total_2001", "literacy_rate_persons_total_2011"),
        names_to = "year",
        names_prefix = "literacy_rate_persons_total_",
        values_to = "literacy_rate"
      ) %>%
      mutate(year = as.integer(year))

    # Plot
    ggplot(plot_data, aes(x = year, y = literacy_rate)) +
      geom_line(color = "darkblue") +
      geom_point() +
      labs(title = paste("Total Literacy Rate in", input$state),
           x = "Year", y = "Literacy Rate (%)") +
      theme_minimal()
  })

  output$summary <- renderUI({
    req(filtered_data())

    # Calculating the change in literacy rate between 2001 and 2011
    rate_2001 <- filtered_data()$literacy_rate_persons_total_2001
    rate_2011 <- filtered_data()$literacy_rate_persons_total_2011
    literacy_change <- ifelse(!is.na(rate_2001) & !is.na(rate_2011),
                              rate_2011 - rate_2001, NA)

    # Displaying the result
    if (is.na(literacy_change)) {
      HTML(paste0(
        "<strong>Summary:</strong><br/>",
        "The change in total literacy rate for ", input$state,
        " from 2001 to 2011 could not be calculated due to missing data."
      ))
    } else {
      HTML(paste0(
        "<strong>Summary:</strong><br/>",
        "The change in total literacy rate for ", input$state,
        " from 2001 to 2011 is ", round(literacy_change, 2), "%"
      ))
    }
  })

  # Plot and summary for urban literacy rates
  output$urban_plot <- renderPlot({
    req(filtered_data())

    # Preparing data for plotting urban literacy rate
    plot_data <- filtered_data() %>%
      select(literacy_rate_persons_urban_2001, literacy_rate_persons_urban_2011) %>%
      pivot_longer(
        cols = c("literacy_rate_persons_urban_2001", "literacy_rate_persons_urban_2011"),
        names_to = "year",
        names_prefix = "literacy_rate_persons_urban_",
        values_to = "literacy_rate"
      ) %>%
      mutate(year = as.integer(year))

    # Plot
    ggplot(plot_data, aes(x = year, y = literacy_rate)) +
      geom_line(color = "magenta") +
      geom_point() +
      labs(title = paste("Urban Literacy Rate in", input$state),
           x = "Year", y = "Urban Literacy Rate (%)") +
      theme_minimal()
  })

  output$urban_summary <- renderUI({
    req(filtered_data())

    # Calculating the change in urban literacy rate between 2001 and 2011
    rate_urban_2001 <- filtered_data()$literacy_rate_persons_urban_2001
    rate_urban_2011 <- filtered_data()$literacy_rate_persons_urban_2011
    literacy_change_urban <- ifelse(!is.na(rate_urban_2001) & !is.na(rate_urban_2011),
                                    rate_urban_2011 - rate_urban_2001, NA)

    # Display the result
    if (is.na(literacy_change_urban)) {
      HTML(paste0(
        "<strong>Urban Summary:</strong><br/>",
        "The change in urban literacy rate for ", input$state,
        " from 2001 to 2011 could not be calculated due to missing data."
      ))
    } else {
      HTML(paste0(
        "<strong>Urban Summary:</strong><br/>",
        "The change in urban literacy rate for ", input$state,
        " from 2001 to 2011 is ", round(literacy_change_urban, 2), "%"
      ))
    }
  })

  # Plot and summary for rural literacy rates
  output$rural_plot <- renderPlot({
    req(filtered_data())

    # Preparing data for plotting rural literacy rate
    plot_data <- filtered_data() %>%
      select(literacy_rate_persons_rural_2001, literacy_rate_persons_rural_2011) %>%
      pivot_longer(
        cols = c("literacy_rate_persons_rural_2001", "literacy_rate_persons_rural_2011"),
        names_to = "year",
        names_prefix = "literacy_rate_persons_rural_",
        values_to = "literacy_rate"
      ) %>%
      mutate(year = as.integer(year))

    # Plot
    ggplot(plot_data, aes(x = year, y = literacy_rate)) +
      geom_line(color = "royalblue") +
      geom_point() +
      labs(title = paste("Rural Literacy Rate in", input$state),
           x = "Year", y = "Rural Literacy Rate (%)") +
      theme_minimal()
  })

  output$rural_summary <- renderUI({
    req(filtered_data())

    # Calculating the change in rural literacy rate between 2001 and 2011
    rate_rural_2001 <- filtered_data()$literacy_rate_persons_rural_2001
    rate_rural_2011 <- filtered_data()$literacy_rate_persons_rural_2011
    literacy_change_rural <- ifelse(!is.na(rate_rural_2001) & !is.na(rate_rural_2011),
                                    rate_rural_2011 - rate_rural_2001, NA)

    # Displaying the result
    if (is.na(literacy_change_rural)) {
      HTML(paste0(
        "<strong>Rural Summary:</strong><br/>",
        "The change in rural literacy rate for ", input$state,
        " from 2001 to 2011 could not be calculated due to missing data."
      ))
    } else {
      HTML(paste0(
        "<strong>Rural Summary:</strong><br/>",
        "The change in rural literacy rate for ", input$state,
        " from 2001 to 2011 is ", round(literacy_change_rural, 2), "%"
      ))
    }
  })
}

# Running the application
shinyApp(ui = ui, server = server)


