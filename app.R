library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(here)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

data <- read_csv(here::here("data", "processed", "cleaned_salaries.csv"))


app$layout(
  dbcContainer(
    list(
      htmlDiv("Data Science Salaries", style=list(color = "gray", fontSize = 26)),
      htmlP(
        "Shiny Dashboard", id="my-para"
      ),
      htmlDiv(
        list(
          htmlDiv("Are you a Data Scientist?"),
          dccDropdown(
            id="DS_identity",
            options = list(
              list(label = "Yes", value = "Yes"),
              list(label = "No", value = "No"),
              list(label = "Sort of", value = 'Sort of (Explain more)')
            ),
            value = c('Yes', 'No', 'Sort of (Explain more)'),
            multi = TRUE
          ),
          dccGraph(id = "side_plot")
        )
      )
    )
  )
)


app$callback(
  output("side_plot", "figure"), 
  list(input("DS_identity", "value")),
  function(DS_identity) {
    # Clean data
    data <- data |> 
      drop_na() |> 
      dplyr::filter(Salary_USD < 400000) |>
      dplyr::filter(Tenure != "I don't write code to analyze data")
    
    # Change list to vector
    DS_identity <- unlist(!!sym(DS_identity))
    
    # Filter data
    if (is.null(DS_identity)){
      DS_identity = c('Yes', 'No', 'Sort of (Explain more)')
    }
    
    data <- data |>
      dplyr::filter(DataScienceIdentitySelect %in% DS_identity)
    
    # Plot order
    order_tenure <- c('More than 10 years', '6 to 10 years', '3 to 5 years', '1 to 2 years', 'Less than a year')
    
    # Create Plot
    points <- data |> ggplot(aes(
      x = Salary_USD,
      y = Country,
      color = Tenure
    )) + geom_point() +
      labs(
        title = "Interactive window for coding experience count",
        x = "Salary in USD",
        y = "Country"
      ) +
      theme(legend.position="none")
    
    bars <- data |> ggplot(aes(
      y = Tenure,
      fill = Tenure
    )) + geom_bar() +
      labs(
        x = "Counts",
        y = "Coding Experience"
      ) +
      theme(legend.position="none")
    
    subplot(ggplotly(points, tooltip = "EmployerIndustry"), 
                    bars, 
                    nrows = 2, 
                    heights = c(0.85, 0.15))
  }
)


# app$run_server(host = '0.0.0.0')
app$run_server(debug = TRUE)
