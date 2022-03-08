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

data <- read_csv(here("data", "processed", "cleaned_salaries.csv"))

education_order <- c(
  "Bachelor's degree",
  "Master's degree",
  "Doctoral degree"
)

tenure_order <- c(
  "Less than a year",
  "1 to 2 years",
  "3 to 5 years",
  "6 to 10 years",
  "More than 10 years")

app$layout(
  dbcContainer(
    list(
      dccGraph(id='stacked-hist'),
      htmlDiv("Select a feature to stack by:"),
      dccDropdown(
        id="stack-select",
        options = list(list(label = "Formal Education",
                            value = "FormalEducation"),
                       list(label = "Coding Experience", value = "Tenure")),
        value="FormalEducation")
    )
  )
)

app$callback(
  output('stacked-hist', 'figure'),
  list(input('stack-select', 'value')),
  function(stack) {

    p <- data %>%
      drop_na(Salary_USD, Tenure, FormalEducation) %>%
      filter(Tenure != "I don't write code to analyze data") %>%
      mutate(
        FormalEducation = case_when(
          !(FormalEducation %in% education_order) ~ "Less than bachelor's degree",
          TRUE ~ FormalEducation)) %>%
      mutate(
        FormalEducation = factor(
          FormalEducation, levels = c(
          "Less than bachelor's degree", education_order)
          ),
        Tenure = factor(Tenure, levels = tenure_order)
        ) %>%
      ggplot(aes(x = Salary_USD, fill = !!sym(stack))) +
      geom_histogram(bins = 20, color = "white") +
      scale_x_continuous(labels = scales::label_number_si()) +
      labs(x = "Salary in USD", y = "Counts") +
      theme_bw()

    if (stack == "Tenure") {
      p <- p +
        labs(fill = "Coding experience")
    }
    else {
      p <- p +
        labs(fill = "Formal education level")
    }

    ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')
# app$run_server(debug = T)