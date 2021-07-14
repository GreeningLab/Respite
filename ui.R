library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Respite"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("files", label = h3("File input")),
            numericInput(
              "logger",
              "Logger Number",
              min = 1,
              max = 100,
              value = 1
            ),
            sliderInput(
              "minseglen",
              "Minimum Segment Length:",
              min = 3,
              max = 100,
              value = 30
            ),
            verbatimTextOutput("description")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotly::plotlyOutput("segments"),
            plotOutput("fit")
        )
    )
))
