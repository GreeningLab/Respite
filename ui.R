library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Respite"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("files", label = h3("File input")),
            splitLayout(
              actionButton("logger_left", "←", width = "100%"),
              numericInput(
                "logger",
                NULL,
                min = 1,
                max = 100,
                value = 1
              ),
              actionButton("logger_right", "→", width = "100%"),
              cellWidths = c('25%', '50%', '25%')
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
